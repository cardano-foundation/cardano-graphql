{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}

module Test.Cardano.Prelude.GHC.Heap.NormalForm.Classy (tests) where

-- We avoid bang patterns as well as the use of '($!)', to make sure that these
-- tests pass with @-O0@. See 'isNormalForm' for discussion.
import Cardano.Prelude hiding (($!))

import GHC.Types (IO (..), Int (..))
import Prelude (String)
import System.Random (randomRIO)
import qualified Data.Text as Text
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Internal as SI

import Hedgehog
import Hedgehog.Internal.Config (UseColor (..))
import Hedgehog.Internal.Region (displayRegion)
import Hedgehog.Internal.Report (Result (..), reportStatus)
import Hedgehog.Internal.Runner (checkNamed)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

-- | The model for a value describes that value, being explicit where we
-- can expect thunks in the value.
class (NoUnexpectedThunks a, Show (Model a)) => FromModel a where
  data Model a :: Type

  -- | Generate model value (see below for examples)
  genModel  :: Gen (Model a)

  -- | Does the model describe a value in NF?
  modelIsNF :: [String] -> Model a -> IsNormalForm [String]

  -- | Context as it should be returned by 'noUnexpectedThunks'
  --
  -- This has a default implementation in terms of 'modelIsNF': there are
  -- unexpected thunks iff the model is not fully in NF.
  modelUnexpected :: [String] -> Model a -> Maybe [String]
  modelUnexpected ctxt m =
    case modelIsNF ctxt m of
      IsNF      -> Nothing
      IsWHNF  c -> Just c
      NotWHNF c -> Just c

  -- | Translate from the model to an actual value
  --
  -- The @a@ thunk should contain no unevaluated calls to 'fromModel'.
  fromModel :: forall r. Model a -> (a -> r) -> r

-- | Is a value in normal form?
data IsNormalForm a =
    IsNF      -- ^ Value completely in normal form
  | IsWHNF  a -- ^ Value is in WHNF, but not NF. Record information about thunk.
  | NotWHNF a -- ^ Value is not in WHNF. Record information about thunk.
  deriving (Show, Functor)

-- | 'IsNormalForm' for a constructor applied to arguments
--
-- A constructor applied to arguments is always in WHNF; it is in NF iff all
-- arguments are.
constrNF :: forall a. [IsNormalForm a] -> IsNormalForm a
constrNF args =
    case firstNotNF args of
      Nothing -> IsNF
      Just a  -> IsWHNF a
  where
    firstNotNF :: [IsNormalForm a] -> Maybe a
    firstNotNF []                  = Nothing
    firstNotNF (NotWHNF a : _    ) = Just a
    firstNotNF (IsWHNF  a : _    ) = Just a
    firstNotNF (IsNF      : args') = firstNotNF args'

testWithModel :: forall a. FromModel a
              => (ThunkInfo -> Maybe [String] -> Bool)
              -> Proxy a
              -- ^ Compare @ThunkInfo@. When we use 'noUnexpectedThunks' this
              -- can just be @(==)@; however, when we use 'isNormalForm', the
              -- context we will get from the model will be too detailed.
              -> Property
testWithModel compareInfo _proxy = withTests 1000 $ property $ do
    m :: Model a <- forAll genModel
    collect $ modelUnexpected [] m
    fromModel m $ \a -> do
      annotate $ show $ modelIsNF [] m
      tree <- liftIO $ buildAndRenderClosureTree treeOpts a
      isNF <- liftIO $ noUnexpectedThunks [] a
      annotate (Text.unpack tree)
      Hedgehog.diff isNF compareInfo (modelUnexpected [] m)

{-------------------------------------------------------------------------------
  Int
-------------------------------------------------------------------------------}

instance FromModel Int where
  data Model Int =
      IntThunk (Model Int)
    | IntValue Int
    deriving (Show)

  -- for integers there is no difference between NF/WHNF
  modelIsNF ctxt = \case
      IntThunk _ -> NotWHNF ctxt'
      IntValue _ -> IsNF
    where
      ctxt' = "Int" : ctxt

  fromModel (IntThunk i) k = fromModel i $ \i' -> k (if ack 3 3 > 0 then i' else i')
  fromModel (IntValue n) k = case n of I# result -> k (I# result)

  genModel = Gen.choice [
        IntValue <$> Gen.int Range.linearBounded
      , IntThunk <$> genModel
      ]

{-------------------------------------------------------------------------------
  Pairs
-------------------------------------------------------------------------------}

instance (FromModel a, FromModel b) => FromModel (a, b) where
  data Model (a, b) =
      PairThunk (Model (a, b))
    | PairDefined (Model a) (Model b)

  modelIsNF ctxt = \case
      PairThunk _     -> NotWHNF ctxt'
      PairDefined a b -> constrNF [modelIsNF ctxt' a, modelIsNF ctxt' b]
    where
      ctxt' = "(,)" : ctxt

  fromModel (PairThunk p)     k = fromModel p $ \p' -> k (if ack 3 3 > 0 then p' else p')
  fromModel (PairDefined a b) k = fromModel a $ \a' ->
                                  fromModel b $ \b' ->
                                  k (a', b')

  genModel = Gen.choice [
        PairDefined <$> genModel <*> genModel
      , PairThunk <$> genModel
      ]

deriving instance (Show (Model a), Show (Model b)) => Show (Model (a, b))

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

instance FromModel a => FromModel [a] where
  data Model [a] =
      ListThunk (Model [a])
    | ListNil
    | ListCons (Model a) (Model [a])

  modelIsNF ctxt = \case
      ListThunk _    -> NotWHNF ctxt'
      ListNil        -> IsNF
      ListCons x xs' -> constrNF [modelIsNF ctxt' x, modelIsNF ctxt xs']
    where
      ctxt' = "[]" : ctxt

  fromModel (ListThunk xs)  k = fromModel xs $ \xs' -> k (if ack 3 3 > 0 then xs' else xs')
  fromModel ListNil         k = k []
  fromModel (ListCons x xs) k = fromModel x  $ \x'  ->
                                fromModel xs $ \xs' ->
                                k (x' : xs')

  genModel = do
      sz <- Gen.int $ Range.linear 0 10
      go sz
    where
      go :: Int -> Gen (Model [a])
      go 0 = pure ListNil
      go n = Gen.choice [
                 ListCons <$> genModel <*> go (n - 1)
               , ListThunk <$> go (n - 1)
               ]

deriving instance Show (Model a) => Show (Model [a])

{-------------------------------------------------------------------------------
  Seq
-------------------------------------------------------------------------------}

instance FromModel (Seq Int) where
  data Model (Seq Int) = SeqEmpty | SeqEnqueue (Model Int) (Model (Seq Int))
    deriving (Show)

  modelIsNF ctxt = \case
      SeqEmpty        -> IsNF
      SeqEnqueue x xs -> constrNF [modelIsNF ctxt' x, modelIsNF ctxt xs]
    where
      ctxt' = "Seq" : ctxt

  fromModel m = \k -> go m $ \s -> forceSeqToWhnf s k
    where
      go :: Model (Seq Int) -> (Seq Int -> r) -> r
      go SeqEmpty          k = k Seq.empty
      go (SeqEnqueue x xs) k =
          fromModel x  $ \x'  ->
          go        xs $ \xs' ->
          k (x' Seq.<| xs')

  genModel = do
      sz <- Gen.int $ Range.linear 0 100
      -- It is important that we have a good probability of generating sequences
      -- that the model considers to be in normal form: for such sequences the
      -- model and the 'isNormalForm' check (but not the 'noUnexpectedThunks'
      -- check) can diverge, because the internal @FingerTree@ may not be
      -- fully evaluated.
      Gen.choice [
          go (pure $ IntValue 0) sz
        , go genModel sz
        ]
    where
      go :: Gen (Model Int) -> Int -> Gen (Model (Seq Int))
      go _      0 = return SeqEmpty
      go genInt n = SeqEnqueue <$> genInt <*> go genInt (n - 1)

forceSeqToWhnf :: Seq a -> (Seq a -> r) -> r
forceSeqToWhnf (SI.Seq SI.EmptyT)          k = k (SI.Seq SI.EmptyT)
forceSeqToWhnf (SI.Seq (SI.Single a))      k = k (SI.Seq (SI.Single a))
forceSeqToWhnf (SI.Seq (SI.Deep n l ft r)) k = k (SI.Seq (SI.Deep n l ft r))

{-------------------------------------------------------------------------------
  AllowThunksIn
-------------------------------------------------------------------------------}

data Record = Record
  { field1 :: [Int]
  , field2 :: Int
  } deriving (Generic, Show)

instance FromModel (AllowThunksIn '["field1"] Record) where
  data Model (AllowThunksIn '["field1"] Record)
    = RecordThunk   (Model (AllowThunksIn '["field1"] Record))
    | RecordDefined (Model [Int]) (Model Int)

  modelIsNF ctxt = \case
    RecordThunk _ -> NotWHNF ctxt'
    RecordDefined a b -> constrNF [modelIsNF ctxt' a, modelIsNF ctxt' b]
    where
      ctxt' = "Record" : ctxt

  modelUnexpected ctxt m =
      case m of
        RecordThunk   _   -> Just ctxt'
        RecordDefined _ y -> modelUnexpected ctxt' y
      where
        ctxt' = "Record" : ctxt

  fromModel (RecordThunk r) k = fromModel r $ \r' -> k (if ack 3 3 > 0 then r' else r')
  fromModel (RecordDefined a b) k =
    fromModel a $ \a' ->
    fromModel b $ \b' ->
    k (AllowThunksIn (Record a' b'))

  genModel = Gen.choice [
        RecordDefined <$> genModel <*> genModel
      , RecordThunk <$> genModel
      ]

deriving instance Show (Model (AllowThunksIn '["field1"] Record))


{-------------------------------------------------------------------------------
  Special case: function closures

  Since we don't traverse the function closure, we should only check if
  the function itself is in WHNF or not.

  We have to be careful here exactly how we phrase this test to avoid the GHC
  optimizer being too smart, turning what we think ought to be thunks into
  top-level CAFs.
-------------------------------------------------------------------------------}

-- | Ackermann (anything that ghc won't just optimize away..)
ack :: Int -> Int -> Int
ack 0 n = succ n
ack m 0 = ack (pred m) 1
ack m n = ack (pred m) (ack m (pred n))

-- | Function which is not strict in either 'Int' argument
{-# NOINLINE notStrict #-}
notStrict :: Bool -> Int -> Int -> Int
notStrict False x _ = x
notStrict True  _ y = y

definitelyInNF :: Int -> Int
definitelyInNF n = n

instance FromModel (Int -> Int) where
  data Model (Int -> Int) =
      FnInNF                           -- Function in NF
    | FnNotInNF Bool Int               -- Function in WHNF but not in NF
    | FnNotInWHNF (Model (Int -> Int)) -- Function not in WHNF
    | FnToWHNF (Model (Int -> Int))    -- Force function to WHNF
    deriving (Show)

  fromModel FnInNF          k = k definitelyInNF
  fromModel (FnNotInNF b n) k = k (\x -> notStrict b (ack 5 n) x) -- Lambda is in WHNF
  fromModel (FnNotInWHNF f) k = fromModel f $ \f' -> k (if ack 3 3 > 0 then f' else f')
  fromModel (FnToWHNF    f) k = fromModel f $ \f' -> f' `seq` k f'

  -- By default we don't distinguish between NF and WHNF for functions
  modelUnexpected ctxt m =
      case modelIsNF ctxt m of
        IsNF      -> Nothing
        IsWHNF  _ -> Nothing
        NotWHNF c -> Just c

  modelIsNF ctxt = \case
      FnInNF        -> IsNF
      FnNotInNF _ _ -> IsWHNF ctxt'
      FnNotInWHNF _ -> NotWHNF ctxt'
      FnToWHNF f    ->
        case f of
          -- Forcing a function already in NF leaves it in NF
          FnInNF         -> IsNF

          -- Forcing a function which is already in WHNF (but not in NF)
          -- leaves it in WHNF
          FnNotInNF _ _  -> IsWHNF ctxt'

          -- Forcing a computation reveals what's underneath it.
          -- We leave the 'FnToWHNF' constructor at the top because
          -- It doens't matter quite how many computations are underneath,
          -- a single force forces them all.
          FnNotInWHNF f' -> modelIsNF ctxt (FnToWHNF f')

          -- Forcing twice is the same as forcing once
          FnToWHNF f'    -> modelIsNF ctxt (FnToWHNF f')
    where
      ctxt' = ("->" : ctxt)

  genModel = Gen.choice [
        pure FnInNF
      , FnNotInNF   <$> Gen.bool <*> Gen.int Range.linearBounded
      , FnNotInWHNF <$> genModel
      , FnToWHNF    <$> genModel
      ]

{-------------------------------------------------------------------------------
  Special case: IO

  Similar kind of thing as for function closures. Here we have to be even more
  careful in our choice of examples to get something that works both with @-O0@
  and @-O1@.
-------------------------------------------------------------------------------}

-- IO action which is definitely in NF
doNothing :: IO ()
doNothing = IO (\w -> (# w, () #) )

instance FromModel (IO ()) where
  -- We reuse the model we use for functions, we do the same 4 types
  newtype Model (IO ()) = ModelIO (Model (Int -> Int))
    deriving Show

  fromModel (ModelIO m) = go m
    where
      go :: Model (Int -> Int) -> (IO () -> r) -> r
      go FnInNF          k = k doNothing
      go (FnNotInNF b n) k = k (IO (\w -> let x = notStrict b (ack 5 n) 6
                                          in x `seq` (# w, () #) ))
      go (FnNotInWHNF f) k = go f $ \f' -> k (if ack 3 3 > 0 then f' else f')
      go (FnToWHNF    f) k = go f $ \f' -> f' `seq` k f'

  modelUnexpected ctxt (ModelIO f) = fnToIOContext <$> modelUnexpected ctxt f
  modelIsNF       ctxt (ModelIO f) = fnToIOContext <$> modelIsNF       ctxt f
  genModel = ModelIO <$> genModel

fnToIOContext :: [String] -> [String]
fnToIOContext ("->" : ctxt)         = "IO" : ctxt
fnToIOContext ("..." : "->" : ctxt) = "..." : "IO" : ctxt
fnToIOContext ctxt                  = ctxt

{-------------------------------------------------------------------------------
  Check that we /can/ check functions and IO actions for nested thunks
-------------------------------------------------------------------------------}

newtype ThunkFree (name :: Symbol) a = ThunkFree a
  deriving NoUnexpectedThunks via UseIsNormalFormNamed name a

instance FromModel (ThunkFree "->" (Int -> Int)) where
  newtype Model (ThunkFree "->" (Int -> Int)) = ThunkFreeFn (Model (Int -> Int))
    deriving (Show)

  genModel = ThunkFreeFn <$> genModel
  fromModel (ThunkFreeFn f) k = fromModel f $ \f' -> k (ThunkFree f')
  modelIsNF ctxt (ThunkFreeFn f) = modelIsNF ctxt f

  modelUnexpected ctxt m =
      case modelIsNF ctxt m of
        IsNF      -> Nothing
        IsWHNF  _ -> Just ["...", "->"]
        NotWHNF _ -> Just ["->"]

instance FromModel (ThunkFree "IO" (IO ())) where
  newtype Model (ThunkFree "IO" (IO ())) = ThunkFreeIO (Model (Int -> Int))
    deriving (Show)

  genModel =
      ThunkFreeIO <$> genModel
  fromModel (ThunkFreeIO m) k =
      fromModel (ModelIO m) $ \f -> k (ThunkFree f)
  modelIsNF ctxt (ThunkFreeIO f) =
      fnToIOContext <$> modelIsNF ctxt (ThunkFreeFn f)
  modelUnexpected ctxt (ThunkFreeIO f) =
      fnToIOContext <$> modelUnexpected ctxt (ThunkFreeFn f)

{-------------------------------------------------------------------------------
  Using the standard 'isNormalForm' check
-------------------------------------------------------------------------------}

instance (FromModel a, Typeable a) => FromModel (UseIsNormalForm a) where
  newtype Model (UseIsNormalForm a) = Wrap { unwrap :: Model a }

  genModel       = Wrap <$> genModel
  modelUnexpected ctxt = modelUnexpected ctxt . unwrap
  modelIsNF ctxt = modelIsNF ctxt . unwrap
  fromModel m k  = fromModel (unwrap m) $ \x -> k (UseIsNormalForm x)

deriving instance Show (Model a) => Show (Model (UseIsNormalForm a))

{-------------------------------------------------------------------------------
  Some sanity checks

  These are primarily designed to check that we can distinguish between
  functions with nested thunks and functions without.
-------------------------------------------------------------------------------}

{-# NOINLINE checkNF #-}
checkNF :: Bool -> IO a -> Property
checkNF expectedNF mkX = withTests 1 $ property $ do
    x    <- liftIO $ mkX
    tree <- liftIO $ buildAndRenderClosureTree treeOpts x
    nf   <- liftIO $ isNormalForm x
    annotate (Text.unpack tree)
    nf === expectedNF

treeOpts :: ClosureTreeOptions
treeOpts = ClosureTreeOptions AnyDepth TraverseCyclicClosures

{-# NOINLINE sanityCheckInt #-}
sanityCheckInt :: Property
sanityCheckInt = checkNF False $ return (if ack 3 3 > 0 then x else x)
  where
    x :: Int
    x = 0

{-# NOINLINE sanityCheckPair #-}
sanityCheckPair :: Property
sanityCheckPair = checkNF False $ return (if ack 3 3 > 0 then x else x)
  where
    x :: (Int, Bool)
    x = (0, True)

{-# NOINLINE sanityCheckFn #-}
sanityCheckFn :: Property
sanityCheckFn = checkNF False $ do
    b <- randomRIO (False, True)
    n <- ack 5 <$> randomRIO (0, 10)
    return $ (notStrict b n :: Int -> Int)

{-# NOINLINE sanityCheckIO #-}
sanityCheckIO :: Property
sanityCheckIO = checkNF False $ do
    b <- randomRIO (False, True)
    n <- ack 5 <$> randomRIO (0, 10)
    return $ (print (notStrict b n 6) :: IO ())

{-------------------------------------------------------------------------------
  Driver
-------------------------------------------------------------------------------}

tests :: IO Bool
tests = and <$> sequence [checkSequential testGroup]

testGroup :: Group
testGroup = Group "Test.Cardano.Prelude.GHC.Heap.NormalForm.Classy" [
      ("prop_sanityCheck_Int"  , sanityCheckInt)
    , ("prop_sanityCheck_Pair" , sanityCheckPair)
    , ("prop_sanityCheck_Fn"   , sanityCheckFn)
    , ("prop_sanityCheck_IO"   , sanityCheckIO)

    , ("prop_isNormalForm_Int"        ,                 testWithModel agreeOnNF $ Proxy @(UseIsNormalForm Int))
    , ("prop_isNormalForm_IntInt"     ,                 testWithModel agreeOnNF $ Proxy @(UseIsNormalForm (Int, Int)))
    , ("prop_isNormalForm_ListInt"    ,                 testWithModel agreeOnNF $ Proxy @(UseIsNormalForm [Int]))
    , ("prop_isNormalForm_IntListInt" ,                 testWithModel agreeOnNF $ Proxy @(UseIsNormalForm (Int, [Int])))
    , ("prop_isNormalForm_SeqInt"     , expectFailure $ testWithModel agreeOnNF $ Proxy @(UseIsNormalForm (Seq Int)))

    -- TODO: add list of pairs tests

    , ("prop_noUnexpectedThunks_Int"        , testWithModel agreeOnContext $ Proxy @Int)
    , ("prop_noUnexpectedThunks_IntInt"     , testWithModel agreeOnContext $ Proxy @(Int, Int))
    , ("prop_noUnexpectedThunks_ListInt"    , testWithModel agreeOnContext $ Proxy @[Int])
    , ("prop_noUnexpectedThunks_IntListInt" , testWithModel agreeOnContext $ Proxy @(Int, [Int]))
    , ("prop_noUnexpectedThunks_SeqInt"     , testWithModel agreeOnContext $ Proxy @(Seq Int))

    , ("prop_noUnexpectedThunks_AllowThunksIn"
      , testWithModel agreeOnContext $ Proxy @(AllowThunksIn '["field1"] Record)
      )

    , ("prop_noUnexpectedThunks_Fn" , testWithModel agreeOnContext $ Proxy @(Int -> Int))
    , ("prop_noUnexpectedThunks_IO" , testWithModel agreeOnContext $ Proxy @(IO ()))

    , ("prop_noUnexpectedThunks_ThunkFreeFn" , testWithModel agreeOnContext $ Proxy @(ThunkFree "->" (Int -> Int)))
    , ("prop_noUnexpectedThunks_ThunkFreeIO" , testWithModel agreeOnContext $ Proxy @(ThunkFree "IO" (IO ())))
    ]

-- | When using @UseIsNormalForm@ we don't get a context, so merely check if
-- both the model and the implementation agree whether or not the value is
-- in NF
agreeOnNF :: ThunkInfo -> Maybe [String] -> Bool
NoUnexpectedThunks `agreeOnNF` Nothing = True
UnexpectedThunk _  `agreeOnNF` Just _  = True
_                  `agreeOnNF` _       = False

-- | Check whether the model and the implementation agree on whether the value
-- is in NF, and if not, what the context of the thunk is.
agreeOnContext :: ThunkInfo -> Maybe [String] -> Bool
NoUnexpectedThunks   `agreeOnContext` Nothing   = True
UnexpectedThunk info `agreeOnContext` Just ctxt = unexpectedThunkContext info == ctxt
_                    `agreeOnContext` _         = False

{-------------------------------------------------------------------------------
  Hedgehog auxiliary
-------------------------------------------------------------------------------}

expectFailure :: Property -> Property
expectFailure p = withTests 1 $ property $ do
    report <- liftIO $ displayRegion $ \r -> checkNamed r EnableColor (Just "EXPECTED FAILURE") p
    case reportStatus report of
      Failed _ ->
        success
      _otherwise -> do
        footnote "The test passed, but we expected it to fail."
        failure
