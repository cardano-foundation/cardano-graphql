{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
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

import Control.Exception (throw)
import GHC.Types (Int(..), IO(..))
import Prelude (String)
import System.Random (randomRIO)
import qualified Data.Text as Text

import Data.Sequence (Seq)
import qualified Data.Sequence          as Seq
import qualified Data.Sequence.Internal as SI

import Hedgehog
import Hedgehog.Internal.Region
import Hedgehog.Internal.Runner
import Hedgehog.Internal.Report
import qualified Hedgehog.Gen   as Gen
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
  --
  -- If not, return the (type) context of the thunk.
  modelThunkInfo :: [String] -> Model a -> Maybe [String]

  -- | Translate from the model to an actual value
  --
  -- The @a@ thunk should contain no unevaluated calls to 'fromModel'.
  fromModel :: forall r. Model a -> (a -> r) -> r

bottom :: Text -> a
bottom = throw . Bottom

data Bottom = Bottom Text
  deriving (Show)

instance Exception Bottom

testWithModel :: forall a. FromModel a
              => (ThunkInfo -> Maybe [String] -> Bool)
              -> Proxy a
              -- ^ Compare @ThunkInfo@. When we use 'noUnexpectedThunks' this
              -- can just be @(==)@; however, when we use 'isNormalForm', the
              -- context we will get from the model will be too detailed.
              -> Property
testWithModel compareInfo _proxy = withTests 1000 $ property $ do
    m :: Model a <- forAll genModel
    collect $ modelThunkInfo [] m
    fromModel m $ \a -> do
      tree <- liftIO $ buildAndRenderClosureTree treeOpts a
      isNF <- liftIO $ noUnexpectedThunks [] a
      annotate (Text.unpack tree)
      Hedgehog.diff isNF compareInfo (modelThunkInfo [] m)

{-------------------------------------------------------------------------------
  Int
-------------------------------------------------------------------------------}

instance FromModel Int where
  data Model Int =
      IntUndefined
    | IntValue Int
    deriving (Show)

  modelThunkInfo ctxt IntUndefined = Just ("Int" : ctxt)
  modelThunkInfo _    (IntValue _) = Nothing

  fromModel IntUndefined k = k (bottom "int thunk")
  fromModel (IntValue n) k = case n of I# result -> k (I# result)

  genModel = Gen.choice [
        pure $ IntUndefined
      , IntValue <$> Gen.int Range.linearBounded
      ]

{-------------------------------------------------------------------------------
  Pairs
-------------------------------------------------------------------------------}

instance (FromModel a, FromModel b) => FromModel (a, b) where
  data Model (a, b) =
      PairUndefined
    | PairDefined (Model a) (Model b)

  modelThunkInfo ctxt ab =
      case ab of
        PairUndefined   -> Just ctxt'
        PairDefined a b -> modelThunkInfo ctxt' a <|> modelThunkInfo ctxt' b
    where
      ctxt' = "(,)" : ctxt

  fromModel PairUndefined     k = k (bottom "pair thunk")
  fromModel (PairDefined a b) k = fromModel a $ \a' ->
                                  fromModel b $ \b' ->
                                  k (a', b')

  genModel = Gen.choice [
        pure $ PairUndefined
      , PairDefined <$> genModel <*> genModel
      ]

deriving instance (Show (Model a), Show (Model b)) => Show (Model (a, b))

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

instance FromModel a => FromModel [a] where
  data Model [a] =
      ListUndefined
    | ListNil
    | ListCons (Model a) (Model [a])

  modelThunkInfo ctxt xs =
      case xs of
        ListUndefined  -> Just ctxt'
        ListNil        -> Nothing
        ListCons x xs' -> modelThunkInfo ctxt' x <|> modelThunkInfo ctxt xs'

    where
      ctxt' = "[]" : ctxt

  fromModel ListUndefined   k = k (bottom "list thunk")
  fromModel ListNil         k = k []
  fromModel (ListCons x xs) k = fromModel x  $ \x'  ->
                                fromModel xs $ \xs' ->
                                k (x' : xs')

  genModel = do
      sz <- Gen.int $ Range.linear 0 10
      go sz
    where
      go :: Int -> Gen (Model [a])
      go 0 = Gen.choice [
                 pure $ ListUndefined
               , pure $ ListNil
               ]
      go n = ListCons <$> genModel <*> go (n - 1)

deriving instance Show (Model a) => Show (Model [a])

{-------------------------------------------------------------------------------
  Seq
-------------------------------------------------------------------------------}

instance FromModel (Seq Int) where
  data Model (Seq Int) = SeqEmpty | SeqEnqueue (Model Int) (Model (Seq Int))
    deriving (Show)

  modelThunkInfo ctxt = go
    where
      go SeqEmpty          = Nothing
      go (SeqEnqueue x xs) = modelThunkInfo ctxt' x <|> go xs

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
          go genModel sz
        , go (pure $ IntValue 0) sz
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
  fromModel (FnNotInNF b n) k = k (notStrict b (ack 5 n))
  fromModel (FnNotInWHNF f) k = fromModel f $ \f' -> k (if ack 3 3 > 0 then f' else f')
  fromModel (FnToWHNF    f) k = fromModel f $ \f' -> f' `seq` k f'

  modelThunkInfo _    FnInNF          = Nothing
  modelThunkInfo _    (FnNotInNF _ _) = Nothing
  modelThunkInfo ctxt (FnNotInWHNF _) = Just ("->" : ctxt)
  modelThunkInfo _    (FnToWHNF _)    = Nothing

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

  modelThunkInfo ctxt (ModelIO f) =
     case modelThunkInfo ctxt f of
       Just ("->" : ctxt') -> Just ("IO" : ctxt')
       info                -> info

  genModel = ModelIO <$> genModel

{-------------------------------------------------------------------------------
  Check that we /can/ check functions and IO actions for nested thunks
-------------------------------------------------------------------------------}

newtype ThunkFree a = ThunkFree a
  deriving NoUnexpectedThunks via UseIsNormalForm a

instance FromModel (ThunkFree (Int -> Int)) where
  newtype Model (ThunkFree (Int -> Int)) = ThunkFreeFn (Model (Int -> Int))
    deriving (Show)

  genModel = ThunkFreeFn <$> genModel
  fromModel (ThunkFreeFn f) k = fromModel f $ \f' -> k (ThunkFree f')

  modelThunkInfo ctxt (ThunkFreeFn m)
    | isInNF m  = Nothing
    | otherwise = Just ("Int -> Int" : ctxt)
    where
      isInNF :: Model (Int -> Int) -> Bool
      isInNF FnInNF          = True
      isInNF (FnNotInNF _ _) = False
      isInNF (FnNotInWHNF _) = False
      isInNF (FnToWHNF f)    =
         -- Computing the effect of forcing to WHNF is a bit tricky:
         case f of
           -- If the function is in NF, forcing it leaves it in NF
           FnInNF -> True

           -- If the function is already in WHNF, but not in NF, then
           -- forcing it will have no effect
           FnNotInNF _ _ -> False

           -- Forcing twice is the same as forcing one
           FnToWHNF f' -> isInNF (FnToWHNF f')

           -- Forcing a computation reveals what's underneath. It doesn't matter
           -- however /how many/ computations are underneath
           --
           -- > force (compute (compute (compute x))) = x
           --
           -- so we leave the FnToWHNF at the top.
           FnNotInWHNF f' -> isInNF (FnToWHNF f')

-- Just for completeness sake, we do the same for IO
instance FromModel (ThunkFree (IO ())) where
  newtype Model (ThunkFree (IO ())) = ThunkFreeIO (Model (Int -> Int))
    deriving (Show)

  genModel = ThunkFreeIO <$> genModel
  fromModel (ThunkFreeIO m) k = fromModel (ModelIO m) $ \f -> k (ThunkFree f)

  modelThunkInfo ctxt (ThunkFreeIO f) =
     case modelThunkInfo ctxt (ThunkFreeFn f) of
       Just ("Int -> Int" : ctxt') -> Just ("IO ()" : ctxt')
       info                        -> info

{-------------------------------------------------------------------------------
  Using the standard 'isNormalForm' check
-------------------------------------------------------------------------------}

instance (FromModel a, Typeable a) => FromModel (UseIsNormalForm a) where
  newtype Model (UseIsNormalForm a) = Wrap { unwrap :: Model a }

  genModel       = Wrap <$> genModel
  modelThunkInfo ctxt = modelThunkInfo ctxt . unwrap
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
sanityCheckInt = checkNF False $ return (bottom "thunk" :: Int)

{-# NOINLINE sanityCheckPair #-}
sanityCheckPair :: Property
sanityCheckPair = checkNF False $ return ((bottom "thunk", True) :: (Int, Bool))

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

    , ("prop_noUnexpectedThunks_Int"        , testWithModel agreeOnContext $ Proxy @Int)
    , ("prop_noUnexpectedThunks_IntInt"     , testWithModel agreeOnContext $ Proxy @(Int, Int))
    , ("prop_noUnexpectedThunks_ListInt"    , testWithModel agreeOnContext $ Proxy @[Int])
    , ("prop_noUnexpectedThunks_IntListInt" , testWithModel agreeOnContext $ Proxy @(Int, [Int]))
    , ("prop_noUnexpectedThunks_SeqInt"     , testWithModel agreeOnContext $ Proxy @(Seq Int))

    , ("prop_noUnexpectedThunks_Fn" , testWithModel agreeOnContext $ Proxy @(Int -> Int))
    , ("prop_noUnexpectedThunks_IO" , testWithModel agreeOnContext $ Proxy @(IO ()))

    , ("prop_noUnexpectedThunks_ThunkFreeFn" , testWithModel agreeOnContext $ Proxy @(ThunkFree (Int -> Int)))
    , ("prop_noUnexpectedThunks_ThunkFreeIO" , testWithModel agreeOnContext $ Proxy @(ThunkFree (IO ())))
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
    report <- liftIO $ displayRegion $ \r -> checkNamed r Nothing (Just "EXPECTED FAILURE") p
    case reportStatus report of
      Failed _ ->
        success
      _otherwise -> do
        footnote "The test passed, but we expected it to fail."
        failure
