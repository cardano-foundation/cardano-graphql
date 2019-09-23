{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Cardano.Prelude.GHC.Heap.NormalForm.Classy (tests) where

-- We avoid bang patterns as well as the use of '($!)', to make sure that these
-- tests pass with @-O0@. See 'isNormalForm' for discussion.
import Cardano.Prelude hiding (($!))

import Prelude (String)
import GHC.Types (Int(..))
import Control.Exception (throw)

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
  modelIsNF :: [String] -> Model a -> Maybe [String]

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
    collect $ modelIsNF [] m
    fromModel m $ \a -> do
      isNF <- liftIO $ noUnexpectedThunks [] a
      Hedgehog.diff isNF compareInfo (modelIsNF [] m)

{-------------------------------------------------------------------------------
  Int
-------------------------------------------------------------------------------}

instance FromModel Int where
  data Model Int =
      IntUndefined
    | IntValue Int
    deriving (Show)

  modelIsNF ctxt IntUndefined = Just ("Int" : ctxt)
  modelIsNF _    (IntValue _) = Nothing

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

  modelIsNF ctxt ab =
      case ab of
        PairUndefined   -> Just ctxt'
        PairDefined a b -> modelIsNF ctxt' a <|> modelIsNF ctxt' b
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

  modelIsNF ctxt xs =
      case xs of
        ListUndefined  -> Just ctxt'
        ListNil        -> Nothing
        ListCons x xs' -> modelIsNF ctxt' x <|> modelIsNF ctxt xs'

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

  modelIsNF ctxt = go
    where
      go SeqEmpty          = Nothing
      go (SeqEnqueue x xs) = modelIsNF ctxt' x <|> go xs

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
  Using the standard 'isNormalForm' check
-------------------------------------------------------------------------------}

instance (FromModel a, Typeable a) => FromModel (UseIsNormalForm a) where
  newtype Model (UseIsNormalForm a) = Wrap { unwrap :: Model a }

  genModel       = Wrap <$> genModel
  modelIsNF ctxt = modelIsNF ctxt . unwrap
  fromModel m k  = fromModel (unwrap m) $ \x -> k (UseIsNormalForm x)

deriving instance Show (Model a) => Show (Model (UseIsNormalForm a))

{-------------------------------------------------------------------------------
  Driver
-------------------------------------------------------------------------------}

tests :: IO Bool
tests = and <$> sequence [checkSequential testGroup]

testGroup :: Group
testGroup = Group "Test.Cardano.Prelude.GHC.Heap.NormalForm.Classy" [
      ("prop_isNormalForm_Int"        ,                 testWithModel agreeOnNF $ Proxy @(UseIsNormalForm Int))
    , ("prop_isNormalForm_IntInt"     ,                 testWithModel agreeOnNF $ Proxy @(UseIsNormalForm (Int, Int)))
    , ("prop_isNormalForm_ListInt"    ,                 testWithModel agreeOnNF $ Proxy @(UseIsNormalForm [Int]))
    , ("prop_isNormalForm_IntListInt" ,                 testWithModel agreeOnNF $ Proxy @(UseIsNormalForm (Int, [Int])))
    , ("prop_isNormalForm_SeqInt"     , expectFailure $ testWithModel agreeOnNF $ Proxy @(UseIsNormalForm (Seq Int)))

    , ("prop_noUnexpectedThunks_Int"        , testWithModel agreeOnContext $ Proxy @Int)
    , ("prop_noUnexpectedThunks_IntInt"     , testWithModel agreeOnContext $ Proxy @(Int, Int))
    , ("prop_noUnexpectedThunks_ListInt"    , testWithModel agreeOnContext $ Proxy @[Int])
    , ("prop_noUnexpectedThunks_IntListInt" , testWithModel agreeOnContext $ Proxy @(Int, [Int]))
    , ("prop_noUnexpectedThunks_SeqInt"     , testWithModel agreeOnContext $ Proxy @(Seq Int))
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
