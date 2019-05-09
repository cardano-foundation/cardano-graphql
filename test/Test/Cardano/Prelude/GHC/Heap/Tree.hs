{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Prelude.GHC.Heap.Tree
  ( tests
  )
where

import Cardano.Prelude

import Control.Applicative ((<$>))
import Data.Text (unpack)
import Hedgehog
  ( Gen
  , Property
  , checkParallel
  , discover
  , failure
  , forAll
  , property
  , withTests
  , annotate
  , (===)
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Property: The depth of a `[Word8]`'s closure tree is its length + 1.
-- n.b. The (+ 1) is for the `[]` constructor at the end.
prop_Word8ListClosureTreeDepth :: Property
prop_Word8ListClosureTreeDepth =
  withTests 500
    $ property
    $ do
        let
          genElem :: Gen Word8
          genElem = Gen.word8 Range.constantBounded
        listLen <- forAll $ Gen.int (Range.constant 0 500)
        xs      <- forAll $ Gen.list (Range.singleton listLen) genElem
        let
          maxTreeDepth = AnyDepth -- This should be okay since, in this property,
                                  -- the maximum depth of a generated tree can
                                  -- only be `listLen + 1`.
          travCycClos  = NoTraverseCyclicClosures
          opts         = ClosureTreeOptions maxTreeDepth travCycClos
        mbClosureTree <- liftIO $ buildClosureTree opts $!! xs
        case mbClosureTree of
          Nothing -> do
            annotate "prop_Word8ListClosureTreeDepth: Impossible"
            failure
          Just ct -> do
            annotate $ unpack (renderTree ct renderClosure)
            depth ct === (length xs) + 1

-- | Property: Specifying a 'TreeDepth' other than 'AnyDepth' should
-- appropriately limit the maximum depth of the 'Closure' 'Tree' generated.
-- i.e. If we specify a maximum depth of @TreeDepth maxDepth@, we would expect
-- that the depth of the 'Closure' 'Tree', @closureTree@, would either be
-- @maxDepth@ or, @if depth closureTree < maxDepth@, @depth closureTree@.
prop_ClosureTreeHasSpecifiedDepth :: Property
prop_ClosureTreeHasSpecifiedDepth = withTests 500 $ property $ do
  let
    genElem :: Gen Word8
    genElem = Gen.word8 Range.constantBounded
  listLen  <- forAll $ Gen.int (Range.constant 0 500)
  xs       <- forAll $ Gen.list (Range.singleton listLen) genElem
  maxDepth <- forAll $ Gen.int (Range.constant 0 1000)
  let
    travCycClos  = NoTraverseCyclicClosures
    maxTreeDepth = TreeDepth maxDepth
    opts         = ClosureTreeOptions maxTreeDepth travCycClos
  mbClosureTree <- liftIO $ buildClosureTree opts $!! xs
  case mbClosureTree of
    Nothing -> maxDepth === 0
    Just ct -> do
      annotate $ unpack (renderTree ct renderClosure)
      if depth ct < maxDepth
        then depth ct === (length xs) + 1
        else depth ct === maxDepth

tests :: IO Bool
tests = and <$> sequence [checkParallel $$(discover)]
