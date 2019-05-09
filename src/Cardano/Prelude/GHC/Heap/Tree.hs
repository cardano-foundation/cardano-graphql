{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      :  Cardano.Prelude.GHC.Heap.Tree

This module exports functions and data types for building and printing GHC
heap object trees. Very useful for debugging issues involving the heap
representation of Haskell expressions.

-}

module Cardano.Prelude.GHC.Heap.Tree
  ( ClosureTreeOptions(..)
  , TraverseCyclicClosures(..)
  , TreeDepth(..)
  , buildClosureTree
  , buildAndRenderClosureTree
  , depth
  , isZeroOrNegativeTreeDepth
  , renderClosure
  , renderTree
  )
where

import Cardano.Prelude.Base

import Data.Text (pack, unpack)
import Data.Tree (Tree(..), drawTree, levels)
import GHC.Exts.Heap
  (Box(..), Closure, allClosures, areBoxesEqual, asBox, getClosureData)
import System.Mem (performGC)

-- | The depth of a 'Tree'.
data TreeDepth
  = TreeDepth {-# UNPACK #-} !Int
  -- ^ A specific tree depth bound.
  | AnyDepth
  -- ^ No tree depth bound.
  deriving (Show)

-- | Whether to traverse cyclic closures in a 'Closure' 'Tree'.
data TraverseCyclicClosures
  = TraverseCyclicClosures
  -- ^ Traverse cyclic closures.
  | NoTraverseCyclicClosures
  -- ^ Do not traverse cyclic closures.
  deriving (Show)

-- | Options which detail how a 'Closure' 'Tree' should be constructed.
data ClosureTreeOptions = ClosureTreeOptions
  { ctoMaxDepth       :: !TreeDepth
  -- ^ Construct a closure tree given a maximum depth.
  , ctoCyclicClosures :: !TraverseCyclicClosures
  -- ^ Whether to traverse cyclic closures while constructing a closure tree.
  } deriving (Show)

depth :: Tree a -> Int
depth = length . levels

isZeroOrNegativeTreeDepth :: TreeDepth -> Bool
isZeroOrNegativeTreeDepth AnyDepth = False
isZeroOrNegativeTreeDepth (TreeDepth d)
  | d <= 0    = True
  | otherwise = False

renderClosure :: Closure -> Text
renderClosure = show

renderTree :: Tree a -> (a -> Text) -> Text
renderTree tree renderA = pack $ drawTree (fmap (unpack . renderA) tree)

-- | Given a Haskell expression, build a 'Tree' which reflects its heap object
-- representation.
buildClosureTree :: ClosureTreeOptions -> a -> IO (Maybe (Tree Closure))
buildClosureTree opts x = do
  performGC
  go opts [] $ asBox x
 where
  go :: ClosureTreeOptions -> [Box] -> Box -> IO (Maybe (Tree Closure))
  go (ClosureTreeOptions { ctoMaxDepth, ctoCyclicClosures }) !vs b@(Box y)
    | isZeroOrNegativeTreeDepth ctoMaxDepth = pure Nothing
    | otherwise = do
      let
        nextMaxDepth = case ctoMaxDepth of
          AnyDepth    -> AnyDepth
          TreeDepth d -> TreeDepth (d - 1)
        nextOpts = ClosureTreeOptions nextMaxDepth ctoCyclicClosures
      case ctoCyclicClosures of
        NoTraverseCyclicClosures -> do
          isElem <- liftM or $ mapM (areBoxesEqual b) vs
          if isElem
            then pure Nothing
            else do
              closure  <- getClosureData y
              subtrees <- mapM (go nextOpts (b : vs)) (allClosures closure)
              pure (Just (Node closure (catMaybes subtrees)))
        TraverseCyclicClosures -> do
          closure  <- getClosureData y
          subtrees <- mapM (go nextOpts (b : vs)) (allClosures closure)
          pure (Just (Node closure (catMaybes subtrees)))

-- | Given a Haskell expression, build a 'Tree' which reflects its heap object
-- representation and render it as 'Text'.
buildAndRenderClosureTree :: ClosureTreeOptions -> a -> IO Text
buildAndRenderClosureTree opts x = do
  mbTr <- buildClosureTree opts x
  case mbTr of
    Nothing -> pure mempty
    Just tr -> pure (renderTree tr renderClosure)
