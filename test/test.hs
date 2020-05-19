module Main
  ( main
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Test.Cardano.Prelude.GHC.Heap.NormalForm
import qualified Test.Cardano.Prelude.GHC.Heap.NormalForm.Classy
import qualified Test.Cardano.Prelude.GHC.Heap.Size
import qualified Test.Cardano.Prelude.GHC.Heap.Tree

main :: IO ()
main = runTests
  [ Test.Cardano.Prelude.GHC.Heap.NormalForm.tests
  , Test.Cardano.Prelude.GHC.Heap.NormalForm.Classy.tests
  , Test.Cardano.Prelude.GHC.Heap.Size.tests
  , Test.Cardano.Prelude.GHC.Heap.Tree.tests
  ]
