module Main
  ( main
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Test.Cardano.Prelude.GHC.Heap.NormalForm

main :: IO ()
main = runTests [Test.Cardano.Prelude.GHC.Heap.NormalForm.tests]
