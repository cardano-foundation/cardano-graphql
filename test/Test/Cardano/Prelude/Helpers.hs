module Test.Cardano.Prelude.Helpers
  ( assertEitherIsLeft
  , assertEitherIsRight
  )
where

import Cardano.Prelude

import Formatting (Buildable, build, sformat)

import Hedgehog (MonadTest, success)
import Hedgehog.Internal.Property (failWith)

assertEitherIsLeft
  :: (MonadTest m, Buildable c) => (a -> Either b c) -> a -> m ()
assertEitherIsLeft func val = case func val of
  Left  _   -> success
  Right res -> failWith Nothing (show $ sformat build res)

assertEitherIsRight
  :: (MonadTest m, Buildable b) => (a -> Either b c) -> a -> m ()
assertEitherIsRight func val = case func val of
  Left  err -> failWith Nothing (show $ sformat build err)
  Right _   -> success
