module Test.Cardano.Prelude.Helpers
  ( assertEitherIsLeft
  , assertEitherIsRight
  , compareValueRight
  )
where

import Cardano.Prelude

import Formatting (Buildable, build, sformat)

import Hedgehog (MonadTest, success, (===))
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

compareValueRight
  :: (Buildable a, Eq b, MonadTest m, Show b) => b -> Either a b -> m ()
compareValueRight iVal eith = case eith of
  Left  err  -> failWith Nothing (show $ sformat build err)
  Right fVal -> iVal === fVal
