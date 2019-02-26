{-# LANGUAGE LambdaCase #-}

module Test.Cardano.Prelude.Helpers
  ( assertIsLeftConstr
  , assertIsRight
  , assertIsJust
  , assertIsNothing
  , compareValueRight
  )
where

import Cardano.Prelude

import Data.Data (Data, Constr, toConstr)
import Formatting (Buildable, build, sformat)
import GHC.Stack (HasCallStack, withFrozenCallStack)

import Hedgehog (MonadTest, success, (===))
import Hedgehog.Internal.Property (failWith)

assertIsLeftConstr
  :: (Buildable b, Data a, HasCallStack, MonadTest m)
  => Constr
  -> Either a b
  -> m ()
assertIsLeftConstr expectedFailure = \case
  Left failure -> toConstr failure === expectedFailure
  Right res ->
    withFrozenCallStack $ failWith Nothing (show $ sformat build res)

assertIsRight :: (Buildable a, HasCallStack, MonadTest m) => Either a b -> m ()
assertIsRight = \case
  Left  err -> withFrozenCallStack $ failWith Nothing (show $ sformat build err)
  Right _   -> success

assertIsJust :: (HasCallStack, MonadTest m) => Maybe a -> m ()
assertIsJust = \case
  Nothing -> withFrozenCallStack $ failWith Nothing "Nothing"
  Just _  -> success

assertIsNothing :: (Buildable a, HasCallStack, MonadTest m) => Maybe a -> m ()
assertIsNothing = \case
  Nothing  -> success
  Just res -> withFrozenCallStack $ failWith Nothing (show $ sformat build res)

compareValueRight
  :: (Buildable a, Eq b, HasCallStack, MonadTest m, Show b)
  => b
  -> Either a b
  -> m ()
compareValueRight iVal eith = case eith of
  Left err -> withFrozenCallStack $ failWith Nothing (show $ sformat build err)
  Right fVal -> iVal === fVal

