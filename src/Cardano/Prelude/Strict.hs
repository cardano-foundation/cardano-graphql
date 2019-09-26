-- | Helper functions for enforcing strictness.

module Cardano.Prelude.Strict
  ( forceElemsToWHNF
  )
where

import Cardano.Prelude.Base

-- | Force all of the elements of a 'Foldable' to weak head normal form.
--
-- In order to ensure that all of the elements of a 'Foldable' are strict, we
-- can simply 'foldMap' over it and 'seq' each value with '()'. However,
-- '()''s 'mappend' implementation is actually completely lazy: @_ <> _ = ()@
-- So, in order to work around this, we instead utilize this newly defined
-- 'StrictUnit' whose 'mappend' implementation is specifically strict.
forceElemsToWHNF :: Foldable t => t a -> t a
forceElemsToWHNF x = foldMap (`seq` StrictUnit) x `seq` x

-- | The equivalent of '()', but with a strict 'mappend' implementation.
--
-- For more information, see the documentation for 'forceElemsToWHNF'.
data StrictUnit = StrictUnit

instance Semigroup StrictUnit where
  StrictUnit <> StrictUnit = StrictUnit

instance Monoid StrictUnit where
  mempty = StrictUnit
