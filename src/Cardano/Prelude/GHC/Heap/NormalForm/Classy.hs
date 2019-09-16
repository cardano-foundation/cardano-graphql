{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Cardano.Prelude.GHC.Heap.NormalForm.Classy (
    NoUnexpectedThunks(..)
  , genericWhnfNoUnexpectedThunks
  , UseIsNormalForm(..)
  ) where

import Cardano.Prelude.Base

import Data.Foldable (toList)
import Data.Sequence (Seq)
import GHC.Exts.Heap
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Cardano.Prelude.GHC.Heap.NormalForm as NF

class NoUnexpectedThunks a where
  -- | Check if the argument does not contain any unexpected thunks
  --
  -- It is acceptable for 'noUnexpectedThunks' to evaluate its argument to NF
  -- as it executes, as long as arguments that contain unexpected thunks are
  -- reported as not in normal form (return @False@).
  --
  -- For example, the internal fingertree 'Data.Sequence.Sequence' might contain
  -- thunks (this is important for the asymptotic complexity of this data
  -- structure). However, we should still check that the /values/ in the
  -- sequence don't contain any unexpected thunks. This means that we need to
  -- traverse the sequence, which might force some of the thunks in the tree;
  -- this is acceptable.
  --
  -- For most datatypes we should have that
  --
  -- > noUnexpectedThunks x == isNormalForm x
  --
  -- See also discussion of caveats listed for 'NF.isNormalForm'.
  noUnexpectedThunks :: a -> IO Bool
  noUnexpectedThunks = whenInHeadNormalForm whnfNoUnexpectedThunks

  -- | Like 'noUnexpectedThunks', but can assume value is in WHNF
  whnfNoUnexpectedThunks :: a -> IO Bool
  default whnfNoUnexpectedThunks :: (Generic a, GNoUnexpectedThunks (Rep a))
                                 => a -> IO Bool
  whnfNoUnexpectedThunks = genericWhnfNoUnexpectedThunks

-- | Check if argument contains no unexpected thunks by converting to generic
-- representation first
--
-- NOTE: if it's in WHNF then we can safely translate to generic value without
-- forcing anything.
genericWhnfNoUnexpectedThunks :: (Generic a, GNoUnexpectedThunks (Rep a))
                              => a -> IO Bool
genericWhnfNoUnexpectedThunks = gNoUnexpectedThunks . from

whenInHeadNormalForm :: (a -> IO Bool) -> (a -> IO Bool)
whenInHeadNormalForm k x = do
    c   <- getBoxedClosureData (asBox x)
    hnf <- NF.isHeadNormalForm c
    if hnf
      then k x
      else return False

{-------------------------------------------------------------------------------
  Standard instances
-------------------------------------------------------------------------------}

deriving via UseIsNormalForm Int instance NoUnexpectedThunks Int

-- | Instance for 'Seq' checks elements only
--
-- The internal fingertree in 'Seq' might have thunks, which is essential for
-- its asymptotic complexity.
instance NoUnexpectedThunks a => NoUnexpectedThunks (Seq a) where
  whnfNoUnexpectedThunks = andM . map noUnexpectedThunks . toList

-- | Instance for 'Map' checks elements only (Map is spine strict)
instance ( NoUnexpectedThunks k
         , NoUnexpectedThunks v
         ) => NoUnexpectedThunks (Map k v) where
  whnfNoUnexpectedThunks = andM . map noUnexpectedThunks . Map.toList

-- | Instance for 'Set' checks elements only (Set is spine strict)
instance NoUnexpectedThunks a => NoUnexpectedThunks (Set a) where
  whnfNoUnexpectedThunks = andM . map noUnexpectedThunks . Set.toList

-- | Instance for function closures is always 'True'
--
-- We could use 'isNormalForm' here, but this would (1) break compositionality
-- (what if the function closure contained a sequence?) and (2) 'isNormalForm'
-- doesn't seem to work so well for function closures anyway.
instance NoUnexpectedThunks (a -> b) where
  whnfNoUnexpectedThunks _ = return True

{-------------------------------------------------------------------------------
  Instances that rely on generics

  We define instances for tuples up to length 7; larger tuples don't have
  standard 'Generic' instances.
-------------------------------------------------------------------------------}

instance NoUnexpectedThunks ()

instance ( NoUnexpectedThunks a
         , NoUnexpectedThunks b
         ) => NoUnexpectedThunks (a, b)

instance ( NoUnexpectedThunks a
         , NoUnexpectedThunks b
         , NoUnexpectedThunks c
         ) => NoUnexpectedThunks (a, b, c)

instance ( NoUnexpectedThunks a
         , NoUnexpectedThunks b
         , NoUnexpectedThunks c
         , NoUnexpectedThunks d
         ) => NoUnexpectedThunks (a, b, c, d)

instance ( NoUnexpectedThunks a
         , NoUnexpectedThunks b
         , NoUnexpectedThunks c
         , NoUnexpectedThunks d
         , NoUnexpectedThunks e
         ) => NoUnexpectedThunks (a, b, c, d, e)

instance ( NoUnexpectedThunks a
         , NoUnexpectedThunks b
         , NoUnexpectedThunks c
         , NoUnexpectedThunks d
         , NoUnexpectedThunks e
         , NoUnexpectedThunks f
         ) => NoUnexpectedThunks (a, b, c, d, e, f)

instance ( NoUnexpectedThunks a
         , NoUnexpectedThunks b
         , NoUnexpectedThunks c
         , NoUnexpectedThunks d
         , NoUnexpectedThunks e
         , NoUnexpectedThunks f
         , NoUnexpectedThunks g
         ) => NoUnexpectedThunks (a, b, c, d, e, f, g)

instance ( NoUnexpectedThunks a
         , NoUnexpectedThunks b
         ) => NoUnexpectedThunks (Either a b)

instance NoUnexpectedThunks a => NoUnexpectedThunks [a]

{-------------------------------------------------------------------------------
  Using the standard 'isNormalForm' check
-------------------------------------------------------------------------------}

newtype UseIsNormalForm a = UseIsNormalForm a

instance NoUnexpectedThunks (UseIsNormalForm a) where
  noUnexpectedThunks     = NF.isNormalForm
  whnfNoUnexpectedThunks = NF.isNormalForm

{-------------------------------------------------------------------------------
  Generic instance
-------------------------------------------------------------------------------}

class GNoUnexpectedThunks f where
  gNoUnexpectedThunks :: f x -> IO Bool

instance GNoUnexpectedThunks f => GNoUnexpectedThunks (M1 i c f) where
  gNoUnexpectedThunks (M1 fp) = gNoUnexpectedThunks fp

instance ( GNoUnexpectedThunks f
         , GNoUnexpectedThunks g
         ) => GNoUnexpectedThunks (f :*: g) where
  gNoUnexpectedThunks (fp :*: gp) =
      andM [ gNoUnexpectedThunks fp
           , gNoUnexpectedThunks gp
           ]

instance ( GNoUnexpectedThunks f
         , GNoUnexpectedThunks g
         ) => GNoUnexpectedThunks (f :+: g) where
  gNoUnexpectedThunks (L1 fp) = gNoUnexpectedThunks fp
  gNoUnexpectedThunks (R1 fp) = gNoUnexpectedThunks fp

instance NoUnexpectedThunks c => GNoUnexpectedThunks (K1 i c) where
  gNoUnexpectedThunks (K1 c) = noUnexpectedThunks c

instance GNoUnexpectedThunks U1 where
  gNoUnexpectedThunks U1 = return True

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

andM :: Monad m => [m Bool] -> m Bool
andM []       = return True
andM (mb:mbs) = do
    b <- mb
    if b then andM mbs else return False
