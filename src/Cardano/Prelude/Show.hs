{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}   -- for Rep WRAP

-- | This module uses "GHC.Generics" to define 'gshowsPrecWithoutRecordSyntax',
-- which can often make the result of 'show' more legible, especially for
-- @newtype@s for example.
--
module Cardano.Prelude.Show (
    gshowsPrecWithoutRecordSyntax,
    GShowK1 (..),
    WithoutRecordSyntax,
  ) where

import           Prelude

import           Data.Coerce
import           Data.Kind (Type)
import           GHC.Generics

import qualified Generics.Deriving.Show as GD

-- | Implements 'showsPrec' as if the constructors' record fields were instead
-- plain arguments
--
gshowsPrecWithoutRecordSyntax ::
  forall a.
     ( Generic a
     , Coercible (Rep a) (WithoutRecordSyntax (Rep a))
     , GD.GShow' (WithoutRecordSyntax (Rep a))
     )
  => Int -> a -> ShowS
gshowsPrecWithoutRecordSyntax p =
    GD.gshowsPrecdefault p . WRAP

{-------------------------------------------------------------------------------
  Forgetting fields
-------------------------------------------------------------------------------}

-- not exported
newtype WRAP a = WRAP a

-- Without this definition, GHC cannot make the same inference at the usage
-- sites within this module, probably because f and g involve type families
--
coerce1 :: Coercible f g => f a -> g a
coerce1 x = coerce x

instance
  ( Generic a
  , Coercible (Rep a) (WithoutRecordSyntax (Rep a))
  ) => Generic (WRAP a)
  where
    type Rep (WRAP a) = WithoutRecordSyntax (Rep a)

    to = WRAP . to . coerce1
    from (WRAP a) = (coerce1 . from) a

-- | An implementation detail of 'gshowsPrecWithoutRecordSyntax'
--
-- This removes the metadata in a "GHC.Generics" 'Rep' instance incurred by the
-- use of record syntax in the data type declaration. It also replaces 'K1'
-- with 'GShowK1'.
--
-- The result is as if the constructors' record fields were instead plain
-- arguments, modulo the 'K1' and 'GShowK1' swap.
--
type family WithoutRecordSyntax (rep :: Type -> Type) :: Type -> Type where
    -- the interesting case
    WithoutRecordSyntax (M1 i c f) = M1 i (WithoutRecordSyntaxMeta c) (WithoutRecordSyntax f)

    -- the tedious case
    --
    -- GShow' (K1 i c) requires GShow c, whereas we want it to use Show c
    -- instead
    WithoutRecordSyntax (K1 i c) = GShowK1 i c

    -- the mundane recursive cases
    WithoutRecordSyntax (a :+: b)  = WithoutRecordSyntax a :+: WithoutRecordSyntax b
    WithoutRecordSyntax (a :*: b)  = WithoutRecordSyntax a :*: WithoutRecordSyntax b
    WithoutRecordSyntax (a :.: b)  = WithoutRecordSyntax a :.: WithoutRecordSyntax b

    -- the mundane base cases
    WithoutRecordSyntax V1           = V1
    WithoutRecordSyntax U1           = U1
    WithoutRecordSyntax UChar        = UChar
    WithoutRecordSyntax UDouble      = UDouble
    WithoutRecordSyntax UFloat       = UFloat
    WithoutRecordSyntax UInt         = UInt
    WithoutRecordSyntax UWord        = UWord

-- | Remove data about field selectors
--
type family WithoutRecordSyntaxMeta (rep :: Meta) :: Meta where
    WithoutRecordSyntaxMeta ('MetaSel 'Nothing  su ss ds) = 'MetaSel 'Nothing su ss ds
    WithoutRecordSyntaxMeta ('MetaSel ('Just _) su ss ds) = 'MetaSel 'Nothing su ss ds
    WithoutRecordSyntaxMeta ('MetaCons n f _s)            = 'MetaCons n f 'False
    WithoutRecordSyntaxMeta ('MetaData n m p nt)          = 'MetaData n m p nt

-- | Merely an implementation detail
--
-- This constructor must be in scope at usage sites of 'gshowsPrecWithoutRecordSyntax'
-- for its 'Coercible' constraint to be satisfiable.
--
newtype GShowK1 i c p = GShowK1 (K1 i c p)

instance Show c => GD.GShow' (GShowK1 i c) where
    gshowsPrec' _ p (GShowK1 (K1 c)) = showsPrec p c

    -- copied from @GShow' (K1 i c)@ instance in @generic-deriving@
    isNullary _ = False
