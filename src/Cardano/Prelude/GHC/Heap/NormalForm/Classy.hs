{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE TypeFamilies          #-}
-- This here to allow the following instance:
-- instance (HasField x a t, HasFields xs a) => HasFields (x ': xs) a
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


module Cardano.Prelude.GHC.Heap.NormalForm.Classy (
    -- * Check a value for unexpected thunks
    NoUnexpectedThunks(..)
  , allNoUnexpectedThunks
  , noUnexpectedThunksInValues
  , noUnexpectedThunksInKeysAndValues
  , unsafeNoUnexpectedThunks
    -- * Results of the check
  , ThunkInfo(..)
  , UnexpectedThunkInfo(..)
  , thunkInfoToIsNF
    -- * Deriving-via wrappers
  , UseIsNormalForm(..)
  , UseIsNormalFormNamed(..)
  , OnlyCheckIsWHNF(..)
  , AllowThunk(..)
  , AllowThunksIn(..)
  ) where

import Cardano.Prelude.Base

import Data.Time
import GHC.Exts.Heap (asBox, getBoxedClosureData)
import GHC.TypeLits (CmpSymbol)
import Prelude (String)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS.Strict
import qualified Data.ByteString.Lazy as BS.Lazy
import           Data.ByteString.Short (ShortByteString)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text.Strict
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector.Boxed
import qualified Data.Vector.Unboxed as Vector.Unboxed

import Cardano.Prelude.GHC.Heap.Tree
import qualified Cardano.Prelude.GHC.Heap.NormalForm as NF

{-------------------------------------------------------------------------------
  Check a value for unexpected thunks
-------------------------------------------------------------------------------}

-- | Check a value for unexpected thunks
class NoUnexpectedThunks a where
  -- | Check if the argument does not contain any unexpected thunks
  --
  -- For most datatypes, we should have that
  --
  -- > noUnexpectedThunks ctxt x == NoUnexpectedThunks
  --
  -- if and only if
  --
  -- > isNormalForm x
  --
  -- For some datatypes however, some thunks are expected. For example, the
  -- internal fingertree 'Data.Sequence.Sequence' might contain thunks (this is
  -- important for the asymptotic complexity of this data structure). However,
  -- we should still check that the /values/ in the sequence don't contain any
  -- unexpected thunks.
  --
  -- This means that we need to traverse the sequence, which might force some of
  -- the thunks in the tree. In general, it is acceptable for
  -- 'noUnexpectedThunks' to force such "expected thunks", as long as it always
  -- reports the /unexpected/ thunks.
  --
  -- The default implementation of 'noUnexpectedThunks' checks that the argument
  -- is in WHNF, and if so, adds the type into the context (using 'showTypeOf'),
  -- and calls 'whnfNoUnexpectedThunks'. See 'UnexpectedThunkInfo' for a
  -- detailed discussion of the type context.
  --
  -- See also discussion of caveats listed for 'NF.isNormalForm'.
  noUnexpectedThunks :: HasCallStack => [String] -> a -> IO ThunkInfo
  noUnexpectedThunks ctxt x = do
      c   <- getBoxedClosureData (asBox x)
      hnf <- NF.isHeadNormalForm c
      if hnf
        then whnfNoUnexpectedThunks ctxt' x
        else return $ UnexpectedThunk UnexpectedThunkInfo {
                          unexpectedThunkContext   = ctxt'
                        , unexpectedThunkCallStack = callStack
                        , unexpectedThunkClosure   = Just (renderClosure c)
                        }
    where
      ctxt' :: [String]
      ctxt' = showTypeOf (Proxy @a) : ctxt

  -- | Check that the argument is in normal form, assuming it is in WHNF.
  --
  -- The context will already have been extended with the type we're looking at,
  -- so all that's left is to look at the thunks /inside/ the type. The default
  -- implementation uses GHC Generics to do this.
  whnfNoUnexpectedThunks :: [String] -> a -> IO ThunkInfo
  default whnfNoUnexpectedThunks :: (Generic a, GWhnfNoUnexpectedThunks '[] (Rep a))
                                 => [String] -> a -> IO ThunkInfo
  whnfNoUnexpectedThunks ctxt x = gWhnfNoUnexpectedThunks (Proxy @'[]) ctxt fp
    where
      -- Force the result of @from@ to WHNF: we are not interested in thunks
      -- that arise from the translation to the generic representation.
      fp :: Rep a x
      !fp = from x

  -- | Show type @a@ (to add to the context)
  --
  -- We try hard to avoid 'Typeable' constraints in this module: there are types
  -- with no 'Typeable' instance but with a 'NoUnexpectedThunks' instance (most
  -- important example are types such as @ST s@ which rely on parametric
  -- polymorphism). By default we should therefore only show the "outer layer";
  -- for example, if we have a type
  --
  -- > Seq (ST s ())
  --
  -- then 'showTypeOf' should just give @Seq@, leaving it up to the instance for
  -- @ST@ to decide how to implement 'showTypeOf'; this keeps things
  -- compositional. The default implementation does precisely this using the
  -- metadata that GHC Generics provides.
  --
  -- For convenience, however, some of the @deriving via@ newtype wrappers we
  -- provide /do/ depend on @Typeable@; see below.
  showTypeOf :: Proxy a -> String
  default showTypeOf :: (Generic a, GShowTypeOf (Rep a)) => Proxy a -> String
  showTypeOf _ = gShowTypeOf (from x)
    where
      x :: a
      x = x

-- | Short-circuit a list of checks
allNoUnexpectedThunks :: [IO ThunkInfo] -> IO ThunkInfo
allNoUnexpectedThunks = go
  where
    go :: [IO ThunkInfo] -> IO ThunkInfo
    go []     = return NoUnexpectedThunks
    go (a:as) = do
        nf <- a
        case nf of
          NoUnexpectedThunks  -> go as
          UnexpectedThunk nfo -> return $ UnexpectedThunk nfo

-- | Check that all elements in the list are thunk-free
--
-- Does not check the list itself. Useful for checking the elements of a
-- container.
--
-- See also 'noUnexpectedThunksInKeysAndValues'
noUnexpectedThunksInValues :: NoUnexpectedThunks a
                           => [String] -> [a] -> IO ThunkInfo
noUnexpectedThunksInValues ctxt =
      allNoUnexpectedThunks
    . map (noUnexpectedThunks ctxt)

-- | Variant on 'noUnexpectedThunksInValues' for keyed containers.
--
-- Neither the list nor the tuples are checked for thunks.
noUnexpectedThunksInKeysAndValues :: (NoUnexpectedThunks k, NoUnexpectedThunks v)
                                  => [String] -> [(k, v)] -> IO ThunkInfo
noUnexpectedThunksInKeysAndValues ctxt =
      allNoUnexpectedThunks
    . concatMap (\(k, v) -> [ noUnexpectedThunks ctxt k
                            , noUnexpectedThunks ctxt v
                            ])

{-# NOINLINE unsafeNoUnexpectedThunks #-}
unsafeNoUnexpectedThunks :: NoUnexpectedThunks a => a -> Maybe String
unsafeNoUnexpectedThunks a = unsafePerformIO $ errorMessage =<< noUnexpectedThunks [] a
  where
    errorMessage :: ThunkInfo -> IO (Maybe String)
    errorMessage NoUnexpectedThunks     = return Nothing
    errorMessage (UnexpectedThunk info) = do
        -- We render the tree /after/ checking; in a way, this is not correct,
        -- because 'noUnexpectedThunks' might have forced some stuff. However,
        -- computing the tree beforehand, even when there is no failure, would
        -- be prohibitively expensive.
        --
        -- TODO rendering the tree has been disabled for now, as this loops
        -- indefinitely, consuming gigabytes of memory, and prevents us from
        -- printing a message about the thunk. Moreover, the thunk info is in
        -- most cases a clearer indication of where the thunk is than the
        -- /huge/ tree. Use the two commented-out lines below to include the
        -- tree in the message.
        --
        -- tree <- Text.Strict.unpack <$> buildAndRenderClosureTree opts a
        -- return $ Just $ show info ++ "\nTree:\n" ++ tree

        let _mkTree = Text.Strict.unpack <$> buildAndRenderClosureTree opts a
        return $ Just $ show info

    opts :: ClosureTreeOptions
    opts = ClosureTreeOptions {
        ctoMaxDepth       = AnyDepth
      , ctoCyclicClosures = NoTraverseCyclicClosures
      }

{-------------------------------------------------------------------------------
  Results of the check
-------------------------------------------------------------------------------}

data ThunkInfo =
    -- | We found no unexpected thunks
    NoUnexpectedThunks

    -- | We found unexpected thunks
  | UnexpectedThunk UnexpectedThunkInfo
  deriving (Show)

-- | Information about unexpected thunks
--
-- TODO: The ghc-debug work by Matthew Pickering includes some work that allows
-- to get source spans from closures. If we could take advantage of that, we
-- could not only show the type of the unexpected thunk, but also where it got
-- allocated.
data UnexpectedThunkInfo = UnexpectedThunkInfo {
      -- The @[String]@ argument is intended to give a clue to add debugging.
      -- For example, suppose we have something of type @(Int, [Int])@. The
      -- various contexts we might get are
      --
      -- > Context                  The thunk is..
      -- > ---------------------------------------------------------------------
      -- > ["(,)"]                  the pair itself
      -- > ["Int","(,)"]            the Int in the pair
      -- > ["[]","(,)"]             the [Int] in the pair
      -- > ["Int","[]","(,)"]       an Int in the [Int] in the pair
      unexpectedThunkContext   :: [String]

      -- | CallStack to where we /found/ the unexpected thunk
      --
      -- NOTE: This is /not/ the callstack of the /creation/ of the thunk.
    , unexpectedThunkCallStack :: CallStack

      -- | The specific closure where we found the problem
      --
      -- This will be unavailable when we used 'isNormalForm' to do the check,
      -- since in that case we don't have detailed information available about
      -- exactly which thunk was the problem. To aid debugging in such cases
      -- 'buildAndRenderClosureTree' can be used to render the closure tree.
    , unexpectedThunkClosure   :: Maybe Text
    }
  deriving (Show)

-- | Was the value in normal form?
thunkInfoToIsNF :: ThunkInfo -> Bool
thunkInfoToIsNF NoUnexpectedThunks  = True
thunkInfoToIsNF (UnexpectedThunk _) = False

{-------------------------------------------------------------------------------
  Newtype wrappers for deriving via
-------------------------------------------------------------------------------}

-- | Newtype wrapper for use with @deriving via@ to use 'isNormalForm'
--
-- 'NoUnexpectedThunks' instances derived using 'UseIsNormalForm' will use
-- 'isNormalForm' to check whether the argument is in normal form or not.
-- Since 'isNormalForm' does not need any additional type class instances, this
-- is useful for types that contain fields for which 'NoUnexpectedThunks'
-- instances are not available.
--
-- Since the primary use case for 'UseIsNormalForm' then is to give instances
-- for 'NoUnexpectedThunks' from third party libraries, we also don't want to
-- rely on a 'Generic' instance, which may likewise not be available. Instead,
-- we will rely on 'Typeable', which is available for /all/ types. However, as
-- 'showTypeOf' explains, requiring 'Typeable' may not always be suitable; if
-- it isn't, 'UseIsNormalFormNamed' can be used.
--
-- Example:
--
-- > deriving via UseIsNormalForm T instance NoUnexpectedThunks T
newtype UseIsNormalForm a = UseIsNormalForm a

-- | Newtype wrapper for use with @deriving via@ to use 'isNormalForm'
--
-- This is a variant on 'UseIsNormalForm' which does not depend on 'Typeable',
-- but instead requires a name to be explicitly given. Example:
--
-- > deriving via UseIsNormalFormNamed "T" T instance NoUnexpecedThunks T
newtype UseIsNormalFormNamed (name :: Symbol) a = UseIsNormalFormNamed a

-- | Newtype wrapper for use with @deriving via@ to check for WHNF only
--
-- For some types we don't want to check for nested thunks, and we only want
-- check if the argument is in WHNF, not in NF. A typical example are functions;
-- see the instance of @(a -> b)@ for detailed discussion. This should be used
-- sparingly.
--
-- Example:
--
-- > deriving via OnlyCheckIsWHNF "T" T instance NoUnexpectedThunks T
newtype OnlyCheckIsWHNF (name :: Symbol) a = OnlyCheckIsWHNF a

-- | Newtype wrapper for values that should be allowed to be a thunk
--
-- This should be used /VERY/ sparingly, and should /ONLY/ be used on values
-- (or, even rarer, types) which you are /SURE/ cannot retain any data that they
-- shouldn't. Bear in mind allowing a value of type @T@ to be a thunk might
-- cause a value of type @S@ to be retained if @T@ was computed from @S@.
newtype AllowThunk a = AllowThunk a

-- | Newtype wrapper for records where some of the fields are allowed to be
-- thunks.
--
--Example:
-- > deriving via AllowThunksIn '["foo","bar"] T instance NoUnexpectedThunks T
--
-- This will create an instance that skips the thunk checks for the "foo" and
-- "bar" fields.

newtype AllowThunksIn (fields :: [Symbol]) a = AllowThunksIn a

{-------------------------------------------------------------------------------
  Internal: instances for the deriving-via wrappers
-------------------------------------------------------------------------------}

instance Typeable a => NoUnexpectedThunks (UseIsNormalForm a) where
  showTypeOf _ = show $ typeRep (Proxy @a)
  whnfNoUnexpectedThunks = noUnexpectedThunksUsingNormalForm

instance KnownSymbol name => NoUnexpectedThunks (UseIsNormalFormNamed name a) where
  showTypeOf _ = symbolVal (Proxy @name)
  whnfNoUnexpectedThunks = noUnexpectedThunksUsingNormalForm

instance KnownSymbol name => NoUnexpectedThunks (OnlyCheckIsWHNF name a) where
  showTypeOf _ = symbolVal (Proxy @name)
  whnfNoUnexpectedThunks _ _ = return NoUnexpectedThunks

instance NoUnexpectedThunks (AllowThunk a) where
  showTypeOf _ = "<never used since never fails>"
  noUnexpectedThunks _ _ = return NoUnexpectedThunks
  whnfNoUnexpectedThunks = noUnexpectedThunks

instance (HasFields s a, Generic a, Typeable a, GWhnfNoUnexpectedThunks s (Rep a))
   => NoUnexpectedThunks (AllowThunksIn s a) where
  showTypeOf _ = show $ typeRep (Proxy @a)
  whnfNoUnexpectedThunks ctxt (AllowThunksIn x) = gWhnfNoUnexpectedThunks (Proxy @s) ctxt fp
    where
      fp :: Rep a x
      !fp = from x

-- | This exists to catch mismatches between the arguments to `AllowThunksIn` and
-- the fields of a record. If any of the symbols is not the name of a field then
-- this constraint won't be satisfied.
class HasFields (s :: [Symbol]) a
instance HasFields '[] a
instance (HasField x a t, HasFields xs a) => HasFields (x ': xs) a

-- | Internal: implementation of 'whnfNoUnexpectedThunks' for 'UseIsNormalForm'
-- and 'UseIsNormalFormNamed'
noUnexpectedThunksUsingNormalForm :: [String] -> a -> IO ThunkInfo
noUnexpectedThunksUsingNormalForm ctxt x = do
    nf <- NF.isNormalForm x
    return $ if nf then NoUnexpectedThunks
                   else UnexpectedThunk UnexpectedThunkInfo {
                            unexpectedThunkContext   = "..." : ctxt
                          , unexpectedThunkCallStack = callStack
                          , unexpectedThunkClosure   = Nothing
                          }

{-------------------------------------------------------------------------------
  Internal: generic infrastructure
-------------------------------------------------------------------------------}

class GWhnfNoUnexpectedThunks (a :: [Symbol]) f where
  gWhnfNoUnexpectedThunks :: proxy a -> [String] -> f x -> IO ThunkInfo

instance GWhnfNoUnexpectedThunks a f => GWhnfNoUnexpectedThunks a (D1 c f) where
  gWhnfNoUnexpectedThunks a ctxt (M1 fp) = gWhnfNoUnexpectedThunks a ctxt fp

instance GWhnfNoUnexpectedThunks a f => GWhnfNoUnexpectedThunks a (C1 c f) where
  gWhnfNoUnexpectedThunks a ctxt (M1 fp) = gWhnfNoUnexpectedThunks a ctxt fp

instance (CaseS1 f (ElemSymbol fieldName a)) =>
  GWhnfNoUnexpectedThunks a (S1 ('MetaSel ('Just fieldName) su ss ds) f)  where
  gWhnfNoUnexpectedThunks _ ctxt (M1 fp) = aux (Proxy @(ElemSymbol fieldName a)) ctxt fp

class CaseS1 f (b :: Bool) where
  aux :: proxy b -> [String] -> f x -> IO ThunkInfo
instance CaseS1 f 'True where
  aux _ _ _ = return NoUnexpectedThunks
instance GWhnfNoUnexpectedThunks '[] f => CaseS1 f 'False where
  aux _ ctxt f = gWhnfNoUnexpectedThunks (Proxy @'[]) ctxt f

instance
  GWhnfNoUnexpectedThunks a f =>
  GWhnfNoUnexpectedThunks a (S1 ('MetaSel ('Nothing) su ss ds) f) where
  gWhnfNoUnexpectedThunks a ctxt (M1 fp) = gWhnfNoUnexpectedThunks a ctxt fp

instance ( GWhnfNoUnexpectedThunks a f
         , GWhnfNoUnexpectedThunks a g
         ) => GWhnfNoUnexpectedThunks a (f :*: g) where
  gWhnfNoUnexpectedThunks a ctxt (fp :*: gp) = do
      allNoUnexpectedThunks [
          gWhnfNoUnexpectedThunks a ctxt fp
        , gWhnfNoUnexpectedThunks a ctxt gp
        ]

instance ( GWhnfNoUnexpectedThunks a f
         , GWhnfNoUnexpectedThunks a g
         ) => GWhnfNoUnexpectedThunks a (f :+: g) where
  gWhnfNoUnexpectedThunks a ctxt (L1 fp) = gWhnfNoUnexpectedThunks a ctxt fp
  gWhnfNoUnexpectedThunks a ctxt (R1 gp) = gWhnfNoUnexpectedThunks a ctxt gp

instance NoUnexpectedThunks c => GWhnfNoUnexpectedThunks a (K1 i c) where
  gWhnfNoUnexpectedThunks _a ctxt (K1 c) = noUnexpectedThunks ctxt' c
    where
      -- If @c@ is a recursive occurrence of the type itself, we want to avoid
      -- accumulating context. For example, suppose we are dealing with @[Int]@,
      -- and we have an unexpected thunk as the third @Int@ in the list. If
      -- we use the generic instance, then without this correction, the final
      -- context will look something like
      --
      -- > ["Int", "[]", "[]", "[]"]
      --
      -- While that is more informative (it's the /third/ element that is a
      -- thunk), it's not that helpful (typically we just want /all/ elements
      -- to be in NF). We strip the context here so that we just get
      --
      -- > ["Int", "[]"]
      --
      -- which is a bit easier to interpret.
      ctxt' = case ctxt of
                hd : tl | hd == showTypeOf (Proxy @c) -> tl
                _otherwise                            -> ctxt

instance GWhnfNoUnexpectedThunks a U1 where
  gWhnfNoUnexpectedThunks _a _ctxt U1 = return NoUnexpectedThunks

instance GWhnfNoUnexpectedThunks a V1 where
  -- By assumption, the argument is already in WHNF. Since every inhabitant of
  -- this type is bottom, this code is therefore unreachable.
  gWhnfNoUnexpectedThunks _a _ctxt _ =
      panic "unreachable gWhnfNoUnexpectedThunks @V1"

{-------------------------------------------------------------------------------
  Internal: generic function to get name of a type
-------------------------------------------------------------------------------}

class GShowTypeOf f where
  gShowTypeOf :: f x -> String

instance Datatype c => GShowTypeOf (D1 c f) where
  gShowTypeOf = datatypeName

{-------------------------------------------------------------------------------
  Instances for primitive types
-------------------------------------------------------------------------------}

deriving via UseIsNormalForm Bool    instance NoUnexpectedThunks Bool
deriving via UseIsNormalForm Natural instance NoUnexpectedThunks Natural
deriving via UseIsNormalForm Integer instance NoUnexpectedThunks Integer
deriving via UseIsNormalForm Float   instance NoUnexpectedThunks Float
deriving via UseIsNormalForm Double  instance NoUnexpectedThunks Double
deriving via UseIsNormalForm Char    instance NoUnexpectedThunks Char

deriving via UseIsNormalForm Int   instance NoUnexpectedThunks Int
deriving via UseIsNormalForm Int8  instance NoUnexpectedThunks Int8
deriving via UseIsNormalForm Int16 instance NoUnexpectedThunks Int16
deriving via UseIsNormalForm Int32 instance NoUnexpectedThunks Int32
deriving via UseIsNormalForm Int64 instance NoUnexpectedThunks Int64

deriving via UseIsNormalForm Word   instance NoUnexpectedThunks Word
deriving via UseIsNormalForm Word8  instance NoUnexpectedThunks Word8
deriving via UseIsNormalForm Word16 instance NoUnexpectedThunks Word16
deriving via UseIsNormalForm Word32 instance NoUnexpectedThunks Word32
deriving via UseIsNormalForm Word64 instance NoUnexpectedThunks Word64

deriving via UseIsNormalForm Day              instance NoUnexpectedThunks Day
deriving via UseIsNormalForm DiffTime         instance NoUnexpectedThunks DiffTime
deriving via UseIsNormalForm LocalTime        instance NoUnexpectedThunks LocalTime
deriving via UseIsNormalForm NominalDiffTime  instance NoUnexpectedThunks NominalDiffTime
deriving via UseIsNormalForm TimeLocale       instance NoUnexpectedThunks TimeLocale
deriving via UseIsNormalForm TimeOfDay        instance NoUnexpectedThunks TimeOfDay
deriving via UseIsNormalForm TimeZone         instance NoUnexpectedThunks TimeZone
deriving via UseIsNormalForm UniversalTime    instance NoUnexpectedThunks UniversalTime
deriving via UseIsNormalForm UTCTime          instance NoUnexpectedThunks UTCTime
deriving via UseIsNormalForm ZonedTime        instance NoUnexpectedThunks ZonedTime

{-------------------------------------------------------------------------------
  Instances for text types

  We use UseIsNormalFormNamed here so that we can distinguish between strict
  and lazy variants.
-------------------------------------------------------------------------------}

-- | Strict bytestrings /shouldn't/ contain any thunks, but could, due to
-- <https://gitlab.haskell.org/ghc/ghc/issues/17290>. However, such thunks
-- can't retain any data that they shouldn't, and so it's safe to ignore such
-- thunks.
deriving via OnlyCheckIsWHNF "Strict.ByteString" BS.Strict.ByteString instance NoUnexpectedThunks BS.Strict.ByteString

-- | We have:
--
-- > data ShortByteString = SBS ByteArray#
--
-- which means that values of this type consist of a tag followed by an
-- __unboxed__ byte array, which can't contain thunks. Therefore we only check
-- WHNF.
deriving via OnlyCheckIsWHNF "ShortByteString" ShortByteString instance NoUnexpectedThunks ShortByteString

-- | Unlike strict bytestrings, lazy bytestrings of course /could/ have thunks
deriving via UseIsNormalFormNamed "Lazy.ByteString" BS.Lazy.ByteString instance NoUnexpectedThunks BS.Lazy.ByteString

deriving via UseIsNormalFormNamed "Strict.Text" Text.Strict.Text instance NoUnexpectedThunks Text.Strict.Text
deriving via UseIsNormalFormNamed "Lazy.Text"   Text.Lazy.Text   instance NoUnexpectedThunks Text.Lazy.Text

{-------------------------------------------------------------------------------
  Instances that use the generic definition
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
instance NoUnexpectedThunks a => NoUnexpectedThunks (Maybe a)
instance NoUnexpectedThunks a => NoUnexpectedThunks (NonEmpty a)

{-------------------------------------------------------------------------------
  Spine-strict container types

  Such types can /only/ contain thunks in the values, so that's all we check.
  Note that containers using keys are typically strict in those keys, but that
  forces them to WHNF only, not NF; in /most/ cases the @Ord@ instance on those
  keys will force them to NF, but not /always/ (for example, when using lists
  as keys); this means we must check keys for thunks to be sure.
-------------------------------------------------------------------------------}

instance ( NoUnexpectedThunks k
         , NoUnexpectedThunks v
         ) => NoUnexpectedThunks (Map k v) where
  showTypeOf _ = "Map"
  whnfNoUnexpectedThunks ctxt = noUnexpectedThunksInKeysAndValues ctxt
                              . Map.toList

instance NoUnexpectedThunks a => NoUnexpectedThunks (Set a) where
  showTypeOf _ = "Set"
  whnfNoUnexpectedThunks ctxt = noUnexpectedThunksInValues ctxt
                              . Set.toList

instance NoUnexpectedThunks a => NoUnexpectedThunks (IntMap a) where
  showTypeOf _ = "IntMap"
  whnfNoUnexpectedThunks ctxt = noUnexpectedThunksInValues ctxt
                              . IntMap.toList

instance NoUnexpectedThunks a => NoUnexpectedThunks (Vector.Boxed.Vector a) where
  showTypeOf _ = "Boxed.Vector"
  whnfNoUnexpectedThunks ctxt = noUnexpectedThunksInValues ctxt
                              . Vector.Boxed.toList

{-------------------------------------------------------------------------------
  Instances for which we check for WHNF only

  These all need justification.
-------------------------------------------------------------------------------}

-- | Unboxed vectors can't contain thunks
--
-- Implementation note: defined manually rather than using 'OnlyCheckIsWHNF'
-- due to ghc limitation in deriving via, making it impossible to use with it
-- with data families.
instance NoUnexpectedThunks (Vector.Unboxed.Vector a) where
  showTypeOf _ = "Unboxed.Vector"
  whnfNoUnexpectedThunks _ _ = return NoUnexpectedThunks

-- | We do NOT check function closures for captured thunks by default
--
-- Since we have no type information about the values captured in a thunk,
-- the only check we could possibly do is 'isNormalForm': we can't recursively
-- call 'noUnexpectedThunks' on those captured values, which is problematic if
-- any of those captured values /requires/ a custom instance (for example,
-- data types that depend on laziness, such as 'Seq').
--
-- By default we therefore /only/ check if the function is in WHNF, and don't
-- check the captured values at all. If you want a stronger check, you can
-- use @IsNormalForm (a -> b)@ instead.
deriving via OnlyCheckIsWHNF "->" (a -> b) instance NoUnexpectedThunks (a -> b)

-- | We do not check IO actions for captured thunks by default
--
-- See instance for @(a -> b)@ for detailed discussion.
deriving via OnlyCheckIsWHNF "IO" (IO a) instance NoUnexpectedThunks (IO a)

{-------------------------------------------------------------------------------
  Special cases
-------------------------------------------------------------------------------}

-- | Since CallStacks can't retain application data, we don't want to check
-- them for thunks /at all/
deriving via AllowThunk CallStack instance NoUnexpectedThunks CallStack

-- | Instance for 'Seq' checks elements only
--
-- The internal fingertree in 'Seq' might have thunks, which is essential for
-- its asymptotic complexity.
instance NoUnexpectedThunks a => NoUnexpectedThunks (Seq a) where
  showTypeOf _ = "Seq"
  whnfNoUnexpectedThunks ctxt = noUnexpectedThunksInValues ctxt . toList

instance NoUnexpectedThunks a => NoUnexpectedThunks (Ratio a) where
  showTypeOf _ = "Ratio"
  whnfNoUnexpectedThunks ctxt r = noUnexpectedThunksInValues ctxt [n, d]
   where
     -- The 'Ratio' constructor is not exported: we only have two accessor
     -- functions. However, @numerator r@ is obviously trivially a trunk
     -- (due to the unevaluated call to @numerator@). By forcing the values of
     -- @n@ and @d@ where we get rid of these function calls, leaving only the
     -- values inside the @Ratio@. Note that @Ratio@ is strict in both of these
     -- fields, so forcing them to WHNF won't change them.
     !n = numerator   r
     !d = denominator r

{-------------------------------------------------------------------------------
  Type level symbol comparison logic
-------------------------------------------------------------------------------}

type family EqSymbol s t where
  EqSymbol s t = IsEq (CmpSymbol s t)

type family IsEq (o :: Ordering) where
  IsEq 'EQ = 'True
  IsEq _x   = 'False

type family Or (a :: Bool) (b :: Bool) where
  Or 'False 'False = 'False
  Or _a _b = 'True

type family ElemSymbol (s :: Symbol) (xs :: [Symbol]) where
  ElemSymbol s (x ': xs) = Or (EqSymbol s x) (ElemSymbol s xs)
  ElemSymbol _s '[] = 'False
