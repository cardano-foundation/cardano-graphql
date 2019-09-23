{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.Prelude.GHC.Heap.NormalForm.Classy (
    NoUnexpectedThunks(..)
  , allNoUnexpectedThunks
  , genericWhnfNoUnexpectedThunks
  , UseIsNormalForm(..)
  , ThunkInfo(..)
  , UnexpectedThunkInfo(..)
  , thunkInfoToIsNF
  , showTypeOfTypeable
  ) where

import Cardano.Prelude.Base

import Data.Foldable (toList)
import Data.Sequence (Seq)
import GHC.Exts.Heap
import Prelude (String)

import qualified Data.ByteString as BS.Strict
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text.Strict
import qualified Data.Text.Lazy as Text.Lazy

import Cardano.Prelude.GHC.Heap.Tree
import qualified Cardano.Prelude.GHC.Heap.NormalForm as NF

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
    , unexpectedThunkClosure   :: Text
    }
  deriving (Show)

-- | Was the value in normal form?
thunkInfoToIsNF :: ThunkInfo -> Bool
thunkInfoToIsNF NoUnexpectedThunks  = True
thunkInfoToIsNF (UnexpectedThunk _) = False

class NoUnexpectedThunks a where
  -- | Show type @a@ (to add to the context)
  --
  -- NOTE: This should be compositional. For example, 'showTypeOf' for @Seq a@
  -- should just show @"Seq"@, rather than for instance requiring @Typeable@ on
  -- @a@; the latter would not be compositional (in the sense that 'showTypeOf'
  -- on @a@ would be irrelevant). In general, we don't want to avoid @Typeable@
  -- constraints, because some things are instances of 'NoUnexpectedThunks' but
  -- not instances of 'Typeable'.
  showTypeOf :: Proxy a -> String
  default showTypeOf :: (Generic a, GShowTypeOf (Rep a)) => Proxy a -> String
  showTypeOf = showTypeOfGeneric

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
  noUnexpectedThunks :: HasCallStack => [String] -> a -> IO ThunkInfo
  noUnexpectedThunks ctxt =
      whenInHeadNormalForm (showTypeOf (Proxy @a) : ctxt) whnfNoUnexpectedThunks

  -- | Like 'noUnexpectedThunks', but can assume value is in WHNF
  whnfNoUnexpectedThunks :: [String] -> a -> IO ThunkInfo
  default whnfNoUnexpectedThunks :: (Generic a, GWhnfNoUnexpectedThunks (Rep a))
                                 => [String] -> a -> IO ThunkInfo
  whnfNoUnexpectedThunks = genericWhnfNoUnexpectedThunks

-- | Check if argument contains no unexpected thunks by converting to generic
-- representation first
--
-- NOTE: if it's in WHNF then we can safely translate to generic value without
-- forcing anything.
genericWhnfNoUnexpectedThunks :: forall a.
                                 (Generic a, GWhnfNoUnexpectedThunks (Rep a))
                              => [String] -> a -> IO ThunkInfo
genericWhnfNoUnexpectedThunks ctxt x =
    -- Force the result of @from@ to WHNF: we are not interested in thunks
    -- that arise from the translation to the generic representation.
    let fp :: Rep a x
        !fp = from x
    in gWhnfNoUnexpectedThunks ctxt fp

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

whenInHeadNormalForm :: HasCallStack
                     => [String]
                     -> ([String] -> a -> IO ThunkInfo)
                     -> (a -> IO ThunkInfo)
whenInHeadNormalForm ctxt k x = do
    c   <- getBoxedClosureData (asBox x)
    hnf <- NF.isHeadNormalForm c
    if hnf
      then k ctxt x
      else return $ UnexpectedThunk UnexpectedThunkInfo {
               unexpectedThunkContext   = ctxt
             , unexpectedThunkCallStack = callStack
             , unexpectedThunkClosure   = renderClosure c
             }

{-------------------------------------------------------------------------------
  Standard instances
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

deriving via UseIsNormalForm BS.Strict.ByteString instance NoUnexpectedThunks BS.Strict.ByteString
deriving via UseIsNormalForm BS.Lazy.ByteString   instance NoUnexpectedThunks BS.Lazy.ByteString

deriving via UseIsNormalForm Text.Strict.Text instance NoUnexpectedThunks Text.Strict.Text
deriving via UseIsNormalForm Text.Lazy.Text   instance NoUnexpectedThunks Text.Lazy.Text

-- | Instance for 'Seq' checks elements only
--
-- The internal fingertree in 'Seq' might have thunks, which is essential for
-- its asymptotic complexity.
instance NoUnexpectedThunks a => NoUnexpectedThunks (Seq a) where
  showTypeOf _ = "Seq"
  whnfNoUnexpectedThunks ctxt = allNoUnexpectedThunks
                              . map (noUnexpectedThunks ctxt)
                              . toList

-- | Instance for 'Map' checks elements only (Map is spine strict)
instance (NoUnexpectedThunks k, NoUnexpectedThunks v) => NoUnexpectedThunks (Map k v) where
  showTypeOf _ = "Map"
  whnfNoUnexpectedThunks ctxt = allNoUnexpectedThunks
                              . map (noUnexpectedThunks ctxt)
                              . Map.toList

-- | Instance for 'Set' checks elements only (Set is spine strict)
instance NoUnexpectedThunks a => NoUnexpectedThunks (Set a) where
  showTypeOf _ = "Set"
  whnfNoUnexpectedThunks ctxt = allNoUnexpectedThunks
                              . map (noUnexpectedThunks ctxt)
                              . Set.toList

-- | Instance for 'IntMap' checks elements only (IntMap is spine strict)
instance NoUnexpectedThunks a => NoUnexpectedThunks (IntMap a) where
  showTypeOf _ = "IntMap"
  whnfNoUnexpectedThunks ctxt = allNoUnexpectedThunks
                              . map (noUnexpectedThunks ctxt)
                              . IntMap.toList

-- | Instance for function closures is always 'True'
--
-- We could use 'isNormalForm' here, but this would (1) break compositionality
-- (what if the function closure contained a sequence?) and (2) 'isNormalForm'
-- doesn't seem to work so well for function closures anyway.
instance NoUnexpectedThunks (a -> b) where
  showTypeOf _ = "->"
  whnfNoUnexpectedThunks _ctxt _fun = return NoUnexpectedThunks

-- | See comments for @a -> b@
instance NoUnexpectedThunks (IO a) where
  showTypeOf _ = "IO"
  whnfNoUnexpectedThunks _ctxt _fun = return NoUnexpectedThunks

instance NoUnexpectedThunks a => NoUnexpectedThunks (Ratio a) where
  showTypeOf _ = "Ratio"
  whnfNoUnexpectedThunks ctxt r = do
     -- The 'Ratio' constructor is not exported: we only have two accessor
     -- functions. However, @numerator r@ is obviously trivially a trunk
     -- (due to the unevaluated call to @numerator@). By forcing the values of
     -- @n@ and @d@ where we get rid of these function calls, leaving on the
     -- values inside the @Ratio@. Note that @Ratio@ is strict in both of these
     -- fields, so forcing them to WHNF won't change them.
     let !n = numerator   r
         !d = denominator r
     allNoUnexpectedThunks [
         noUnexpectedThunks ctxt n
       , noUnexpectedThunks ctxt d
       ]

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
instance NoUnexpectedThunks a => NoUnexpectedThunks (Maybe a)

{-------------------------------------------------------------------------------
  Using the standard 'isNormalForm' check
-------------------------------------------------------------------------------}

newtype UseIsNormalForm a = UseIsNormalForm a

instance Typeable a => NoUnexpectedThunks (UseIsNormalForm a) where
  showTypeOf                = showTypeOfTypeable
  whnfNoUnexpectedThunks    = noUnexpectedThunks
  noUnexpectedThunks ctxt x = do
      c  <- getBoxedClosureData (asBox x)
      nf <- NF.isNormalForm x
      if nf
        then return $ NoUnexpectedThunks
        else return $ UnexpectedThunk UnexpectedThunkInfo {
                 unexpectedThunkContext   = ctxt'
               , unexpectedThunkCallStack = callStack
               , unexpectedThunkClosure   = renderClosure c
               }
    where
      ctxt' = showTypeOfTypeable (Proxy @a) : ctxt

{-------------------------------------------------------------------------------
  Generic instance
-------------------------------------------------------------------------------}

class GWhnfNoUnexpectedThunks f where
  gWhnfNoUnexpectedThunks :: [String] -> f x -> IO ThunkInfo

instance GWhnfNoUnexpectedThunks f => GWhnfNoUnexpectedThunks (M1 i c f) where
  gWhnfNoUnexpectedThunks ctxt (M1 fp) = gWhnfNoUnexpectedThunks ctxt fp

instance ( GWhnfNoUnexpectedThunks f
         , GWhnfNoUnexpectedThunks g
         ) => GWhnfNoUnexpectedThunks (f :*: g) where
  gWhnfNoUnexpectedThunks ctxt (fp :*: gp) = do
      allNoUnexpectedThunks [
          gWhnfNoUnexpectedThunks ctxt fp
        , gWhnfNoUnexpectedThunks ctxt gp
        ]

instance ( GWhnfNoUnexpectedThunks f
         , GWhnfNoUnexpectedThunks g
         ) => GWhnfNoUnexpectedThunks (f :+: g) where
  gWhnfNoUnexpectedThunks ctxt (L1 fp) = gWhnfNoUnexpectedThunks ctxt fp
  gWhnfNoUnexpectedThunks ctxt (R1 gp) = gWhnfNoUnexpectedThunks ctxt gp

instance NoUnexpectedThunks c => GWhnfNoUnexpectedThunks (K1 i c) where
  gWhnfNoUnexpectedThunks ctxt (K1 c) = noUnexpectedThunks ctxt' c
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

instance GWhnfNoUnexpectedThunks U1 where
  gWhnfNoUnexpectedThunks _ctxt U1 = return NoUnexpectedThunks

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

showTypeOfTypeable :: forall k proxy (a :: k). Typeable a => proxy a -> String
showTypeOfTypeable = show . typeRep

showTypeOfGeneric :: forall proxy a. (Generic a, GShowTypeOf (Rep a))
                  => proxy a -> String
showTypeOfGeneric _ = gShowTypeOf (from x)
  where
    x :: a
    x = x

class GShowTypeOf f where
  gShowTypeOf :: f x -> String

instance Datatype c => GShowTypeOf (D1 c f) where
  gShowTypeOf = datatypeName
