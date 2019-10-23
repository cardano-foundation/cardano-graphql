{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE UnliftedFFITypes    #-}

module Cardano.Prelude.GHC.Heap.Size (
    CountFailure(..)
  , PerformGC(..)
  , computeHeapSize
  , computeHeapSize'
  , computeHeapSizeWorkList
  ) where

import Cardano.Prelude.Base hiding (Any)

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.StablePtr
import Foreign.Storable
import GHC.Exts.Heap.ClosureTypes (ClosureType)
import GHC.Prim
import GHC.Types
import System.Mem (performMajorGC)

{-------------------------------------------------------------------------------
  Failure
-------------------------------------------------------------------------------}

cNO_FAILURE, cWORK_LIST_FULL, cVISITED_FULL, cOUT_OF_MEMORY, cUNSUPPORTED_CLOSURE :: CUInt

cNO_FAILURE          = 0
cWORK_LIST_FULL      = 1
cVISITED_FULL        = 2
cOUT_OF_MEMORY       = 3
cUNSUPPORTED_CLOSURE = 4

data CountFailure =
    WorkListFull
  | VisitedFull
  | OutOfMemory
  | UnsupportedClosure ClosureType
  deriving (Show, Eq)

toCountFailure :: CUInt -> Maybe CountFailure
toCountFailure n
  | n == cNO_FAILURE          = Nothing
  | n == cWORK_LIST_FULL      = Just $ WorkListFull
  | n == cVISITED_FULL        = Just $ VisitedFull
  | n == cOUT_OF_MEMORY       = Just $ OutOfMemory
  | n >= cUNSUPPORTED_CLOSURE = Just $ UnsupportedClosure typ
  | otherwise = panic "getCountFailure: impossible"
  where
    typ :: ClosureType
    typ = toEnum (fromIntegral (n - cUNSUPPORTED_CLOSURE))

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Bind to the C function to count the closure size
--
-- It is crucial that this function is marked unsafe, because GHC guarantees
-- that garbage collection will not occur during an unsafe call
-- (see <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#guaranteed-call-safety>),
-- which is crucial for reliable counting (Haskell's GC moves objects around,
-- and so if a GC occurred during counting, we might end up counting objects
-- more than once or indeed miss them in the count if they happen to be moved to
-- a location we marked as already visited. This is also the reason that we
-- count the size of the object in C rather than in Haskell.
foreign import ccall unsafe "hs_cardanoprelude_closureSize"
  closureSize_ :: CUInt -> CUInt -> CUInt -> Ptr CUInt -> StablePtr a -> IO CULong

-- | Should we perform a GC call before counting the size?
data PerformGC =
    -- | Yes, first perform GC before counting
    --
    -- This should be used for most accurate results. Without calling GC first,
    -- the computed size might be larger than expected due to leftover
    -- indirections (black holes, selector thunks, etc.)
    FirstPerformGC

    -- | No, do not perform GC before counting
    --
    -- If pinpoint accuracy is not requried, then GC can be skipped, making the
    -- call much less expensive.
  | DontPerformGC

-- | Wrapper around 'closureSize_' that takes care of creating the stable ptr
--
-- We can't simply pass the address of the closure to the C function, because
-- we have no guarantee that GC will not happen in between taking that address
-- and the C call. We therefore create and pass a stable pointer instead.
closureSize :: PerformGC -> CUInt -> CUInt -> CUInt -> Ptr CUInt -> a -> IO CULong
closureSize performGC
            workListCapacity
            visitedInitCapacity
            visitedMaxCapacity
            err
            a
          = do
    case performGC of
      FirstPerformGC -> performMajorGC
      DontPerformGC  -> return ()
    bracket (newStablePtr a) freeStablePtr $ \stablePtr ->
      closureSize_ workListCapacity
                   visitedInitCapacity
                   visitedMaxCapacity
                   err
                   stablePtr

-- | Compute the size of the given closure
--
-- The size of the worklist should be set to the maximum expected /depth/ of
-- the closure; the size of the visited set should be set to the maximum /number
-- of nodes/ in the closure.
--
-- 'computeHeapSizeWorkList' can be used to estimate the size of the worklist
-- required.
computeHeapSize' :: PerformGC -- ^ Should we call GC before counting?
                 -> Word      -- ^ Capacity of the worklist
                 -> Word      -- ^ Initial capacity of the visited set
                 -> Word      -- ^ Maximum capacity of the visited set
                 -> a -> IO (Either CountFailure Word64)
computeHeapSize' performGC
                 workListCapacity
                 visitedInitCapacity
                 visitedMaxCapacity
                 a
               = do
    alloca $ \(err :: Ptr CUInt) -> do
      size     <- closureSize performGC
                              workListCapacity'
                              visitedInitCapacity'
                              visitedMaxCapacity'
                              err
                              a
      mFailure <- toCountFailure <$> peek err
      return $ case mFailure of
                 Just failure -> Left failure
                 Nothing      -> Right (fromIntegral size)
  where
    workListCapacity', visitedInitCapacity', visitedMaxCapacity' :: CUInt
    workListCapacity'    = fromIntegral workListCapacity
    visitedInitCapacity' = fromIntegral visitedInitCapacity
    visitedMaxCapacity'  = fromIntegral visitedMaxCapacity

-- | Compute the size of the given closure
--
-- This is a wrapper around 'computeHeapSize'' which sets some defaults for the
-- capacity of worklist and the visited set: it uses a worklist capacity of 10k
-- (which, assuming balanced data structures, should be more than enough), an
-- initial visited set capacity of 250k, and a maximum visited set capacity of
-- 16M. This means that this will use between 2 MB and 128 MB of heaps space.
--
-- It also does NOT perform GC before counting, for improved performance.
-- Client code can call 'performMajorGC' manually or use 'computeHeapSize''.
 --
-- Should these limits not be sufficient, or conversely, the memory requirements
-- be too large, use 'computeHeapSize'' directly.
computeHeapSize :: a -> IO (Either CountFailure Word64)
computeHeapSize =
   computeHeapSize' DontPerformGC
                    workListCapacity
                    visitedInitCapacity
                    visitedMaxCapacity
  where
    -- Memory usage assuming 64-bit (i.e. 8 byte) pointers
    workListCapacity, visitedInitCapacity, visitedMaxCapacity :: Word
    workListCapacity    =        10 * 1000 --  80 kB
    visitedInitCapacity =       250 * 1000 --   2 MB
    visitedMaxCapacity  = 16 * 1000 * 1000 -- 128 MB

{-------------------------------------------------------------------------------
  Compute the depth of the closure
-------------------------------------------------------------------------------}

-- | Upper bound on the required work list size to compute closure size
--
-- NOTE: This ignores sharing, and so provides an upper bound only.
--
-- The size of a closure with no nested pointers can be computed without any
-- stack space.
--
-- When we have a closure with @(N + 1)@ nested pointers
--
-- > p0 p1 .. pN
--
-- We will
--
-- * Push @pN, .., p1, p0@ onto the stack
-- * Pop off @p0@ and count its children
-- * Pop off @p1@ and count its children
-- * ..
--
-- until we have processed all children. This means that the stack space
-- required will be the maximum of
--
-- > [ N + 1 -- For the initial list
-- > , requiredWorkList p0 + (N + 1) - 1
-- > , requiredWorkList p1 + (N + 1) - 2
-- > , ..
-- > , requiredWorkList pN + (N + 1) - (N + 1)
-- > ]
--
-- For example, for a list, we would get that
--
-- > requiredWorkList []     == 0
-- > requiredWorkList (x:xs) == max [ 2
-- >                                , requiredWorkList x + 1
-- >                                , requiredWorkList xs
-- >                                ]
--
-- which, for a list of @Int@ (which requires only a stack of size 1), equals 2
-- (unless the list is empty).
--
-- Similarly, for binary trees, we get
--
-- > requiredWorkList Leaf           == 0
-- > requiredWorkList (Branch l x r) == max [ 3
-- >                                        , requiredWorkList l + 2
-- >                                        , requiredWorkList x + 1
-- >                                        , requiredWorkList r
-- >                                        ]
--
-- which, for a tree of @Int@, is bound by @(height * 2) + 1@.
computeHeapSizeWorkList :: a -> Word64
computeHeapSizeWorkList a =
    maximum $ fromIntegral (I# (sizeofArray# ptrs))
            : map nested (collect [] 0#)
  where
    ptrs :: Array# Any
    !(# _addr, _raw, ptrs #) = unpackClosure# a

    -- Recursive worklist size of nested pointer @p@, with additional stack @n@
    nested :: (Any, Word64) -> Word64
    nested (p, n) = computeHeapSizeWorkList p + n

    -- @collect [] 0@ will construct the sequence
    --
    -- > [ (p0, (N + 1) - 1)
    -- > , (p1, (N + 1) - 2)
    -- > , ..
    -- > , (pN, (N + 1) - (N + 1))
    -- > ]
    collect :: [(Any, Word64)] -> Int# -> [(Any, Word64)]
    collect acc ix =
        case ix <# sizeofArray# ptrs of
          0# -> acc
          _  -> let n :: Word64
                    !n = fromIntegral (I# (sizeofArray# ptrs -# (ix +# 1#)))
                in case indexArray# ptrs ix of
                     (# p #) -> collect ((p, n) : acc) (ix +# 1#)
