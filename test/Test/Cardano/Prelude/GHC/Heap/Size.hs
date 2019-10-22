{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-full-laziness #-}

module Test.Cardano.Prelude.GHC.Heap.Size (tests) where

import Cardano.Prelude hiding (diff)

import GHC.Exts.Heap.Constants
import GHC.Prim (Int#)
import qualified Data.ByteString as BS.Strict

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

{-------------------------------------------------------------------------------
  Various unboxed tuples, for testing purposes

  We don't use an UNPACK pragma here because that is ignored at @-O0@, which
  could result in false negatives.
-------------------------------------------------------------------------------}

data Ints2 = Ints2 Int# Int#
data Ints3 = Ints3 Int# Int# Int#
data Ints4 = Ints4 Int# Int# Int# Int#
data Ints5 = Ints5 Int# Int# Int# Int# Int#

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

newtype NumWords = NumWords Word64
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Num)

-- | Size of a word (in bytes)
wordSize :: Word64
wordSize = fromIntegral wORD_SIZE

-- | Divide @n@ by @m@, rounding up if necessary
--
-- > divRoundUp 10 2 == 5
-- > divRoundUp 10 3 == 4
divRoundUp :: Integral a => a -> a -> a
divRoundUp n m = ceiling (n' / m')
  where
    n', m' :: Double
    n' = fromIntegral n
    m' = fromIntegral m

-- | Estimate the size of a strict bytestring
--
-- Strict 'ByteString' is defined as
--
-- > data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
-- >                      {-# UNPACK #-} !Int                -- offset
-- >                      {-# UNPACK #-} !Int                -- length
--
-- where
--
-- > data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents
-- > data ForeignPtrContents
-- >   = PlainForeignPtr !(IORef Finalizers)
-- >   | MallocPtr      (MutableByteArray# RealWorld) !(IORef Finalizers)
-- >   | PlainPtr       (MutableByteArray# RealWorld)
--
-- The UNPACK pragma on the 'ForeignPtr' means that the @Addr#@ from
-- 'ForeignPtr' will be inlined in the 'PS' constructor. This means we have a
-- total of 5 words for the 'PS' constructor (one for the info table pointer and
-- 4 for the arguments). Similarly, we'll have 2-word size for 'ForeignPtr' and
-- another 2-word size for the 'ForeignPtrContents'  (for regularly constructed
-- bytestring only uses 'PlainPtr').
--
-- Since the payload itself will be allocated rounded to the nearest word, a
-- bytestring of @n@ elements is @5 + 2 + 2 + ceiling (n / wordSize)@ words.
bsSize :: Word64 -> NumWords
bsSize numElems = NumWords (5 + 2 + 2 + numElems `divRoundUp` wordSize)

-- | Check that the reported size is within the specified bounds
--
-- Bounds are in words.
verifySize :: NumWords -> a -> Property
verifySize (NumWords expected) !x = withTests 1 $ property $ do
    annotate (show wordSize)
    sz <- liftIO $ computeHeapSize x
    sz === Right expected

-- | Wrapper around 'BS.Strict.pack' which cannot be inlined, to avoid
-- some parts of bytestrings being duplicated rather than shared
mkBS :: [Word8] -> ByteString
{-# NOINLINE mkBS #-}
mkBS = BS.Strict.pack

{-------------------------------------------------------------------------------
  Auxiliary: binary trees
-------------------------------------------------------------------------------}

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving (Show, Generic, NFData)

genBalancedTree :: forall a. Int -> Gen a -> Gen (Tree a)
genBalancedTree height genA = go height
  where
    go :: Int -> Gen (Tree a)
    go 0 = return Leaf
    go n = Branch <$> go (n - 1) <*> genA <*> go (n - 1)

heightOf :: Tree a -> Word64
heightOf Leaf           = 0
heightOf (Branch l _ r) = 1 + max (heightOf l) (heightOf r)

{-------------------------------------------------------------------------------
  Tests for 'computeHeapSizeWorkList'
-------------------------------------------------------------------------------}

prop_WorkList_ListInt :: Property
prop_WorkList_ListInt = property $ do
    xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
    (computeHeapSizeWorkList $! force xs) === 2

prop_WorkList_TreeInt :: Property
prop_WorkList_TreeInt = property $ do
    n  <- forAll $ Gen.int (Range.linear 2 12)
    xs <- forAll $ genBalancedTree n (Gen.int (Range.constantBounded))
    (computeHeapSizeWorkList $! force xs) === (heightOf xs * 2) + 1

{-------------------------------------------------------------------------------
  Tests proper

  We avoid using Double, as that will fit into a single machine word on 64-bit
  machines but not on 32-bit machines; the bytestring tests check boundary cases
  around the payload (which is rounded to the nearest word), as well as what
  happens when we /share/ bytestrings in a tuple.

  For now these are unit tests not property tests; setting up property tests
  would be a fairly delicate task as we'd have to know a lot about type
  internals as well as the precise sharing.

  Note that for nullary constructors, the size is two words, because closures
  have a minimum closure size. See for instance
  <https://mail.haskell.org/pipermail/ghc-devs/2019-February/017030.html>
-------------------------------------------------------------------------------}

prop_Int     = verifySize 2 (1    :: Int)
prop_Float   = verifySize 2 (1.0  :: Float)
prop_Bool    = verifySize 2 (True :: Bool)
prop_Ints2   = verifySize 3 $ Ints2 1# 2#
prop_Ints3   = verifySize 4 $ Ints3 1# 2# 3#
prop_Ints4   = verifySize 5 $ Ints4 1# 2# 3# 4#
prop_Ints5   = verifySize 6 $ Ints5 1# 2# 3# 4# 5#

prop_Bs103   = verifySize (bsSize 103) $ mkBS [1 .. 103]
prop_Bs104   = verifySize (bsSize 104) $ mkBS [1 .. 104]
prop_Bs105   = verifySize (bsSize 105) $ mkBS [1 .. 105]

-- These tuples share a bytestring, so we have the size of the bytestring plus
-- the size of the tuple, which is the iptr plus the fields of the tuple.

prop_2_Bs106 = verifySize (1 + 2 + bsSize 106) $ let !x = mkBS [1 .. 106] in (x, x)
prop_3_Bs107 = verifySize (1 + 3 + bsSize 107) $ let !x = mkBS [1 .. 107] in (x, x, x)
prop_4_Bs108 = verifySize (1 + 4 + bsSize 108) $ let !x = mkBS [1 .. 108] in (x, x, x, x)
prop_5_Bs109 = verifySize (1 + 5 + bsSize 109) $ let !x = mkBS [1 .. 109] in (x, x, x, x, x)

-- Unsupported closure type
prop_MVar = withTests 1 $ property $ do
    mvar <- liftIO $ newMVar ()
    sz <- liftIO $ computeHeapSize mvar
    case sz of
      Left (UnsupportedClosure _) -> return ()
      _otherwise -> failure

-- Compute size of a list with minimum worklist and visited set size
--
-- We run this test repeatedly, to flush out any potential non-determinism
-- in the C code.
prop_list = withTests 1000 $ property $ do
    sz <- liftIO $ computeHeapSize' 2 102 xs
    -- 100 cons nodes : each 3 words
    --   1 nil  node  : 2 words
    --   1 I#   nod e : 2 words
    sz === Right 304
  where
    xs :: [Int]
    !xs = force $ replicate 100 1

prop_list_visited_full = withTests 1 $ property $ do
    sz <- liftIO $ computeHeapSize' 2 101 xs
    sz === Left VisitedFull
  where
    xs :: [Int]
    !xs = force $ replicate 100 1

prop_list_worklist_full = withTests 1 $ property $ do
    sz <- liftIO $ computeHeapSize' 1 102 xs
    sz === Left WorkListFull
  where
    xs :: [Int]
    !xs = force $ replicate 100 1

-- Compute size of infinite list
prop_ones = withTests 1 $ property $ do
    let xs :: [Int]
        xs = 1 : xs
    sz <- liftIO $ computeHeapSize xs
    sz === Right 5

tests :: IO Bool
tests = and <$> sequence [checkParallel $$(discover)]
