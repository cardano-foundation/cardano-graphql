{-# LANGUAGE FlexibleInstances #-}

module Cardano.Prelude.HeapWords
  ( HeapWords(..)
  , heapSizeMb
  , heapSizeKb
  , heapWords0
  , heapWords1
  , heapWords2
  , heapWords3
  , heapWords4
  , heapWords5
  , heapWords6
  , heapWords7
  , heapWords8
  , heapWords9
  , heapWords10
  , heapWords11
  , heapWords12
  , heapWords13
  , heapWordsUArray
  , heapWordsUVector
  )
where

import Cardano.Prelude.Base

import qualified Data.Array.Unboxed as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as BSS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Ix
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Time (Day, UTCTime)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as V.U


--------------------------------------------------------------------------------
-- HeapWords class and instances
--------------------------------------------------------------------------------

-- | These functions assume a 64-bit architecture
heapSizeMb, heapSizeKb :: Int -> Int
heapSizeMb w = wordSize * w `div` (1024 * 1024)
heapSizeKb w = wordSize * w `div` 1024

wordSize :: Int
wordSize = 8

-- | Size in the heap of values, in words (multiply by 4 on a 32-bit machine or
--   8 on a 64-bit machine)
class HeapWords a where
  heapWords :: a -> Int

heapWords0 :: Int
heapWords1 :: HeapWords a => a -> Int
heapWords2 :: (HeapWords a1, HeapWords a) => a -> a1 -> Int
heapWords3 :: (HeapWords a2, HeapWords a1, HeapWords a) => a -> a1 -> a2 -> Int
heapWords4
  :: (HeapWords a3, HeapWords a2, HeapWords a1, HeapWords a)
  => a
  -> a1
  -> a2
  -> a3
  -> Int
heapWords5
  :: (HeapWords a4, HeapWords a3, HeapWords a2, HeapWords a1, HeapWords a)
  => a
  -> a1
  -> a2
  -> a3
  -> a4
  -> Int
heapWords6
  :: ( HeapWords a5
     , HeapWords a4
     , HeapWords a3
     , HeapWords a2
     , HeapWords a1
     , HeapWords a
     )
  => a
  -> a1
  -> a2
  -> a3
  -> a4
  -> a5
  -> Int
heapWords7
  :: ( HeapWords a6
     , HeapWords a5
     , HeapWords a4
     , HeapWords a3
     , HeapWords a2
     , HeapWords a1
     , HeapWords a
     )
  => a
  -> a1
  -> a2
  -> a3
  -> a4
  -> a5
  -> a6
  -> Int
heapWords8
  :: ( HeapWords a7
     , HeapWords a6
     , HeapWords a5
     , HeapWords a4
     , HeapWords a3
     , HeapWords a2
     , HeapWords a1
     , HeapWords a
     )
  => a
  -> a1
  -> a2
  -> a3
  -> a4
  -> a5
  -> a6
  -> a7
  -> Int
heapWords9
  :: ( HeapWords a8
     , HeapWords a7
     , HeapWords a6
     , HeapWords a5
     , HeapWords a4
     , HeapWords a3
     , HeapWords a2
     , HeapWords a1
     , HeapWords a
     )
  => a
  -> a1
  -> a2
  -> a3
  -> a4
  -> a5
  -> a6
  -> a7
  -> a8
  -> Int
heapWords10
  :: ( HeapWords a9
     , HeapWords a8
     , HeapWords a7
     , HeapWords a6
     , HeapWords a5
     , HeapWords a4
     , HeapWords a3
     , HeapWords a2
     , HeapWords a1
     , HeapWords a
     )
  => a
  -> a1
  -> a2
  -> a3
  -> a4
  -> a5
  -> a6
  -> a7
  -> a8
  -> a9
  -> Int
heapWords11
  :: ( HeapWords a10
     , HeapWords a9
     , HeapWords a8
     , HeapWords a7
     , HeapWords a6
     , HeapWords a5
     , HeapWords a4
     , HeapWords a3
     , HeapWords a2
     , HeapWords a1
     , HeapWords a
     )
  => a
  -> a1
  -> a2
  -> a3
  -> a4
  -> a5
  -> a6
  -> a7
  -> a8
  -> a9
  -> a10
  -> Int
heapWords12
  :: ( HeapWords a11
     , HeapWords a10
     , HeapWords a9
     , HeapWords a8
     , HeapWords a7
     , HeapWords a6
     , HeapWords a5
     , HeapWords a4
     , HeapWords a3
     , HeapWords a2
     , HeapWords a1
     , HeapWords a
     )
  => a
  -> a1
  -> a2
  -> a3
  -> a4
  -> a5
  -> a6
  -> a7
  -> a8
  -> a9
  -> a10
  -> a11
  -> Int
heapWords13
  :: ( HeapWords a12
     , HeapWords a11
     , HeapWords a10
     , HeapWords a9
     , HeapWords a8
     , HeapWords a7
     , HeapWords a6
     , HeapWords a5
     , HeapWords a4
     , HeapWords a3
     , HeapWords a2
     , HeapWords a1
     , HeapWords a
     )
  => a
  -> a1
  -> a2
  -> a3
  -> a4
  -> a5
  -> a6
  -> a7
  -> a8
  -> a9
  -> a10
  -> a11
  -> a12
  -> Int


heapWords0 = 0
heapWords1 a = 2 + heapWords a
heapWords2 a b = 3 + heapWords a + heapWords b
heapWords3 a b c = 4 + heapWords a + heapWords b + heapWords c
heapWords4 a b c d = 5 + heapWords a + heapWords b + heapWords c + heapWords d
heapWords5 a b c d e =
  6 + heapWords a + heapWords b + heapWords c + heapWords d + heapWords e
heapWords6 a b c d e f =
  7
    + heapWords a
    + heapWords b
    + heapWords c
    + heapWords d
    + heapWords e
    + heapWords f
heapWords7 a b c d e f g =
  8
    + heapWords a
    + heapWords b
    + heapWords c
    + heapWords d
    + heapWords e
    + heapWords f
    + heapWords g
heapWords8 a b c d e f g h =
  9
    + heapWords a
    + heapWords b
    + heapWords c
    + heapWords d
    + heapWords e
    + heapWords f
    + heapWords g
    + heapWords h
heapWords9 a b c d e f g h i =
  10
    + heapWords a
    + heapWords b
    + heapWords c
    + heapWords d
    + heapWords e
    + heapWords f
    + heapWords g
    + heapWords h
    + heapWords i
heapWords10 a b c d e f g h i j =
  11
    + heapWords a
    + heapWords b
    + heapWords c
    + heapWords d
    + heapWords e
    + heapWords f
    + heapWords g
    + heapWords h
    + heapWords i
    + heapWords j
heapWords11 a b c d e f g h i j k =
  12
    + heapWords a
    + heapWords b
    + heapWords c
    + heapWords d
    + heapWords e
    + heapWords f
    + heapWords g
    + heapWords h
    + heapWords i
    + heapWords j
    + heapWords k
heapWords12 a b c d e f g h i j k l =
  13
    + heapWords a
    + heapWords b
    + heapWords c
    + heapWords d
    + heapWords e
    + heapWords f
    + heapWords g
    + heapWords h
    + heapWords i
    + heapWords j
    + heapWords k
    + heapWords l
heapWords13 a b c d e f g h i j k l m =
  14
    + heapWords a
    + heapWords b
    + heapWords c
    + heapWords d
    + heapWords e
    + heapWords f
    + heapWords g
    + heapWords h
    + heapWords i
    + heapWords j
    + heapWords k
    + heapWords l
    + heapWords m


instance HeapWords (a -> b) where
  heapWords _ = 0

instance HeapWords Int where
  heapWords _ = 2

instance HeapWords Word where
  heapWords _ = 2

instance HeapWords Word8 where
  heapWords _ = 2

instance HeapWords Word32 where
  heapWords _ = 2

instance HeapWords Word64 where
  heapWords _ = 2

instance HeapWords Char where
  heapWords _ = 0

instance HeapWords Bool where
  heapWords _ = 0

instance HeapWords Integer where
  heapWords _ = 2

instance HeapWords Float where
  heapWords _ = 2

instance HeapWords UTCTime where
  heapWords _ = 7

instance HeapWords Day where
  heapWords _ = 2

instance HeapWords a => HeapWords [a] where
  heapWords []     = heapWords0
  heapWords (x:xs) = heapWords2 x xs
--   heapWords xs = 2 + length xs + sum (map heapWords xs)

instance (HeapWords a, HeapWords b) => HeapWords (a,b) where
  heapWords (a,b) = heapWords2 a b

instance (HeapWords a, HeapWords b, HeapWords c) => HeapWords (a,b,c) where
  heapWords (a,b,c) = heapWords3 a b c

instance (HeapWords a, HeapWords b, HeapWords c, HeapWords d) => HeapWords (a,b,c,d) where
  heapWords (a,b,c,d) = heapWords4 a b c d

instance HeapWords a => HeapWords (Maybe a) where
  heapWords Nothing  = heapWords0
  heapWords (Just a) = heapWords1 a

instance (HeapWords a, HeapWords b) => HeapWords (Either a b) where
  heapWords (Left  a) = heapWords1 a
  heapWords (Right b) = heapWords1 b

instance (HeapWords a, HeapWords b) => HeapWords (Map a b) where
  heapWords m = sum [ 6 + heapWords k + heapWords v | (k,v) <- Map.toList m ]

instance HeapWords a => HeapWords (IntMap a) where
  heapWords m = sum [ 8 + heapWords v | v <- IntMap.elems m ]

instance HeapWords a => HeapWords (Set a) where
  heapWords m = sum [ 5 + heapWords v | v <- Set.elems m ]

instance HeapWords IntSet where
  heapWords s = 4 * IntSet.size s --estimate

instance HeapWords a => HeapWords (Seq a) where
  heapWords s = sum [ 5 + heapWords v | v <- toList s ] --estimate

instance HeapWords ByteString where
  heapWords s = let (w,t) = divMod (BS.length s) wordSize
               in 5 + w + signum t

instance HeapWords BSS.ShortByteString where
  heapWords s = let (w,t) = divMod (BSS.length s) wordSize
               in 1 + w + signum t

instance HeapWords LByteString where
  heapWords s = sum [ 1 + heapWords c | c <- LBS.toChunks s ]

instance HeapWords Text where
  heapWords s = let (w,t) = divMod (length s) (wordSize `div` 2)
               in 5 + w + signum t

heapWordsUArray :: (Ix i, A.IArray a e) => Int -> a i e -> Int
heapWordsUArray sz a = 13 + (rangeSize (A.bounds a) * sz) `div` wordSize

instance HeapWords e => HeapWords (V.Vector e) where
  heapWords a = 5 + V.length a + V.foldl' (\s e -> s + heapWords e) 0 a

heapWordsUVector :: V.U.Unbox e => Int -> V.U.Vector e -> Int
heapWordsUVector sz a = 5 + (V.U.length a * sz) `div` wordSize
