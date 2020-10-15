{-# LANGUAGE BangPatterns, MagicHash #-}

{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}

-- |
-- Module      : Data.ByteString.Base16
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base16-encoded strings.
--
-- This code is lifted directly from https://hackage.haskell.org/package/base16-bytestring-0.1.1.7/docs/src/Data.ByteString.Base16.html
-- and is intended to be temporary to facilitate migration from base16-bytestring-0.1.1.7 to base16-bytestring-1.0.0.0

module Cardano.Prelude.Base16.Internal
  ( decode
  ) where

import Data.Functor
import Data.Eq
import Data.Bool
import Data.Function
import Data.ByteString.Char8 (empty)
import Control.Monad
import Data.ByteString.Internal (ByteString(..), createAndTrim')
import Data.Bits (shiftL)
import Data.Ord
import GHC.Num
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Prim
import GHC.Types
import GHC.Word
import GHC.Real

-- | Decode a string from base16 form. The first element of the
-- returned tuple contains the decoded data. The second element starts
-- at the first invalid base16 sequence in the original string.
--
-- Examples:
--
-- > decode "666f6f"  == ("foo", "")
-- > decode "66quux"  == ("f", "quux")
-- > decode "666quux" == ("f", "6quux")
decode :: ByteString -> (ByteString, ByteString)
decode (PS sfp soff slen) =
  unsafePerformIO . createAndTrim' (slen `div` 2) $ \dptr ->
      withForeignPtr sfp $ \sptr ->
        dec (sptr `plusPtr` soff) dptr
 where
  dec sptr = go sptr where
    e = sptr `plusPtr` if odd slen then slen - 1 else slen
    go s d | s == e = let len = e `minusPtr` sptr
                      in return (0, len `div` 2, ps sfp (soff+len) (slen-len))
           | otherwise = do
      hi <- hex `fmap` peek8 s
      lo <- hex `fmap` peek8 (s `plusPtr` 1)
      if lo == 0xff || hi == 0xff
        then let len = s `minusPtr` sptr
             in return (0, len `div` 2, ps sfp (soff+len) (slen-len))
        else do
          poke d . fromIntegral $ lo + (hi `shiftL` 4)
          go (s `plusPtr` 2) (d `plusPtr` 1)

    hex (I# index) = W8# (indexWord8OffAddr# table index)
    !table =
        "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\
        \\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
        \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

peek8 :: Ptr Word8 -> IO Int
peek8 p = fromIntegral `fmap` peek p

ps :: ForeignPtr Word8 -> Int -> Int -> ByteString
ps fp off len
    | len <= 0 = empty
    | otherwise = PS fp off len
