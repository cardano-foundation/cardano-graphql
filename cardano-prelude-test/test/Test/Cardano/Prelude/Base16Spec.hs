{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Prelude.Base16Spec
  ( tests
  ) where

import Cardano.Prelude
import Hedgehog ((===), Property, Range, MonadGen)

import qualified Hedgehog as H
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import qualified Data.ByteString.Base16 as B16
import qualified Cardano.Prelude.Base16.Internal as I
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

{- HLINT ignore "Reduce duplication" -}

genByteString :: MonadGen m => Range Int -> m Word8 -> m ByteString
genByteString r g = BS.pack <$> G.list r g

prop_roundtrip :: Property
prop_roundtrip = H.withTests 100 . H.property $ do
  bs <- H.forAll $ genByteString (R.linear 0 10) (G.word8 R.constantBounded)
  b16 <- H.forAll . pure $ B16.encode bs
  I.decode b16 === (bs, "")

prop_noRegressionsValid :: Property
prop_noRegressionsValid = H.withTests 100 . H.property $ do
  bs <- H.forAll $ genByteString (R.linear 0 10) (G.word8 R.constantBounded)
  b16 <- H.forAll . pure $ B16.encode bs
  I.decode b16 === B16.decode b16

prop_noRegressionsInvalidSuffix :: Property
prop_noRegressionsInvalidSuffix = H.withTests 100 . H.property $ do
  bs <- H.forAll $ genByteString (R.linear 0 10) (G.word8 R.constantBounded)
  suffix <- H.forAll $ genByteString (R.linear 0 10) (G.element (BS.unpack (T.encodeUtf8 "qu")))
  b16 <- H.forAll . pure $ B16.encode bs <> suffix
  I.decode b16 === B16.decode b16

prop_decodeEitherBase16_examples :: Property
prop_decodeEitherBase16_examples = H.withTests 1 . H.property $ do
  decodeEitherBase16 "666f6f" === Right "foo"
  decodeEitherBase16 "66quux" === Left "invalid character at offset: 2"
  decodeEitherBase16 "666quux" === Left "invalid character at offset: 3"

tests :: IO Bool
tests = and <$> sequence [H.checkParallel $$(H.discover)]
