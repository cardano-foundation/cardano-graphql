{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for parsing

module Cardano.Prelude.Base16
  ( Base16ParseError(..)

  , decodeEitherBase16
  , parseBase16
  )
where

import Cardano.Prelude.Base
import Data.String
import Formatting (bprint, shown)
import Formatting.Buildable (Buildable(build))

import qualified Cardano.Prelude.Base16.Internal as B16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as Text

{- HLINT ignore "Use isDigit" -}

newtype Base16ParseError =
  Base16IncorrectSuffix ByteString
  deriving (Eq, Show)

instance Buildable Base16ParseError where
  build (Base16IncorrectSuffix suffix) =
    bprint ("Base16 parsing failed with incorrect suffix " . shown) suffix

parseBase16 :: Text -> Either Base16ParseError ByteString
parseBase16 s = do
  let (bs, suffix) = B16.decode $ Text.encodeUtf8 s
  unless (BS.null suffix) . Left $ Base16IncorrectSuffix suffix
  pure bs

decodeEitherBase16 :: ByteString -> Either String ByteString
decodeEitherBase16 bs = case B16.decode bs of
  (decodedBs, "") -> Right decodedBs
  (_, _) -> Left $ "invalid character at offset: " <> show (BS.length (BS.takeWhile isHex bs))
  where isHex :: Char -> Bool
        isHex w =
          (w >= '0' && w <= '9') ||
          (w >= 'a' && w <= 'f') ||
          (w >= 'A' && w <= 'F')
