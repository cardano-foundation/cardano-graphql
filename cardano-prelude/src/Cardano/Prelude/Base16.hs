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

import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Base16 as B16

{- HLINT ignore "Use isDigit" -}

newtype Base16ParseError =
  Base16IncorrectSuffix ByteString
  deriving (Eq, Show)

instance Buildable Base16ParseError where
  build (Base16IncorrectSuffix suffix) =
    bprint ("Base16 parsing failed with incorrect suffix " . shown) suffix

parseBase16 :: Text -> Either Base16ParseError ByteString
parseBase16 t = first (const (Base16IncorrectSuffix bs)) (B16.decode bs)
  where bs = Text.encodeUtf8 t

decodeEitherBase16 :: ByteString -> Either String ByteString
decodeEitherBase16 = B16.decode
