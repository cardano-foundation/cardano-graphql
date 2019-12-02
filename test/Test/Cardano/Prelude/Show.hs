{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Prelude.Show (
  tests,
  ) where

import qualified Prelude

import           GHC.Generics (Generic)

import Hedgehog
  ( Property
  , (===)
  , checkParallel
  , discover
  , property
  , withTests
  )

import           Cardano.Prelude

-----

prop_T1 :: Property
prop_T1 = withTests 1 $ property $
  Prelude.show (T1 7) === "T1 7"

newtype T1 = T1 {t1Int :: Int}
  deriving (Generic)

instance Prelude.Show T1 where
  showsPrec = gshowsPrecWithoutRecordSyntax

-----

prop_T2 :: Property
prop_T2 = withTests 1 $ property $
  Prelude.show (T2 1 'a') === "T2 1 'a'"

data T2 = T2 {t2Int :: Int, t2Char :: Char}
  deriving (Generic)

instance Prelude.Show T2 where
  showsPrec = gshowsPrecWithoutRecordSyntax

-----

prop_T3A :: Property
prop_T3A = withTests 1 $ property $
  Prelude.show (T3A 1 'b') === "T3A 1 'b'"

prop_T3B :: Property
prop_T3B = withTests 1 $ property $
  Prelude.show (T3B 'x' 3) === "T3B 'x' 3"

data T3
  = T3A {t3Int :: Int, t3Char :: Char}
  | T3B {t3Char :: Char, t3Int :: Int}
  deriving (Generic)

instance Prelude.Show T3 where
  showsPrec = gshowsPrecWithoutRecordSyntax

----- last, so TemplateHaskell sees all prop_*

tests :: IO Bool
tests = and <$> sequence [checkParallel $$(discover)]
