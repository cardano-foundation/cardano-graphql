{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Helpers for round-trip testing datatypes

module Test.Cardano.Prelude.Tripping
  ( runTests
  , discoverPropArg
  , discoverRoundTrip
  , discoverRoundTripArg
  , roundTripsAesonShow
  , roundTripsAesonBuildable
  , roundTripsCanonicalJsonPretty
  , trippingBuildable
  )
where

import Cardano.Prelude

import Data.Aeson (FromJSON, ToJSON, decode, fromJSON, encode, toJSON)
import Data.String (String, unlines)
import qualified Data.Map as Map
import Data.Text.Internal.Builder (toLazyText)
import Formatting.Buildable (Buildable(..))
import System.IO (hSetEncoding, utf8)
import Text.Show.Pretty (Value(..), parseValue)
import qualified Text.JSON.Canonical as CanonicalJSON

import Hedgehog (Group(..), MonadTest, discoverPrefix, success, tripping)
import Hedgehog.Internal.Discovery
  (PropertySource, posLine, posPostion, propertySource, readProperties)
import Hedgehog.Internal.Property
  (Diff(..), GroupName(..), Property, PropertyName(..), failWith)
import Hedgehog.Internal.Show (valueDiff)
import Hedgehog.Internal.TH (TExpQ)

import Language.Haskell.TH (Exp(..), Q, location, runIO)
import Language.Haskell.TH.Syntax (Loc(..), mkName, unTypeQ, unsafeTExpCoerce)

discoverRoundTrip :: TExpQ Group
discoverRoundTrip = discoverPrefix "roundTrip"

discoverRoundTripArg :: TExpQ (a -> Group)
discoverRoundTripArg = discoverPrefixThreadArg "ts_roundTrip"

discoverPropArg :: TExpQ (a -> Group)
discoverPropArg = discoverPrefixThreadArg "ts_prop_"

-- | Lifted from `Hedgehog.Internal.TH.discoverPrefix` and tweaked.
-- This will find top level definitions of type `(a -> Property)` whose
-- variable names are prefixed with `prefix` . It returns a TH-monad-wrapped
-- `(a -> Group)` function which passes that `a`-typed argument to all of
-- the sub-properties, so that they are fully applied `Property`'s and can
-- be wrapped into a Hedgehog `Group`.
discoverPrefixThreadArg :: forall a . String -> TExpQ (a -> Group)
discoverPrefixThreadArg prefix = do
  file       <- getCurrentFile
  properties <- Map.toList <$> runIO (readProperties prefix file)

  let
    startLine :: (c, PropertySource) -> (c, PropertySource) -> Ordering
    startLine = comparing $ posLine . posPostion . propertySource . snd

    names     = fmap (mkNamedProperty . fst) $ sortBy startLine properties

  [|| \arg -> Group $$(testModuleName) (map ($ arg) $$(listTE names)) ||]
 where

  mkNamedProperty :: PropertyName -> TExpQ (a -> (PropertyName, Property))
  mkNamedProperty name = do
    [|| \arg -> (name, $$(unsafeProperty name) arg) ||]

  unsafeProperty :: PropertyName -> TExpQ (a -> Property)
  unsafeProperty pn = do
    let
      prop :: TExpQ (a -> Property)
      prop = unsafeTExpCoerce . pure . VarE . mkName $ unPropertyName pn
    [|| $$prop ||]

  listTE :: [TExpQ b] -> TExpQ [b]
  listTE xs = do
    unsafeTExpCoerce . pure . ListE =<< traverse unTypeQ xs

  testModuleName :: TExpQ GroupName
  testModuleName = do
    loc <- GroupName . loc_module <$> location
    [|| loc ||]

  getCurrentFile :: Q FilePath
  getCurrentFile = loc_filename <$> location


roundTripsAesonShow
  :: (Eq a, MonadTest m, ToJSON a, FromJSON a, Show a) => a -> m ()
roundTripsAesonShow a = tripping a toJSON fromJSON

-- | Round trip any `a` with both `ToJSON` and `FromJSON` instances.
roundTripsAesonBuildable
  :: (Eq a, MonadTest m, ToJSON a, FromJSON a, Buildable a) => a -> m ()
roundTripsAesonBuildable a = trippingBuildable a encode decode

-- | Pretty round trip any `a` with both `ToJSON` and `FromJSON` canonical instances.
roundTripsCanonicalJsonPretty
  :: ( Eq a
     , Show a
     , MonadTest m
     , CanonicalJSON.ToJSON Identity a
     , CanonicalJSON.FromJSON (Either SchemaError) a
     )
  => a
  -> m ()
roundTripsCanonicalJsonPretty a = tripping a canonicalEncodePretty canonicalDecodePretty

runTests :: [IO Bool] -> IO ()
runTests tests' = do
  -- ensure UTF-8. As that's what hedgehog needs.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  result <- and <$> sequence tests'
  unless result exitFailure

-- | Round trip using given encode and decode functions for types with a
--   'Buildable' instance
trippingBuildable
  :: forall f a b m
   . ( HasCallStack
     , Buildable (f a)
     , Eq (f a)
     , Show b
     , Applicative f
     , MonadTest m
     )
  => a
  -> (a -> b)
  -> (b -> f a)
  -> m ()
trippingBuildable x enc dec =
  let
    mx :: f a
    mx = pure x
    i  = enc x
    my = dec i
  in if mx == my
    then success
    else case valueDiff <$> buildValue mx <*> buildValue my of
      Nothing -> withFrozenCallStack $ failWith Nothing $ Data.String.unlines
        [ "━━━ Original ━━━"
        , show $ buildValue mx
        , "━━━ Intermediate ━━━"
        , show i
        , "━━━ Roundtrip ━━━"
        , show $ buildValue my
        ]

      Just dif ->
        withFrozenCallStack
          $ failWith
              (Just $ Diff "━━━ " "- Original" "/" "+ Roundtrip" " ━━━" dif)
          $ Data.String.unlines ["━━━ Intermediate ━━━", show i]

instance (Buildable e, Buildable a) => Buildable (Either e a) where
  build (Left  e) = build e
  build (Right a) = build a

buildValue :: Buildable a => a -> Maybe Value
buildValue = parseValue . toS . toLazyText . build
