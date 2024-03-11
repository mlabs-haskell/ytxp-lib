{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Laws (aesonLaws, aesonLawsWith) where

import Data.Aeson (Encoding, FromJSON, Result (Error, Success),
                   ToJSON (toEncoding, toJSON), fromJSON)
import Data.Aeson.Encoding (encodingToLazyByteString, value)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutSmart, (<+>))
import Prettyprinter.Render.String (renderString)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, Property,
                        counterexample, forAllShrinkShow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)

-- | Verifies the laws that any valid type implementing both 'ToJSON' and
-- 'FromJSON' should follow. More precisely, this checks:
--
-- 1. Roundtripping: @'fromJSON' '.' 'toJSON'@ @=@ @'Success'@
-- 2. 'toJSON' and 'toEncoding' must agree: @'toEncoding'@ @=@ @'value' '.'
--    'toJSON'@
--
-- @since 0.1.0
aesonLaws :: forall (a :: Type) .
  (Typeable a, ToJSON a, FromJSON a, Arbitrary a, Pretty a, Eq a) =>
  TestTree
aesonLaws = aesonLawsWith @a arbitrary shrink

-- | As 'aesonLaws', but allows explicit control of generationa and shrinking.
--
-- @since 0.1.0
aesonLawsWith :: forall (a :: Type) .
  (Typeable a, ToJSON a, FromJSON a, Eq a, Pretty a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
aesonLawsWith gen shr = testGroup groupName [
  testProperty "Roundtrip" . forAllShrinkShow gen shr stringify $ \x ->
    equiv (Success x) (fromJSON . toJSON $ x),
  testProperty "toJSON-toEncoding agreement" .
    forAllShrinkShow gen shr stringify $ \x ->
      equiv (encodingToText . toEncoding $ x)
            (encodingToText . value . toJSON $ x)
  ]
  where
    groupName :: String
    groupName = "Aeson laws for " <> typeName @a

-- Helpers

-- Helper for turning a Pretty thing into a String
stringify :: forall (a :: Type) .
  (Pretty a) => a -> String
stringify = renderString . layoutSmart defaultLayoutOptions . pretty

-- Like ===, but uses Pretty instead of Show
equiv :: forall (a :: Type) .
  (Eq a, Pretty a) =>
  a -> a -> Property
equiv x y = counterexample go $ x == y
  where
    go :: String
    go = let operator = if x == y then "==" else "/=" in
      renderString . layoutSmart defaultLayoutOptions $
        pretty x <+> operator <+> pretty y

-- Helper for turning the name of an arbitrary Typeable into a String
typeName :: forall (a :: Type) .
  Typeable a => String
typeName = tyConName . typeRepTyCon $ typeRep @a

-- Unfortunate unavoidable orphan
instance (Pretty a) => Pretty (Result a) where
  pretty = \case
    Error err -> "Error" <+> pretty err
    Success x -> "Success" <+> pretty x

-- Saves us pain having to define a Pretty instance for Encoding (and Value by
-- extension)
encodingToText :: Encoding -> Text
encodingToText = decodeUtf8Lenient . BSL.toStrict . encodingToLazyByteString
