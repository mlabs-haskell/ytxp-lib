{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module PPrelude (
  module Prelude,
  module Plutarch.Prelude,
  HexStringScript (HexStringScript),
  WrappedConfig (WrappedConfig),
  toJSONPValidator,
  toJSONPMintingPolicy,
  parseJSONPValidator,
  parseJSONPMintingPolicy
) where

import Control.Monad ((<=<), (>=>))
import Data.Aeson (Encoding, FromJSON (parseJSON), ToJSON (toEncoding, toJSON),
                   Value, withText)
import Data.Aeson.Types (Parser)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.List qualified as List
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Numeric (readHex, showHex)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Internal (Config (Config), RawTerm (RCompiled), Term (Term),
                          TermResult (TermResult),
                          TracingMode (DetTracing, DoTracing, DoTracingAndBinds, NoTracing),
                          compile, tracingMode)
import Plutarch.Prelude
import Plutarch.Script (Script (Script), deserialiseScript, serialiseScript)
import Prelude
import Text.Builder qualified as TBuilder
import UntypedPlutusCore.Core.Type qualified as UPLC

-- | Helper for serializing 'Config's.
--
-- See @JSON.md@ for an explanation of our policy.
--
-- @since 0.1.0
newtype WrappedConfig = WrappedConfig Config

-- | @since 0.1.0
instance ToJSON WrappedConfig where
  {-# INLINEABLE toJSON #-}
  toJSON (WrappedConfig conf) = toJSON @Text $ case tracingMode conf of
    NoTracing -> "NoTracing"
    DetTracing -> "DetTracing"
    DoTracing -> "DoTracing"
    DoTracingAndBinds -> "DoTracingAndBinds"
  {-# INLINEABLE toEncoding #-}
  toEncoding (WrappedConfig conf) = toEncoding @Text $ case tracingMode conf of
    NoTracing -> "NoTracing"
    DetTracing -> "DetTracing"
    DoTracing -> "DoTracing"
    DoTracingAndBinds -> "DoTracingAndBinds"

-- | @since 0.1.0
instance FromJSON WrappedConfig where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withText "WrappedConfig" $ fmap (WrappedConfig . Config) . \case
    "NoTracing" -> pure NoTracing
    "DetTracing" -> pure DetTracing
    "DoTracing" -> pure DoTracing
    "DoTracingAndBinds" -> pure DoTracingAndBinds
    t -> fail $ "Not a valid TracingMode: " <> Text.unpack t

-- | Helper for serializing validators represented as Plutarch closed terms.
--
-- See @JSON.md@ for an explanation of our policy.
--
-- @since 0.1.0
toJSONPValidator ::
  Config ->
  (forall (s :: S) . Term s (PData :--> PData :--> PScriptContext :--> POpaque)) ->
  Value
toJSONPValidator conf t = case compile conf t of
  -- Note from Koz (08/03/24): We have to redundantly use 'toJSON' in both
  -- branches of the case here, because the result of a 'case' cannot be
  -- higher-rank polymorphic. To make the more concise form work, the 'case'
  -- would need to return something equivalent to @exists b . ToJSON b => b@,
  -- which isn't currently possible in GHC.
  --
  -- We have to do this for the other three helpers of this kind as well, for
  -- similar reasons.
  Left err -> toJSON $ "1x" <> err
  Right script -> toJSON . HexStringScript @"PValidator" $ script

-- | Helper for serializing minting policies represented as Plutarch closed
-- terms.
--
-- See @JSON.md@ for an explanation of our policy.
--
-- @since 0.1.0
toJSONPMintingPolicy ::
  Config ->
  (forall (s :: S) . Term s (PData :--> PScriptContext :--> POpaque)) ->
  Value
toJSONPMintingPolicy conf t = case compile conf t of
  Left err -> toJSON $ "1x" <> err
  Right script -> toJSON . HexStringScript @"PMintingPolicy" $ script

-- | Helper for deserializing validators represented as Plutarch closed terms.
--
-- See @JSON.md@ for an explanation of our policy.
--
-- @since 0.1.0
parseJSONPValidator ::
  Value ->
  Parser (forall (s :: S) . Term s (PData :--> PData :--> PScriptContext :--> POpaque))
parseJSONPValidator val = withText "PValidator" go val
  where
    go ::
      Text ->
      Parser (forall (s :: S) . Term s (PData :--> PData :--> PScriptContext :--> POpaque))
    go t = case Text.stripPrefix "1x" t of
      -- Note from Koz (08/03/24): We can't use 'do'-notation here, because we
      -- have to return impredicatively. While ImpredicativeTypes permit this in
      -- principle, this extension is broken in conjunction with 'do'-notation
      -- since approximately its introduction in its modern form with QuickLook.
      -- Weirdly, manually writing '>>='s works.
      --
      -- See https://gitlab.haskell.org/ghc/ghc/-/issues/18126#note_423208.
      Nothing -> parseJSON val >>= \((HexStringScript script) :: HexStringScript "PValidator") ->
        pure $ unsafeTermFromScript script
      Just errMsg -> fail $ "Attempted to deserialize a non-compiling Plutarch closed term.\n" <>
                            "Error: " <> Text.unpack errMsg

-- | Helper for deserializing minting policies represented as Plutarch closed terms.
--
-- See @JSON.md@ for an explanation of our policy.
--
-- @since 0.1.0
parseJSONPMintingPolicy ::
  Value ->
  Parser (forall (s :: S) . Term s (PData :--> PScriptContext :--> POpaque))
parseJSONPMintingPolicy val = withText "PValidator" go val
  where
    go ::
      Text ->
      Parser (forall (s :: S) . Term s (PData :--> PScriptContext :--> POpaque))
    go t = case Text.stripPrefix "1x" t of
      Nothing -> parseJSON val >>= \((HexStringScript script) :: HexStringScript "PMintingPolicy") ->
        pure $ unsafeTermFromScript script
      Just errMsg -> fail $ "Attempted to deserialize a non-compiling Plutarch closed term.\n" <>
                            "Error: " <> Text.unpack errMsg

-- | A helper newtype to ensure that any 'Script's we use are serialized (and
-- deserialized), consistently. We also have a \'tag\' for a more specific type
-- name or label when using a via derivation with this, or as part of a larger
-- type.
--
-- See @JSON.md@ for an explanation of our policy.
--
-- @since 0.1.0
newtype HexStringScript (scriptLabel :: Symbol) = HexStringScript Script

-- | @since 0.1.0
instance ToJSON (HexStringScript scriptLabel) where
  {-# INLINEABLE toJSON #-}
  toJSON (HexStringScript script) = toJSON . mungeScript $ script
  {-# INLINEABLE toEncoding #-}
  toEncoding (HexStringScript script) = toEncoding . mungeScript $ script

-- | @since 0.1.0
instance (KnownSymbol scriptLabel) => FromJSON (HexStringScript scriptLabel) where
  {-# INLINEABLE parseJSON #-}
  parseJSON = (pure . HexStringScript . deserialiseScript) <=< withText scriptLabel' go
    where
      scriptLabel' :: String
      scriptLabel' = symbolVal (Proxy @scriptLabel)
      go :: Text -> Parser ShortByteString
      go t = case Text.stripPrefix "0x" t of
        Nothing -> fail "Provided value does not have a leading \'0x\'"
        Just t' -> SBS.pack <$> do
          asPairList <- toPairs . Text.unpack $ t'
          traverse decodeFromHexPair asPairList

-- Helpers

-- Munges a 'Script' into a 'Text' form suitable for JSON serialization
mungeScript :: Script -> Text
mungeScript script =
  TBuilder.run $ "0x" <> (foldMap TBuilder.unsignedHexadecimal . SBS.unpack . serialiseScript $ script)

toPairs :: forall (m :: Type -> Type) (a :: Type) .
  MonadFail m => [a] -> m [(a, a)]
toPairs = \case
  [] -> pure []
  [_] -> fail "Invalid hex encoding provided: odd length"
  (x : y : ys) -> ((x, y) :) <$> toPairs ys

decodeFromHexPair :: forall (m :: Type -> Type) .
  MonadFail m => (Char, Char) -> m Word8
decodeFromHexPair (nibble1, nibble2) = do
  let input = [nibble1, nibble2]
  case readHex input of
    [(w8, [])] -> pure w8
    _ -> fail $ "Invalid hex encoding: could not decode " <> input <> " as hex"

-- Put a compiled script into a Plutarch term. The type of the
-- script is arbitrary: THIS IS UNSAFE. Make sure you know the
-- correct type! Ply can help annotate exported scripts with the correct types.
unsafeTermFromScript :: forall (a :: S -> Type). Script -> forall (s :: S). Term s a
unsafeTermFromScript (Script script) =
  Term $ const $ pure $ TermResult (RCompiled $ UPLC._progTerm script) []
