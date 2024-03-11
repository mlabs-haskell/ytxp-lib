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
  SerialForm (SerialForm),
  toJSONPValidator,
  toJSONPMintingPolicy,
  parseJSONPValidator,
  parseJSONPMintingPolicy,
  equateConfig,
  prettyConfig,
  equatePValidator,
  equatePMintingPolicy,
  prettyPValidator,
  prettyPMintingPolicy
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
import Prettyprinter (Doc, Pretty (pretty), braces, punctuate, sep, (<+>))
import Text.Builder qualified as TBuilder
import UntypedPlutusCore.Core.Type qualified as UPLC

-- | Avoids an orphan 'Eq' instance for 'Config'.
--
-- @since 0.1.0
equateConfig :: Config -> Config -> Bool
equateConfig (Config tm1) (Config tm2) = case tm1 of
  DetTracing -> case tm2 of
    DetTracing -> True
    _ -> False
  DoTracing -> case tm2 of
    DoTracing -> True
    _ -> False
  DoTracingAndBinds -> case tm2 of
    DoTracingAndBinds -> True
    _ -> False
  NoTracing -> case tm2 of
    NoTracing -> True
    _ -> False

-- | Avoids an orphan 'Pretty' instance for 'Config'.
prettyConfig :: forall (ann :: Type) . Config -> Doc ann
prettyConfig (Config tm) =
  ("Config" <+>) . braces . sep . punctuate "," $ [
    "tracingMode:" <+> case tm of
                         DetTracing -> "DetTracing"
                         DoTracing -> "DoTracing"
                         DoTracingAndBinds -> "DoTracingAndBinds"
                         NoTracing -> "NoTracing"
    ]

-- | Try and compile both Plutarch validators, then compare them as 'Either's, treating
-- 'Right's as if they were 'SerialForm's. Use the provided 'Config' to do the
-- compilation.
--
-- @since 0.1.0
equatePValidator ::
  Config ->
  (forall (s :: S) . Term s (PData :--> PData :--> PScriptContext :--> POpaque)) ->
  (forall (s :: S) . Term s (PData :--> PData :--> PScriptContext :--> POpaque)) ->
  Bool
equatePValidator conf v1 v2 = case compile conf v1 of
  Left err -> case compile conf v2 of
    Left err' -> err == err'
    Right _ -> False
  Right s1 -> case compile conf v2 of
    Left _ -> False
    Right s2 -> SerialForm s1 == SerialForm s2

-- | Try and compile both Plutarch minting policies, then compare them as
-- 'Either's, treating 'Right's as if they were 'SerialForm's. Use the provided
-- 'Config' to do the compilation.
--
-- @since 0.1.0
equatePMintingPolicy ::
  Config ->
  (forall (s :: S) . Term s (PData :--> PScriptContext :--> POpaque)) ->
  (forall (s :: S) . Term s (PData :--> PScriptContext :--> POpaque)) ->
  Bool
equatePMintingPolicy conf v1 v2 = case compile conf v1 of
  Left err -> case compile conf v2 of
    Left err' -> err == err'
    Right _ -> False
  Right s1 -> case compile conf v2 of
    Left _ -> False
    Right s2 -> SerialForm s1 == SerialForm s2

-- | Prettyprint a Plutarch validator as if it was a 'SerialForm', assuming it
-- compiles. Otherwise, prettyprint it as its error message. Use the provided
-- 'Config' to do the compilation.
--
-- @since 0.1.0
prettyPValidator :: forall (ann :: Type) .
  Config ->
  (forall (s :: S) . Term s (PData :--> PData :--> PScriptContext :--> POpaque)) ->
  Doc ann
prettyPValidator conf v = case compile conf v of
  Left err -> pretty err
  Right s -> pretty (SerialForm s)

-- | Prettyprint a Plutarch minting policy as if it was a 'SerialForm', assuming it
-- compiles. Otherwise, prettyprint it as its error message. Use the provided
-- 'Config' to do the compilation.
--
-- @since 0.1.0
prettyPMintingPolicy :: forall (ann :: Type) .
  Config ->
  (forall (s :: S) . Term s (PData :--> PScriptContext :--> POpaque)) ->
  Doc ann
prettyPMintingPolicy conf v = case compile conf v of
  Left err -> pretty err
  Right s -> pretty (SerialForm s)

-- | Helper newtype for deriving 'Eq' and 'Pretty' for 'Script' wrappers, based
-- on their serialized form. While this is perhaps not a fully faithful
-- equality, it's suitable for our purposes.
--
-- @since 0.1.0
newtype SerialForm = SerialForm Script

-- | @since 0.1.0
instance Eq SerialForm where
  {-# INLINEABLE (==) #-}
  (SerialForm s1) == (SerialForm s2) =
    serialiseScript s1 == serialiseScript s2

-- | @since 0.1.0
instance Pretty SerialForm where
  {-# INLINEABLE pretty #-}
  pretty (SerialForm script) =
    pretty .
    TBuilder.run .
    foldMap TBuilder.unsignedHexadecimal .
    SBS.unpack .
    serialiseScript $ script

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
