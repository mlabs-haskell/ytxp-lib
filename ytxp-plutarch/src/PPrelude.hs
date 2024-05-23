{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PPrelude (
  module Prelude,
  module Plutarch.Prelude,
  serialiseScript,
  deserialiseScript,
  hexTextToBs,
  bsToHexText,
) where

import Control.Monad ((<=<), (>=>))
import Data.Aeson (
  Encoding,
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  Value,
  withText,
 )
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Foldable (foldl')
import Data.List qualified as List
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Numeric (readHex, showHex)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Internal (
  Config (Config),
  RawTerm (RCompiled),
  Term (Term),
  TermResult (TermResult),
  TracingMode (DetTracing, DoTracing, DoTracingAndBinds, NoTracing),
  compile,
  tracingMode,
 )
import Plutarch.Prelude
import Plutarch.Script (Script (Script))
import Plutarch.Script qualified as PScript
import Prettyprinter (Doc, Pretty (pretty), braces, punctuate, sep, (<+>))
import UntypedPlutusCore.Core.Type qualified as UPLC
import Prelude

bsToHexText :: ByteString -> Text
bsToHexText = TE.decodeUtf8 . Base16.encode

hexTextToBs :: Text -> Either String ByteString
hexTextToBs = Base16.decode . TE.encodeUtf8

-- Helpers

serialiseScript :: Script -> Text
serialiseScript = bsToHexText . SBS.fromShort . PScript.serialiseScript

deserialiseScript ::
  forall (m :: Type -> Type).
  (MonadFail m) =>
  Text ->
  m Script
deserialiseScript s = case hexTextToBs s of
  (Left e) -> fail e
  (Right s) -> pure $ PScript.deserialiseScript . SBS.toShort $ s
