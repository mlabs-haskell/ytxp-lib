{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PPrelude (
  module Prelude,
  module Plutarch.Prelude,
  HexStringScript (HexStringScript)
) where

import Control.Monad ((<=<), (>=>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), withText)
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
import Plutarch.Prelude
import Plutarch.Script (Script, deserialiseScript, serialiseScript)
import Prelude
import Text.Builder qualified as TBuilder

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
