{- |
Module      : Cardano.YTxP.Control.Parameters
Description : Data required to work with the compiled control scripts

This module is consider deprecated. Use Ply instead.
-}
module Cardano.YTxP.SDK.ControlParameters
  {-# DEPRECATED "Use Ply instead." #-} (
  -- * Types
  YieldingScripts (..),
  ControlParameters (..),
  HexStringScript (..),
  sbsToHexText,
  hexTextToSbs,
) where

import Cardano.YTxP.SDK.SdkParameters (SdkParameters)
import Control.Monad ((<=<))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  object,
  pairs,
  withObject,
  withText,
  (.:),
  (.=),
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prettyprinter (
  Pretty (pretty),
  align,
  braces,
  punctuate,
  viaShow,
  vsep,
  (<+>),
 )

{- | A helper newtype to ensure that any 'Script's we use are serialized (and
deserialized), consistently. We also have a \'tag\' for a more specific type
name or label when using a via derivation with this, or as part of a larger
type.

See @JSON.md@ for an explanation of our policy. TODO: update json policy
-}
newtype HexStringScript (scriptLabel :: Symbol) = HexStringScript ShortByteString
  deriving newtype (Eq)

instance Pretty (HexStringScript (scriptLabel :: Symbol)) where
  pretty (HexStringScript s) = viaShow s

instance ToJSON (HexStringScript scriptLabel) where
  {-# INLINEABLE toJSON #-}
  toJSON (HexStringScript script) = toJSON . sbsToHexText $ script
  {-# INLINEABLE toEncoding #-}
  toEncoding (HexStringScript script) = toEncoding . sbsToHexText $ script

instance (KnownSymbol scriptLabel) => FromJSON (HexStringScript scriptLabel) where
  {-# INLINEABLE parseJSON #-}
  parseJSON =
    (pure . HexStringScript)
      <=< withText scriptLabel' hexTextToSbs
    where
      scriptLabel' :: String
      scriptLabel' = symbolVal (Proxy @scriptLabel)

instance (KnownSymbol scriptLabel) => Show (HexStringScript scriptLabel) where
  show (HexStringScript script) = scriptLabel' <> " = " <> show script
    where
      scriptLabel' :: String
      scriptLabel' = symbolVal (Proxy @scriptLabel)

{- | Scripts that yield to transaction families identified by reference scripts
which carry a state thread token
-}
data YieldingScripts = YieldingScripts
  { yieldingMintingPolicies :: [HexStringScript "YieldingMP"]
  -- ^ @since 0.1.0
  , yieldingValidator :: HexStringScript "YieldingValidator"
  -- ^ @since 0.1.0
  , yieldingStakingValidators :: [HexStringScript "YieldingSV"]
  -- ^ @since 0.1.0
  }
  deriving stock (Eq, Show)

instance Pretty YieldingScripts where
  pretty
    YieldingScripts
      { yieldingMintingPolicies
      , yieldingValidator
      , yieldingStakingValidators
      } =
      ("YieldingScripts:" <+>) . braces . align . vsep . punctuate "," $
        [ "yieldingMintingPolicies:" <+> pretty yieldingMintingPolicies
        , "yieldingValidator:" <+> pretty yieldingValidator
        , "yieldingStakingValidators:" <+> pretty yieldingStakingValidators
        ]

-- | @since 0.1.0
instance ToJSON YieldingScripts where
  {-# INLINEABLE toJSON #-}
  toJSON ys =
    object
      [ "yieldingMintingPolicies" .= yieldingMintingPolicies ys
      , "yieldingValidator" .= yieldingValidator ys
      , "yieldingStakingValidators" .= yieldingStakingValidators ys
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding ys =
    pairs $
      "yieldingMintingPolicies"
        .= yieldingMintingPolicies ys
        <> "yieldingValidator"
          .= yieldingValidator ys
        <> "yieldingStakingValidators"
          .= yieldingStakingValidators ys

-- | @since 0.1.0
instance FromJSON YieldingScripts where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "YieldingScripts" $ \obj -> do
    ysmp <- obj .: "yieldingMintingPolicies"
    ysv <- obj .: "yieldingValidator"
    ysvs <- obj .: "yieldingStakingValidators"
    pure $ YieldingScripts ysmp ysv ysvs

{- | Contains the compiled scripts along with the parameters
they were compiled against. This is useful for _library consumers_
and should contain all of the information needed to work with the
library.
-}
data ControlParameters = ControlParameters
  { yieldingScripts :: YieldingScripts
  -- ^ @since 0.1.0
  , sdkParameters :: SdkParameters
  -- ^ @since 0.1.0
  }
  deriving stock (Eq, Show)

-- | @since 0.1.0
instance ToJSON ControlParameters where
  {-# INLINEABLE toJSON #-}
  toJSON cp =
    object
      [ "yieldingScripts" .= yieldingScripts cp
      , "sdkParameters" .= sdkParameters cp
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding cp =
    pairs $
      "yieldingScripts"
        .= yieldingScripts cp
        <> "sdkParameters"
          .= sdkParameters cp

instance FromJSON ControlParameters where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "ControlParameters" $ \obj -> do
    ys <- obj .: "yieldingScripts"
    cpi <- obj .: "sdkParameters"
    pure $ ControlParameters ys cpi

instance Pretty ControlParameters where
  pretty ControlParameters {yieldingScripts, sdkParameters} =
    ("ControlParameters:" <+>) . braces . align . vsep . punctuate "," $
      [ pretty yieldingScripts
      , pretty sdkParameters
      ]

-- | Converts a 'ShortByteString' into a textual representation in hex.
sbsToHexText :: ShortByteString -> Text
sbsToHexText = TE.decodeUtf8 . Base16.encode . SBS.fromShort

{- | Attempts to parse the given 'Text' into the 'ShortByteString' that would
have produced it via 'sbsToHexText', indicating parse failures with 'fail'.
-}
hexTextToSbs :: (MonadFail m) => Text -> m ShortByteString
hexTextToSbs t = case Base16.decode $ TE.encodeUtf8 t of
  Left e -> fail e
  Right b -> pure $ SBS.toShort b
