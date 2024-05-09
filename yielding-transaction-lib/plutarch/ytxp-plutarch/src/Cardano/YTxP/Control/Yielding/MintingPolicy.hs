module Cardano.YTxP.Control.Yielding.MintingPolicy (
  -- * Minting Policy
  YieldingMPScript (mintingPolicy),
  compileYieldingMP,

  -- * Currency Symbol
  YieldingMPCS,
  mkYieldingMPCS,
) where

import Cardano.YTxP.Control.YieldList.MintingPolicy (YieldListSTCS)
import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  object,
  pairs,
  withObject,
  (.:),
  (.=),
 )

import Data.Text (Text)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext, scriptHash)
import Plutarch.Lift (PConstantDecl, PConstanted, PLifted)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), getScriptHash)

--------------------------------------------------------------------------------
-- Yielding Minting Policy Script

-- | @since 0.1.0
data YieldingMPScript (nonceType :: Type) = YieldingMPScript
  { nonce :: nonceType
  -- ^ @since 0.1.0
  , mintingPolicy :: Script
  -- ^ @since 0.1.0
  }

-- | @since 0.1.0
instance (ToJSON nonceType) => ToJSON (YieldingMPScript nonceType) where
  {-# INLINEABLE toJSON #-}
  toJSON ysvs =
    object
      [ "nonce" .= nonce ysvs
      , "stakingValidator"
          .= (HexStringScript @"StakingValidator" . mintingPolicy $ ysvs)
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding ysvs =
    pairs $
      "nonce" .= nonce ysvs
        <> "mintingPolicy"
          .= (HexStringScript @"StakingValidator" . mintingPolicy $ ysvs)

-- | @since 0.1.0
instance (FromJSON nonceType) => FromJSON (YieldingMPScript nonceType) where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "YieldingMPScript" $ \obj -> do
    ysvsNonce :: nonceType <- obj .: "nonce"
    (HexStringScript ysvsStakingValidator) :: HexStringScript "StakingValidator" <-
      obj .: "mintingPolicy"
    pure $ YieldingMPScript ysvsNonce ysvsStakingValidator

compileYieldingMP ::
  forall (nonceType :: Type).
  ( PConstantDecl nonceType
  , nonceType ~ PLifted (PConstanted nonceType)
  ) =>
  Config ->
  YieldListSTCS ->
  nonceType ->
  Either
    Text
    (YieldingMPScript nonceType)
compileYieldingMP config ylstcs nonce = do
  let
    yieldingMP ::
      forall (s :: S).
      ( Term s (PData :--> PScriptContext :--> POpaque)
      )
    yieldingMP = plet (pconstant nonce) (const $ yieldingHelper ylstcs)
  script <- compile config yieldingMP
  pure $ YieldingMPScript nonce script

-------------------------------------------------------------------------------
-- Yielding Minting Policy Currency Symbol

-- | Opaque, semantic newtype for the YieldList state thread currency symbol
newtype YieldingMPCS = YieldingMPCS CurrencySymbol

mkYieldingMPCS :: YieldingMPScript nonceType -> YieldingMPCS
mkYieldingMPCS (YieldingMPScript _nonce script) =
  YieldingMPCS $ CurrencySymbol (getScriptHash $ scriptHash script)
