module Cardano.YTxP.Control.Yielding.StakingValidator (
  -- * Staking Validator
  YieldingStakingValidatorScript (nonce, stakingValidator),
  compileYieldingStakingValidator,

  -- * Credential and Nonce
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
import Numeric.Natural (Natural)
import Plutarch (Config, compile)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Script (Script)

--------------------------------------------------------------------------------
-- Yielding Staking Validator

{- | A yielding staking validator together with its nonce.

@since 0.1.0
-}
data YieldingStakingValidatorScript = YieldingStakingValidatorScript
  { nonce :: Natural
  -- ^ @since 0.1.0
  , stakingValidator :: Script
  -- ^ @since 0.10
  }

-- | @since 0.1.0
instance ToJSON YieldingStakingValidatorScript where
  {-# INLINEABLE toJSON #-}
  toJSON ysvs =
    object
      [ "nonce" .= nonce ysvs
      , "stakingValidator"
          .= (HexStringScript @"StakingValidator" . stakingValidator $ ysvs)
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding ysvs =
    pairs $
      "nonce" .= nonce ysvs
        <> "stakingValidator"
          .= (HexStringScript @"StakingValidator" . stakingValidator $ ysvs)

-- | @since 0.1.0
instance FromJSON YieldingStakingValidatorScript where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "YieldingStakingValidatorScript" $ \obj -> do
    ysvsNonce <- obj .: "nonce"
    (HexStringScript ysvsStakingValidator) :: HexStringScript "StakingValidator" <-
      obj .: "stakingValidator"
    pure $ YieldingStakingValidatorScript ysvsNonce ysvsStakingValidator

{- | Compile a yielding staking validator that has been nonced.
The nonce is required because each staking validator can only
be delegated to a single pool; the inclusion of the nonce will change the
script hash.
-}
compileYieldingStakingValidator ::
  Config ->
  YieldListSTCS ->
  Natural ->
  Either
    Text
    YieldingStakingValidatorScript
compileYieldingStakingValidator config ylstcs nonce = do
  let
    yieldingStakingValidator ::
      Term s (PData :--> PScriptContext :--> POpaque)
    yieldingStakingValidator =
      plet (pconstant $ toInteger nonce) (const $ yieldingHelper ylstcs)

  -- Pull the "Either" through the list
  script <- compile config yieldingStakingValidator

  pure $
    YieldingStakingValidatorScript nonce script
