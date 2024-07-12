module Cardano.YTxP (
  YTxPParams,
  validatorLinker,
  stakeValidatorLinker,
  mintingPolicyLinker,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Map (fromList)
import Data.Text (Text, pack)

import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
  SdkParameters (
    authorisedScriptsSTCS,
    mintingPoliciesNonceList,
    stakingValidatorsNonceList
  ),
 )

import PlutusLedgerApi.V2 (CurrencySymbol)
import Ply qualified

import ScriptExport.ScriptInfo (
  Linker,
  ScriptExport (ScriptExport),
  ScriptRole (ThreeArgumentScript, TwoArgumentScript),
  fetchTS,
  getParam,
  toRoledScript,
 )

--------------------------------------------------------------------------------

data YTxPParams = YTxPParams
  { params :: SdkParameters
  , commitHash :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

validatorLinker :: Linker YTxPParams (ScriptExport YTxPParams)
validatorLinker = do
  info <- getParam

  yieldingValidator <-
    fetchTS
      @'ThreeArgumentScript
      @'[CurrencySymbol]
      "ytxp:yieldingValidator"

  let
    authorisedScriptsSymbol =
      coerce @_ @CurrencySymbol (authorisedScriptsSTCS . params $ info)

    yieldingValidator' =
      yieldingValidator Ply.# authorisedScriptsSymbol

  return $
    ScriptExport
      ( fromList
          [("ytxp:yieldingValidator", toRoledScript yieldingValidator')]
      )
      info

mintingPolicyLinker :: Linker YTxPParams (ScriptExport YTxPParams)
mintingPolicyLinker = do
  info <- getParam

  yieldingMP <-
    fetchTS
      @'TwoArgumentScript
      @'[CurrencySymbol, Integer]
      "ytxp:yieldingMintingPolicy"

  let
    authorisedScriptsSymbol =
      coerce @_ @CurrencySymbol (authorisedScriptsSTCS . params $ info)

    yieldingMPs =
      map
        ( \nonce ->
            ( pack ("ytxp:yieldingMintingPolicy:" <> show nonce)
            , toRoledScript $
                yieldingMP Ply.# authorisedScriptsSymbol Ply.# toInteger nonce
            )
        )
        (mintingPoliciesNonceList . params $ info)

  return $
    ScriptExport
      (fromList yieldingMPs)
      info

stakeValidatorLinker :: Linker YTxPParams (ScriptExport YTxPParams)
stakeValidatorLinker = do
  info <- getParam

  yieldingSV <-
    fetchTS
      @'TwoArgumentScript
      @'[CurrencySymbol, Integer]
      "ytxp:yieldingStakeValidator"

  let
    authorisedScriptsSymbol =
      coerce @_ @CurrencySymbol (authorisedScriptsSTCS . params $ info)

    yieldingSVs =
      map
        ( \nonce ->
            ( pack ("ytxp:yieldingStakeValidator:" <> show nonce)
            , toRoledScript $
                yieldingSV Ply.# authorisedScriptsSymbol Ply.# toInteger nonce
            )
        )
        (stakingValidatorsNonceList . params $ info)

  return $
    ScriptExport
      (fromList yieldingSVs)
      info
