module Cardano.YTxP (
  YTxPParams,
  validatorLinker,
  svLinker,
  mpLinker,
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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

validatorLinker :: Linker YTxPParams (ScriptExport YTxPParams)
validatorLinker = do
  info <- getParam

  yieldingValidator <-
    fetchTS
      @'ThreeArgumentScript
      @'[CurrencySymbol]
      "ytxp:yieldingV"

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

mpLinker :: Linker YTxPParams (ScriptExport YTxPParams)
mpLinker = do
  info <- getParam

  yieldingMP <-
    fetchTS
      @'TwoArgumentScript
      @'[CurrencySymbol, Integer]
      "ytxp:yieldingMP"

  let
    authorisedScriptsSymbol =
      coerce @_ @CurrencySymbol (authorisedScriptsSTCS . params $ info)

    yieldingMPs =
      map
        ( \nonce ->
            ( pack ("ytxp:yieldingMP:" <> show nonce)
            , toRoledScript $
                yieldingMP Ply.# authorisedScriptsSymbol Ply.# toInteger nonce
            )
        )
        (mintingPoliciesNonceList . params $ info)

  return $
    ScriptExport
      (fromList yieldingMPs)
      info

svLinker :: Linker YTxPParams (ScriptExport YTxPParams)
svLinker = do
  info <- getParam

  yieldingSV <-
    fetchTS
      @'TwoArgumentScript
      @'[CurrencySymbol, Integer]
      "ytxp:yieldingSV"

  let
    authorisedScriptsSymbol =
      coerce @_ @CurrencySymbol (authorisedScriptsSTCS . params $ info)

    yieldingSVs =
      map
        ( \nonce ->
            ( pack ("ytxp:yieldingSV:" <> show nonce)
            , toRoledScript $
                yieldingSV Ply.# authorisedScriptsSymbol Ply.# toInteger nonce
            )
        )
        (stakingValidatorsNonceList . params $ info)

  return $
    ScriptExport
      (fromList yieldingSVs)
      info
