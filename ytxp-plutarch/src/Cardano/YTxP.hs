module Cardano.YTxP (
  linker,
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

data YTxPInfo = YTxPInfo
  { params :: SdkParameters
  , commitHash :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

linker :: Linker YTxPInfo (ScriptExport YTxPInfo)
linker = do
  info <- getParam

  yieldingValidator <-
    fetchTS
      @'ThreeArgumentScript
      @'[CurrencySymbol]
      "djed:yieldingValidator"

  yieldingMP <-
    fetchTS
      @'TwoArgumentScript
      @'[CurrencySymbol, Integer]
      "djed:yieldingMP"

  yieldingSV <-
    fetchTS
      @'TwoArgumentScript
      @'[CurrencySymbol, Integer]
      "djed:yieldingSV"

  let
    authorisedScriptsSymbol =
      coerce @_ @CurrencySymbol (authorisedScriptsSTCS . params $ info)

    yieldingValidator' =
      yieldingValidator Ply.# authorisedScriptsSymbol

    yieldingMPs =
      map
        ( \nonce ->
            ( pack ("djed:yieldingMP:" <> show nonce)
            , toRoledScript $
                yieldingMP Ply.# authorisedScriptsSymbol Ply.# toInteger nonce
            )
        )
        (mintingPoliciesNonceList . params $ info)

    yieldingSVs =
      map
        ( \nonce ->
            ( pack ("djed:yieldingSV:" <> show nonce)
            , toRoledScript $
                yieldingSV Ply.# authorisedScriptsSymbol Ply.# toInteger nonce
            )
        )
        (stakingValidatorsNonceList . params $ info)

  return $
    ScriptExport
      ( fromList
          ( ("djed:yieldingValidator", toRoledScript yieldingValidator')
              : yieldingMPs
                <> yieldingSVs
          )
      )
      info
