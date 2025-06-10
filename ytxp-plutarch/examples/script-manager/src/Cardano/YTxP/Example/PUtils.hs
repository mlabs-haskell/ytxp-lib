{-# LANGUAGE LambdaCase #-}

module Cardano.YTxP.Example.PUtils (
  pisRewarding,
  ptraceDebugC,
  tryGetComponentOutput,
  tryGetComponentInput,
) where

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptInfo (PRewardingScript),
  PTxInInfo,
  PTxOut,
  ptxInInfo'resolved,
  ptxOut'address,
  ptxOut'value,
 )
import Plutarch.LedgerApi.Value (pvalueOf)
import PlutusLedgerApi.V3 (TokenName (TokenName))

-- | Return False if script purpose is not rewarding
pisRewarding :: Term s (PScriptInfo :--> PBool)
pisRewarding = phoistAcyclic $ plam $ \purpose ->
  pmatch purpose $ \case
    PRewardingScript _ -> pcon PTrue
    _other -> pcon PFalse

-- | Like `ptraceDebug`, but works in a `TermCont` monad
ptraceDebugC :: Term s PString -> TermCont s ()
ptraceDebugC s = tcont $ \f -> ptraceInfo s (f ())

-- | Find the first valid component output
tryGetComponentOutput ::
  Term s PCurrencySymbol ->
  Term s PAddress ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PTxOut)
tryGetComponentOutput yieldingMPSymbol yieldingValidatorAddress outputs =
  pmatch outputs $ \case
    PCons componentOutput' rest -> unTermCont $ do
      componentOutput <- pmatchC $ pfromData componentOutput'
      return $
        pif
          ( ( pvalueOf
                # pfromData (ptxOut'value componentOutput)
                # yieldingMPSymbol
                # pconstant (TokenName "")
            )
              #== pconstant 1
              #&& yieldingValidatorAddress
              #== ptxOut'address componentOutput
          )
          componentOutput'
          ( pif
              (rest #== pcon PNil)
              ( ptraceInfoError
                  "tryGetComponentOutput: Component not found"
              )
              (tryGetComponentOutput yieldingMPSymbol yieldingValidatorAddress rest)
          )
    _other -> ptraceInfoError "tryGetComponentOutput: Empty output list"

-- | Find the first valid component input
tryGetComponentInput ::
  Term s PCurrencySymbol ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PTxOut
tryGetComponentInput yieldingMPSymbol inputs =
  pmatch inputs $ \case
    PCons componentInInfo' rest -> unTermCont $ do
      componentInInfo <- pmatchC $ pfromData componentInInfo'
      resolved' <- pletC $ ptxInInfo'resolved componentInInfo
      resolved <- pmatchC resolved'

      return $
        pif
          ( ( pvalueOf
                # pfromData (ptxOut'value resolved)
                # yieldingMPSymbol
                # pconstant (TokenName "")
            )
              #== pconstant 1
          )
          resolved'
          ( pif
              (rest #== pcon PNil)
              ( ptraceInfoError
                  "tryGetComponentInput: Component not found"
              )
              (tryGetComponentInput yieldingMPSymbol rest)
          )
    _other -> ptraceInfoError "tryGetComponentInput: Empty output list"
