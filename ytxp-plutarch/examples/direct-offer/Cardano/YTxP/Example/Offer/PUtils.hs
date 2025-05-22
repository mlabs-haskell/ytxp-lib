{-# LANGUAGE LambdaCase #-}

module Cardano.YTxP.Example.Offer.PUtils (
  pisRewarding,
  ptraceDebugC,
  tryGetOfferOutput,
  tryGetOfferInput,
) where

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptInfo (PRewardingScript),
  PTxInInfo,
  PTxOut,
  pdnothing,
  ptxInInfo'resolved,
  ptxOut'address,
  ptxOut'referenceScript,
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

-- | Find the first valid offer component output
tryGetOfferOutput ::
  Term s PCurrencySymbol ->
  Term s PAddress ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PTxOut)
tryGetOfferOutput yieldingMPSymbol yieldingValidatorAddress outputs =
  pmatch outputs $ \case
    PCons offerOutput' rest -> unTermCont $ do
      offerOutput <- pmatchC $ pfromData offerOutput'
      return $
        pif
          ( ( pvalueOf
                # pfromData (ptxOut'value offerOutput)
                # yieldingMPSymbol
                # pconstant (TokenName "")
            )
              #== pconstant 1
              #&& yieldingValidatorAddress
              #== ptxOut'address offerOutput
              #&& pdnothing
              #== ptxOut'referenceScript offerOutput
          )
          offerOutput'
          ( pif
              (rest #== pcon PNil)
              ( ptraceInfoError
                  "tryGetOfferOutput: Offer component not found"
              )
              (tryGetOfferOutput yieldingMPSymbol yieldingValidatorAddress rest)
          )
    _other -> ptraceInfoError "tryGetOfferOutput: Empty output list"

-- | Find the first valid offer component input
tryGetOfferInput ::
  Term s PCurrencySymbol ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PTxOut
tryGetOfferInput yieldingMPSymbol inputs =
  pmatch inputs $ \case
    PCons offerInInfo' rest -> unTermCont $ do
      offerInInfo <- pmatchC $ pfromData offerInInfo'
      resolved' <- pletC $ ptxInInfo'resolved offerInInfo
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
                  "tryGetOfferInput: Offer component not found"
              )
              (tryGetOfferInput yieldingMPSymbol rest)
          )
    _other -> ptraceInfoError "tryGetOfferInput: Empty output list"
