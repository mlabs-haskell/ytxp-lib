{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cardano.YTxP.Example.Offer.PUtils (
  pisRewarding,
  ptraceDebugC,
  tryGetOfferOutput,
  tryGetOfferInput,
  pmkAddress,
) where

import Plutarch.Extra.Record (mkRecordConstr, (.&), (.=))
import Plutarch.LedgerApi.V3 (
  PAddress (PAddress),
  PCredential (PPubKeyCredential),
  PCurrencySymbol,
  PPubKeyHash,
  PScriptInfo (PRewardingScript),
  PTxInInfo,
  PTxOut,
  pdnothing,
 )
import Plutarch.LedgerApi.Value (pvalueOf)

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
    PCons offerOutput rest ->
      pif
        ( ( pvalueOf
              # pfromData (pfield @"value" # offerOutput)
              # yieldingMPSymbol
              # pconstant ""
          )
            #== pconstant 1
            #&& yieldingValidatorAddress
            #== pfield @"address"
            # offerOutput
            #&& pdnothing
            #== pfield @"referenceScript"
            # offerOutput
        )
        offerOutput
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
  Term s (PAsData PTxOut)
tryGetOfferInput yieldingMPSymbol inputs =
  pmatch inputs $ \case
    PCons offerInInfo rest ->
      pletFields @'["resolved"] offerInInfo $
        \fields ->
          pif
            ( ( pvalueOf
                  # pfromData (pfield @"value" # pfromData fields.resolved)
                  # yieldingMPSymbol
                  # pconstant ""
              )
                #== pconstant 1
            )
            fields.resolved
            ( pif
                (rest #== pcon PNil)
                ( ptraceInfoError
                    "tryGetOfferInput: Offer component not found"
                )
                (tryGetOfferInput yieldingMPSymbol rest)
            )
    _other -> ptraceInfoError "tryGetOfferInput: Empty output list"

-- | Make an address from a pub key hash.
pmkAddress ::
  forall (s :: S).
  Term
    s
    ( PAsData PPubKeyHash
        :--> PAddress
    )
pmkAddress = phoistAcyclic $
  plam $ \hash ->
    mkRecordConstr
      PAddress
      ( #credential
          .= pdata
            ( mkRecordConstr
                PPubKeyCredential
                ( #_0 .= hash
                )
            )
          .& #stakingCredential
          .= pdata pdnothing
      )
