{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

module Cardano.YTxP.Example.Offer.Executing (
  -- * TxF Params
  Params,

  -- * Script
  executingTxF,
) where

import Cardano.YTxP.Example.Offer (POfferDatum)
import Cardano.YTxP.Example.Offer.PUtils (
  pisRewarding,
  pmkAddress,
  ptraceDebugC,
  tryGetOfferInput,
 )
import Control.Monad (void)
import Plutarch.LedgerApi.V3 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PAddress,
  PScriptContext,
  PTxOut,
  PValue,
  pdnothing,
 )
import Plutarch.LedgerApi.Value (pvalueOf)
import PlutusLedgerApi.V3 (CurrencySymbol)
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (ptryFromOutputDatum)

-- * Parameters

-- | Parameters for the Executing Transaction Family
newtype Params = Params
  { yieldingMPSymbol :: CurrencySymbol
  }
  deriving stock (Show)

-- * Script

{- | Executing Transaction Family implemented as a YTxP YieldedTo Staking Validator.
Refer to the transaction family specification (examples/direct-offer/doc/transaction-families/executing.md)
for a complete description.
-}
executingTxF :: Params -> Term s (PData :--> PScriptContext :--> POpaque)
executingTxF params = phoistAcyclic $ plam $ \_redeemer context -> unTermCont $ do
  -- ScriptContext extraction

  ptraceDebugC "ScriptContext extraction"

  contextFields <- pletFieldsC @'["txInfo", "scriptInfo"] context

  -- Ensures that this script is activated by the rewarding event
  void $
    pguardC "The current script is expected to be activated by a rewarding event" $
      pisRewarding # contextFields.scriptInfo

  txInfoFields <-
    pletFieldsC
      @'[ "inputs"
        , "outputs"
        , "mint"
        , "data"
        ]
      contextFields.txInfo

  -- The component token must be burned
  pguardC "The component token is expected to be burned" $
    pvalueOf
      # txInfoFields.mint
      # pconstant params.yieldingMPSymbol
      # pconstant ""
      #== -1

  -- Offer input extraction

  ptraceDebugC "Offer input extraction"

  offerInput <-
    pletC $
      tryGetOfferInput
        (pconstant params.yieldingMPSymbol)
        (pfromData txInfoFields.inputs)

  offerInputFields <- pletFieldsC @'["datum"] offerInput

  offerInputDatum <-
    pletC
      ( ptryFromOutputDatum @POfferDatum
          # offerInputFields.datum
          # getField @"data" txInfoFields
      )

  offerInputDatumFields <-
    pletFieldsC @'["creator", "toBuy"] offerInputDatum

  -- Wallet output extraction

  ptraceDebugC "Wallet output extraction"

  void $
    pmatchC $
      tryGetWalletOutput
        (pmkAddress # offerInputDatumFields.creator)
        offerInputDatumFields.toBuy
        (pfromData txInfoFields.outputs)

  pure . popaque $ pconstant ()

-- | Find the first valid wallet output
tryGetWalletOutput ::
  Term s PAddress ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PTxOut)
tryGetWalletOutput walletAddress walletValue outputs =
  pmatch outputs $ \case
    PCons walletOutput rest ->
      pif
        ( walletValue
            #== pfield @"value"
            # walletOutput
            #&& walletAddress
            #== pfield @"address"
            # walletOutput
            #&& pdnothing
            #== pfield @"referenceScript"
            # walletOutput
        )
        walletOutput
        ( pif
            (rest #== pcon PNil)
            ( ptraceInfoError
                "tryGetWalletOutput: Offer component not found"
            )
            (tryGetWalletOutput walletAddress walletValue rest)
        )
    _other -> ptraceInfoError "tryGetWalletOutput: Empty output list"
