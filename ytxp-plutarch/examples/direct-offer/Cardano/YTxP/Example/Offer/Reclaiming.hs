{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

module Cardano.YTxP.Example.Offer.Reclaiming (
  -- * TxF Params
  Params,

  -- * Script
  reclaimingTxF,
) where

import Cardano.YTxP.Example.Offer (POfferDatum)
import Cardano.YTxP.Example.Offer.PUtils (pisRewarding, ptraceDebugC, tryGetOfferInput)
import Control.Monad (void)
import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.LedgerApi.Value (pvalueOf)
import PlutusLedgerApi.V3 (CurrencySymbol)
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (ptryFromOutputDatum)

-- * Parameters

-- | Parameters for the Reclaiming Transaction Family
newtype Params = Params
  { yieldingMPSymbol :: CurrencySymbol
  }
  deriving stock (Show)

-- * Script

{- | Reclaiming Transaction Family implemented as a YTxP YieldedTo Staking Validator.
Refer to the transaction family specification (examples/direct-offer/doc/transaction-families/reclaiming.md)
for a complete description.
-}
reclaimingTxF :: Params -> Term s (PData :--> PScriptContext :--> POpaque)
reclaimingTxF params = phoistAcyclic $ plam $ \_redeemer context -> unTermCont $ do
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
        , "mint"
        , "data"
        , "signatories"
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
    pletFieldsC @'["creator"] offerInputDatum

  pguardC "Expect signature of the offer creator" $
    pelem # offerInputDatumFields.creator # txInfoFields.signatories

  pure . popaque $ pconstant ()
