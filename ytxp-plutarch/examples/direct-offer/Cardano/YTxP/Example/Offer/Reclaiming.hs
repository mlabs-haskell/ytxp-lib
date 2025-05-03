{-# LANGUAGE OverloadedRecordDot #-}

module Cardano.YTxP.Example.Offer.Reclaiming (
  -- * TxF Params
  Params,

  -- * Script
  reclaimingTxF,
) where

import Cardano.YTxP.Example.Offer (POfferDatum (creator))
import Cardano.YTxP.Example.Offer.PUtils (
  pisRewarding,
  ptraceDebugC,
  tryGetOfferInput,
 )
import Control.Monad (void)
import Plutarch.LedgerApi.V3 (
  PDatum (PDatum),
  POutputDatum (POutputDatum),
  PScriptContext,
  pscriptContext'scriptInfo,
  pscriptContext'txInfo,
  ptxInfo'inputs,
  ptxInfo'mint,
  ptxInfo'signatories,
  ptxOut'datum,
 )
import Plutarch.LedgerApi.Value (pvalueOf)
import PlutusLedgerApi.V3 (CurrencySymbol)

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
reclaimingTxF params = phoistAcyclic $ plam $ \_redeemer context' -> unTermCont $ do
  -- ScriptContext extraction

  ptraceDebugC "ScriptContext extraction"

  context <- pmatchC context'

  -- Ensures that this script is activated by the rewarding event
  void $
    pguardC "The current script is expected to be activated by a rewarding event" $
      pisRewarding # pscriptContext'scriptInfo context

  txInfo <- pmatchC $ pscriptContext'txInfo context

  -- The component token must be burned
  pguardC "The component token is expected to be burned" $
    pvalueOf
      # pfromData (ptxInfo'mint txInfo)
      # pconstant params.yieldingMPSymbol
      # pconstant ""
      #== -1

  -- Offer input extraction

  ptraceDebugC "Offer input extraction"

  offerInput <-
    pmatchC $
      tryGetOfferInput
        (pconstant params.yieldingMPSymbol)
        (pfromData $ ptxInfo'inputs txInfo)

  -- TODO fix this
  POutputDatum offerInputDatum <- pmatchC $ ptxOut'datum offerInput
  PDatum offerInputData <- pmatchC offerInputDatum
  offerDatum <-
    pmatchC $
      pfromData $
        ptryFrom @(PAsData POfferDatum) (pto offerInputData) fst

  pguardC "Expect signature of the offer creator" $
    pelem # creator offerDatum # pfromData (ptxInfo'signatories txInfo)

  pure . popaque $ pconstant @PUnit ()
