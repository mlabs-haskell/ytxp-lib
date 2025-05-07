{-# LANGUAGE OverloadedRecordDot #-}

module Cardano.YTxP.Example.Offer.Creating (
  -- * TxF Params
  Params,

  -- * Script
  creatingTxF,
) where

import Cardano.YTxP.Example.Offer (POfferDatum)
import Cardano.YTxP.Example.Offer.PUtils (
  pisRewarding,
  ptraceDebugC,
  tryGetOfferOutput,
 )
import Control.Monad (void)
import Plutarch.Builtin.Unit (punit)
import Plutarch.LedgerApi.V3 (
  PDatum (PDatum),
  POutputDatum (POutputDatum),
  PScriptContext (pscriptContext'scriptInfo),
  pscriptContext'txInfo,
  ptxInfo'mint,
  ptxInfo'outputs,
  ptxOut'datum,
 )
import Plutarch.LedgerApi.Value (pvalueOf)
import PlutusLedgerApi.V3 (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  ScriptHash,
  TokenName (TokenName),
 )

-- * Parameters

-- | Parameters for the Creating Transaction Family
data Params = Params
  { yieldingValidatorScriptHash :: !ScriptHash
  , yieldingMPSymbol :: !CurrencySymbol
  }
  deriving stock (Show)

-- * Script

{- | Creating Transaction Family implemented as a YTxP YieldedTo Staking Validator.
Refer to the transaction family specification (examples/direct-offer/doc/transaction-families/creating.md)
for a complete description.
-}
creatingTxF :: Params -> Term s (PScriptContext :--> POpaque)
creatingTxF params = phoistAcyclic $ plam $ \context' -> unTermCont $ do
  -- ScriptContext extraction

  ptraceDebugC "ScriptContext extraction"

  context <- pmatchC context'

  -- Ensures that this script is activated by the rewarding event
  void $
    pguardC "The current script is expected to be activated by a rewarding event" $
      pisRewarding # pscriptContext'scriptInfo context

  txInfo <- pmatchC $ pscriptContext'txInfo context

  -- The component token must be minted
  pguardC "The component token is expected to be minted" $
    pvalueOf
      # pfromData (ptxInfo'mint txInfo)
      # pconstant params.yieldingMPSymbol
      # pconstant (TokenName "")
      #== 1

  -- Offer output extraction

  ptraceDebugC "Offer output extraction"

  offerOutput <-
    pmatchC $
      pfromData $
        tryGetOfferOutput
          (pconstant params.yieldingMPSymbol)
          ( pconstant . flip Address Nothing . ScriptCredential $
              params.yieldingValidatorScriptHash
          )
          (pfromData $ ptxInfo'outputs txInfo)

  -- Ensures that the value guarantees are met
  -- TODO fix this
  POutputDatum offerInputDatum <- pmatchC $ ptxOut'datum offerOutput
  PDatum offerInputData <- pmatchC offerInputDatum
  void $
    pmatchC $
      pfromData $
        ptryFrom @(PAsData POfferDatum) (pto offerInputData) fst

  pure $ popaque punit
