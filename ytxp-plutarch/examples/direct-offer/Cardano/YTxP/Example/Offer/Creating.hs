{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

module Cardano.YTxP.Example.Offer.Creating (
  -- * TxF Params
  Params,

  -- * Script
  creatingTxF,
) where

import Cardano.YTxP.Example.Offer (POfferDatum)
import Cardano.YTxP.Example.Offer.PUtils (pisRewarding, ptraceDebugC, tryGetOfferOutput)
import Control.Monad (void)
import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.LedgerApi.Value (pvalueOf)
import PlutusLedgerApi.V3 (Address (Address), Credential (ScriptCredential), CurrencySymbol, ScriptHash)
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (ptryFromOutputDatum)

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
creatingTxF :: Params -> Term s (PData :--> PScriptContext :--> POpaque)
creatingTxF params = phoistAcyclic $ plam $ \_redeemer context -> unTermCont $ do
  -- ScriptContext extraction

  ptraceDebugC "ScriptContext extraction"

  contextFields <- pletFieldsC @'["txInfo", "scriptInfo"] context

  -- Ensures that this script is activated by the rewarding event
  void $
    pguardC "The current script is expected to be activated by a rewarding event" $
      pisRewarding # contextFields.scriptInfo

  txInfoFields <-
    pletFieldsC
      @'[ "outputs"
        , "mint"
        , "data"
        ]
      contextFields.txInfo

  -- The component token must be minted
  pguardC "The component token is expected to be minted" $
    pvalueOf
      # txInfoFields.mint
      # pconstant params.yieldingMPSymbol
      # pconstant ""
      #== 1

  -- Offer output extraction

  ptraceDebugC "Offer output extraction"

  offerOutput <-
    pletC $
      tryGetOfferOutput
        (pconstant params.yieldingMPSymbol)
        ( pconstant . flip Address Nothing . ScriptCredential $
            params.yieldingValidatorScriptHash
        )
        (pfromData txInfoFields.outputs)

  offerOutputFields <- pletFieldsC @'["datum"] offerOutput

  -- Ensures that the value guarantees are met
  void $
    pmatchC $
      ptryFromOutputDatum @POfferDatum
        # offerOutputFields.datum
        # getField @"data" txInfoFields

  pure . popaque $ pconstant @PUnit ()
