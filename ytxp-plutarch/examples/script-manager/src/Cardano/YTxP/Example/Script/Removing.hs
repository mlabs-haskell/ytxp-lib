{-# LANGUAGE OverloadedRecordDot #-}

module Cardano.YTxP.Example.Script.Removing (
  -- * TxF Params
  Params,

  -- * Script
  removingTxF,
) where

import Cardano.YTxP.Example.PUtils (
  pisRewarding,
  ptraceDebugC,
  tryGetComponentInput,
 )
import Control.Monad (void)
import Plutarch.Builtin.Unit (punit)
import Plutarch.LedgerApi.V3 (
  PScriptContext (pscriptContext'scriptInfo),
  pscriptContext'txInfo,
  ptxInfo'inputs,
  ptxInfo'mint,
  ptxInfo'signatories,
 )
import Plutarch.LedgerApi.Value (pvalueOf)
import PlutusLedgerApi.V3 (CurrencySymbol, PubKeyHash, TokenName (TokenName))

-- * Parameters

-- | Parameters for the Removing Transaction Family
data Params = Params
  { yieldingMPSymbol :: CurrencySymbol
  , signatory :: PubKeyHash
  }
  deriving stock (Show)

-- * Script

{- | Removing Transaction Family implemented as a YTxP YieldedTo Staking Validator.
Refer to the transaction family specification (examples/script-manager/doc/transaction-families/removing.md)
for a complete description.
-}
removingTxF :: Params -> Term s (PScriptContext :--> PUnit)
removingTxF params = phoistAcyclic $ plam $ \context' -> unTermCont $ do
  -- ScriptContext extraction

  ptraceDebugC "ScriptContext extraction"

  context <- pmatchC context'
  txInfo' <- pletC $ pscriptContext'txInfo context
  scriptInfo <- pletC $ pscriptContext'scriptInfo context
  txInfo <- pmatchC txInfo'

  -- Deploying must be authorised by the signatory
  pguardC "The transaction is expected to be signed by the signatory" $
    pelem
      # pdata (pconstant params.signatory)
      # pfromData (ptxInfo'signatories txInfo)

  -- Ensures that this script is activated by the rewarding event
  void $
    pguardC "The current script is expected to be activated by a rewarding event" $
      pisRewarding # scriptInfo

  -- The component token must be burned
  pguardC "The component token is expected to be burned" $
    pvalueOf
      # pfromData (ptxInfo'mint txInfo)
      # pconstant params.yieldingMPSymbol
      # pconstant (TokenName "")
      #== -1

  -- Ensures there is a valid component input
  _ <-
    pmatchC $
      tryGetComponentInput
        (pconstant params.yieldingMPSymbol)
        (pfromData $ ptxInfo'inputs txInfo)

  pure punit
