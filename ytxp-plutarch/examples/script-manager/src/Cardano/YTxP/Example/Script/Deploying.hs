{-# LANGUAGE OverloadedRecordDot #-}

module Cardano.YTxP.Example.Script.Deploying (
  -- * TxF Params
  Params,

  -- * Script
  deployingTxF,
) where

import Cardano.YTxP.Example.PUtils (
  pisRewarding,
  ptraceDebugC,
  tryGetComponentOutput,
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
  ptxInfo'signatories,
  ptxOut'datum,
 )
import Plutarch.LedgerApi.Value (pvalueOf)
import PlutusLedgerApi.V3 (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  PubKeyHash,
  ScriptHash,
  TokenName (TokenName),
 )

-- * Parameters

-- | Parameters for the Deploying Transaction Family
data Params = Params
  { yieldingValidatorScriptHash :: !ScriptHash
  , yieldingMPSymbol :: !CurrencySymbol
  , signatory :: !PubKeyHash
  }
  deriving stock (Show)

-- * Script

{- | Deploying Transaction Family implemented as a YTxP YieldedTo Staking Validator.
Refer to the transaction family specification (examples/script-manager/doc/transaction-families/deploying.md)
for a complete description.
-}
deployingTxF :: Params -> Term s (PScriptContext :--> PUnit)
deployingTxF params = phoistAcyclic $ plam $ \context' -> unTermCont $ do
  -- ScriptContext extraction

  ptraceDebugC "ScriptContext extraction"

  context <- pmatchC context'

  -- Ensures that this script is activated by the rewarding event
  void $
    pguardC "The current script is expected to be activated by a rewarding event" $
      pisRewarding # pscriptContext'scriptInfo context

  txInfo <- pmatchC $ pscriptContext'txInfo context

  -- Deploying must be authorised by the signatory
  pguardC "The transaction is expected to be signed by the signatory" $
    pelem
      # pdata (pconstant params.signatory)
      # pfromData (ptxInfo'signatories txInfo)

  -- The component token must be minted
  pguardC "The component token is expected to be minted" $
    pvalueOf
      # pfromData (ptxInfo'mint txInfo)
      # pconstant params.yieldingMPSymbol
      # pconstant (TokenName "")
      #== 1

  -- Script output extraction

  ptraceDebugC "Script output extraction"

  scriptOutput <-
    pmatchC $
      pfromData $
        tryGetComponentOutput
          (pconstant params.yieldingMPSymbol)
          ( pconstant . flip Address Nothing . ScriptCredential $
              params.yieldingValidatorScriptHash
          )
          (pfromData $ ptxInfo'outputs txInfo)

  pguardC "The component must have a unit datum" $
    ptxOut'datum scriptOutput
      #== pcon (POutputDatum $ pcon $ PDatum (pforgetData (pdata punit)))

  pure punit
