{-# LANGUAGE OverloadedRecordDot #-}

module Cardano.YTxP.Example.Offer.Executing (
  -- * TxF Params
  Params,

  -- * Script
  executingTxF,
) where

import Cardano.YTxP.Example.Offer (POfferDatum (creator, toBuy))
import Cardano.YTxP.Example.Offer.PUtils (
  pisRewarding,
  ptraceDebugC,
  tryGetOfferInput,
 )
import Control.Monad (void)
import Plutarch.Builtin.Unit (punit)
import Plutarch.LedgerApi.V3 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PAddress (PAddress),
  PCredential (PPubKeyCredential),
  PDatum (PDatum),
  POutputDatum (POutputDatum),
  PScriptContext (pscriptContext'scriptInfo),
  PTxOut,
  PValue,
  pdnothing,
  pscriptContext'txInfo,
  ptxInfo'inputs,
  ptxInfo'mint,
  ptxInfo'outputs,
  ptxOut'address,
  ptxOut'datum,
  ptxOut'referenceScript,
  ptxOut'value,
 )
import Plutarch.LedgerApi.Value (pvalueOf)
import PlutusLedgerApi.V3 (CurrencySymbol, TokenName (TokenName))

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
executingTxF :: Params -> Term s (PScriptContext :--> PUnit)
executingTxF params = phoistAcyclic $ plam $ \context' -> unTermCont $ do
  -- ScriptContext extraction

  ptraceDebugC "ScriptContext extraction"

  context <- pmatchC context'
  txInfo' <- pletC $ pscriptContext'txInfo context
  scriptInfo <- pletC $ pscriptContext'scriptInfo context
  txInfo <- pmatchC txInfo'

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

  -- Wallet output extraction

  ptraceDebugC "Wallet output extraction"

  void $
    pmatchC $
      tryGetWalletOutput
        (pcon $ PAddress (pcon $ PPubKeyCredential $ creator offerDatum) pdnothing)
        (pfromData $ toBuy offerDatum)
        (pfromData $ ptxInfo'outputs txInfo)

  pure punit

-- | Find the first valid wallet output
tryGetWalletOutput ::
  Term s PAddress ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PTxOut)
tryGetWalletOutput walletAddress walletValue outputs =
  pmatch outputs $ \case
    PCons walletOutput' rest -> unTermCont $ do
      walletOutput <- pmatchC $ pfromData walletOutput'
      return $
        pif
          ( walletValue
              #== pfromData (ptxOut'value walletOutput)
              #&& walletAddress
              #== ptxOut'address walletOutput
              #&& pdnothing
              #== ptxOut'referenceScript walletOutput
          )
          walletOutput'
          ( pif
              (rest #== pcon PNil)
              ( ptraceInfoError
                  "tryGetWalletOutput: Offer component not found"
              )
              (tryGetWalletOutput walletAddress walletValue rest)
          )
    _other -> ptraceInfoError "tryGetWalletOutput: Empty output list"
