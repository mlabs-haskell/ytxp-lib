{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  -- * Yielding Script tests static parameters
  ScriptsTestsParams (
    ScriptsTestsParams,
    authorisedScriptsSTCS,
    authorisedScriptHash,
    authorisedScriptsManagerHash
  ),

  -- * Script context builders
  authorisedScriptRefInputContext,
  mintContext,
  spendContext,
  rewardContext,
) where

import Cardano.YTxP.Control.Yielding.Scripts (
  compileYieldingMP,
  compileYieldingSV,
  compileYieldingValidator,
 )
import Cardano.YTxP.SDK.Redeemers (AuthorisedScriptIndex (AuthorisedScriptIndex), AuthorisedScriptProofIndex (AuthorisedScriptProofIndex), AuthorisedScriptPurpose (Minting, Rewarding, Spending), YieldingRedeemer (YieldingRedeemer))
import Cardano.YTxP.SDK.SdkParameters (AuthorisedScriptsSTCS (AuthorisedScriptsSTCS))
import Control.Monad.Reader (Reader, asks)

import Plutarch (
  Config (Tracing),
  LogLevel (LogInfo),
  Script,
  TracingMode (DetTracing),
 )
import Plutarch.Context (Builder, MintingBuilder, RewardingBuilder, SpendingBuilder, input, mintSingletonWith, referenceInput, script, withMinting, withReferenceScript, withRewarding, withSpendingUTXO, withValue, withdrawal)
import PlutusLedgerApi.V2 (
  Credential (ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  ScriptHash (getScriptHash),
  StakingCredential (StakingHash),
  singleton,
 )

-- | Yielding Script tests static parameters
data ScriptsTestsParams = ScriptsTestsParams
  { authorisedScriptsSTCS :: AuthorisedScriptsSTCS
  , authorisedScriptHash :: ScriptHash
  , authorisedScriptsManagerHash :: ScriptHash
  }

-- | Produces a @Reader@ that yields a context builder with an _authorised_ reference using @ScriptsTestsParams@
authorisedScriptRefInputContext :: (Builder a) => Reader ScriptsTestsParams a
authorisedScriptRefInputContext = do
  authorisedScriptsManagerHash' <- asks authorisedScriptsManagerHash
  AuthorisedScriptsSTCS authorisedScriptsSTCS' <- asks authorisedScriptsSTCS
  authorisedScriptHash' <- asks authorisedScriptHash
  return $
    referenceInput $
      script authorisedScriptsManagerHash'
        <> withValue (singleton authorisedScriptsSTCS' "" 1)
        <> withReferenceScript authorisedScriptHash'

-- | Produces a @Reader@ that yields a context builder for a minting use case
mintContext :: YieldingRedeemer -> Reader ScriptsTestsParams MintingBuilder
mintContext redeemer = do
  authorisedMintingPolicy <- asks $ CurrencySymbol . getScriptHash . authorisedScriptHash
  return $
    mintSingletonWith redeemer authorisedMintingPolicy "token name" 42
      <> withMinting authorisedMintingPolicy

-- | Produces a @Reader@ that yields a context builder for a spending use case
spendContext :: Reader ScriptsTestsParams SpendingBuilder
spendContext = do
  authorisedValidator <- asks authorisedScriptHash
  let consumedUTxO =
        script authorisedValidator
          <> withValue (singleton "33333333333333333333333333333333333333333333333333333333" "token name" 43)
  return $
    input consumedUTxO <> withSpendingUTXO consumedUTxO

-- | Produces a @Reader@ that yields a context builder for a rewarding use case
rewardContext :: Reader ScriptsTestsParams RewardingBuilder
rewardContext = do
  authorisedStakingValidator <- asks authorisedScriptHash
  let stakingCredentials = StakingHash $ ScriptCredential authorisedStakingValidator
  return $
    withdrawal stakingCredentials 0
      <> withRewarding stakingCredentials
