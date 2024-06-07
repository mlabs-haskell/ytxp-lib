{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.YTxP.Test.Control.Yielding.Scripts (tests) where

import Cardano.YTxP.Control.Yielding.Scripts (
  compileYieldingMP,
  compileYieldingSV,
  compileYieldingValidator,
 )
import Cardano.YTxP.SDK.Redeemers (AuthorisedScriptIndex (AuthorisedScriptIndex), AuthorisedScriptProofIndex (AuthorisedScriptProofIndex), AuthorisedScriptPurpose (Minting, Rewarding, Spending), YieldingRedeemer (YieldingRedeemer))
import Cardano.YTxP.SDK.SdkParameters (AuthorisedScriptsSTCS (AuthorisedScriptsSTCS))
import Control.Monad.Reader (Reader, asks, runReader)
import Convex.TestUtils (nominalCaseBasic, txfCEKUnitCase)
import Data.Text qualified as T
import Plutarch (
  Config (Tracing),
  LogLevel (LogInfo),
  Script,
  TracingMode (DetTracing),
 )
import Plutarch.Context (Builder, MintingBuilder, RewardingBuilder, SpendingBuilder, buildMinting', buildRewarding', buildSpending', input, mintSingletonWith, mkOutRefIndices, referenceInput, script, withMinting, withReferenceScript, withRewarding, withSpendingUTXO, withValue, withdrawal)
import PlutusLedgerApi.V2 (
  Credential (ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  Datum (Datum),
  Redeemer (Redeemer),
  ScriptContext,
  ScriptHash (getScriptHash),
  StakingCredential (StakingHash),
  ToData (toBuiltinData),
  singleton,
 )
import Test.Tasty (TestTree, testGroup)

data ScriptsTestsParams = ScriptsTestsParams
  { authorisedScriptsSTCS :: AuthorisedScriptsSTCS
  , authorisedScriptHash :: ScriptHash
  , authorisedScriptsManagerHash :: ScriptHash
  }

dummyParams :: ScriptsTestsParams
dummyParams =
  ScriptsTestsParams
    { authorisedScriptHash = "22222222222222222222222222222222222222222222222222222222"
    , authorisedScriptsSTCS = AuthorisedScriptsSTCS "bb"
    , authorisedScriptsManagerHash = "11111111111111111111111111111111111111111111111111111111"
    }

tests :: TestTree
tests = runReader testsR dummyParams

testsR :: Reader ScriptsTestsParams TestTree
testsR = do
  -- Mint
  yieldingMPScript <- yieldingMPScriptR
  (mintRedeemer, mintContext) <- mintNominalCaseBuilderR
  -- Spend
  yieldingVScript <- yieldingVScriptR
  (spendRedeemer, spendContext) <- spendNominalCaseBuilderR
  -- Spend
  yieldingSVScript <- yieldingSVScriptR
  (rewardRedeemer, rewardContext) <- rewardNominalCaseBuilderR
  let
    nominalCases :: TestTree
    nominalCases =
      testGroup
        "Nominal Case"
        [ txfCEKUnitCase $
            nominalCaseBasic
              "Mint Case"
              Nothing
              mintRedeemer
              mintContext
              yieldingMPScript
        , txfCEKUnitCase $
            nominalCaseBasic
              "Spend Case"
              (Just $ Datum $ toBuiltinData ())
              spendRedeemer
              spendContext
              yieldingVScript
        , txfCEKUnitCase $
            nominalCaseBasic
              "Reward Case"
              Nothing
              rewardRedeemer
              rewardContext
              yieldingSVScript
        ]
  pure $
    testGroup
      "YieldingScripts"
      [nominalCases]

-- *** Scripts Readers

yieldingMPScriptR :: Reader ScriptsTestsParams Script
yieldingMPScriptR = do
  authorisedScriptsSTCS' <- asks authorisedScriptsSTCS
  -- NOTE: we use an arbitrary nounce (42)
  case compileYieldingMP (Tracing LogInfo DetTracing) authorisedScriptsSTCS' 42 of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'

yieldingVScriptR :: Reader ScriptsTestsParams Script
yieldingVScriptR = do
  authorisedScriptsSTCS' <- asks authorisedScriptsSTCS
  case compileYieldingValidator (Tracing LogInfo DetTracing) authorisedScriptsSTCS' of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'

yieldingSVScriptR :: Reader ScriptsTestsParams Script
yieldingSVScriptR = do
  authorisedScriptsSTCS' <- asks authorisedScriptsSTCS
  -- NOTE: we use an arbitrary nounce (42)
  case compileYieldingSV (Tracing LogInfo DetTracing) authorisedScriptsSTCS' 42 of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'

-- *** Builders

mintNominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
mintNominalCaseBuilderR = do
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Minting, 1))
  context <- (<>) <$> authorisedScriptRefInputContext <*> mintTokenContext redeemer
  let contextWithOutRefIdx = mkOutRefIndices context
  pure (toLedgerRedeemer redeemer, buildMinting' contextWithOutRefIdx)

spendNominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
spendNominalCaseBuilderR = do
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Spending, 0))
  context <- (<>) <$> authorisedScriptRefInputContext <*> spendTokenContext
  let contextWithOutRefIdx = mkOutRefIndices context
  pure (toLedgerRedeemer redeemer, buildSpending' contextWithOutRefIdx)

rewardNominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
rewardNominalCaseBuilderR = do
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Rewarding, 0))
  -- TODO
  context <- (<>) <$> authorisedScriptRefInputContext <*> rewardTokenContext
  -- TODO what is this
  let contextWithOutRefIdx = mkOutRefIndices context
  pure (toLedgerRedeemer redeemer, buildRewarding' contextWithOutRefIdx)

-- *** Builders utils

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

-- TODO renmae these
mintTokenContext :: YieldingRedeemer -> Reader ScriptsTestsParams MintingBuilder
mintTokenContext redeemer = do
  authorisedMintingPolicy <- asks $ toCurrencySymbol . authorisedScriptHash
  return $
    mintSingletonWith redeemer authorisedMintingPolicy "token name" 42
      <> withMinting authorisedMintingPolicy

spendTokenContext :: Reader ScriptsTestsParams SpendingBuilder
spendTokenContext = do
  authorisedValidator <- asks authorisedScriptHash
  let consumedUTxO =
        script authorisedValidator
          <> withValue (singleton "33333333333333333333333333333333333333333333333333333333" "token name" 43)
  return $
    input consumedUTxO <> withSpendingUTXO consumedUTxO

rewardTokenContext :: Reader ScriptsTestsParams RewardingBuilder
rewardTokenContext = do
  authorisedStakingValidator <- asks authorisedScriptHash
  let stakingCredentials = StakingHash $ ScriptCredential authorisedStakingValidator
  return $
    withdrawal stakingCredentials 0
      <> withRewarding stakingCredentials

-- *** Misc

-- TODO do we need this in the sdk?
toLedgerRedeemer :: YieldingRedeemer -> Redeemer
toLedgerRedeemer = Redeemer . toBuiltinData

-- TODO (I guess this util already exist but I don't know where that is)
toCurrencySymbol :: ScriptHash -> CurrencySymbol
toCurrencySymbol = CurrencySymbol . getScriptHash
