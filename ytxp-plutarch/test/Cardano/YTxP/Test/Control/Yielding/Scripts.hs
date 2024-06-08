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
import Data.Text (Text)
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

--------------------------------------------------------------------------------
-- Data

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

--------------------------------------------------------------------------------
-- Tests

tests :: TestTree
tests = runReader testsR dummyParams

testsR :: Reader ScriptsTestsParams TestTree
testsR = do
  -- Mint
  yieldingMPScript <- yieldingMPScriptR
  (mintRedeemer, mintContext') <- mintNominalCaseBuilderR
  -- Spend
  yieldingVScript <- yieldingVScriptR
  (spendRedeemer, spendContext') <- spendNominalCaseBuilderR
  -- Spend
  yieldingSVScript <- yieldingSVScriptR
  (rewardRedeemer, rewardContext') <- rewardNominalCaseBuilderR
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
              mintContext'
              yieldingMPScript
        , txfCEKUnitCase $
            nominalCaseBasic
              "Spend Case"
              (Just $ Datum $ toBuiltinData ())
              spendRedeemer
              spendContext'
              yieldingVScript
        , txfCEKUnitCase $
            nominalCaseBasic
              "Reward Case"
              Nothing
              rewardRedeemer
              rewardContext'
              yieldingSVScript
        ]
  pure $
    testGroup
      "YieldingScripts"
      [nominalCases]

--------------------------------------------------------------------------------
-- Scripts builders

-- | Helper that produces a @Reader@ that yields a compiled Script, throws an error is compilation fails
mkYieldingScriptR ::
  (Config -> AuthorisedScriptsSTCS -> Either Text Script) ->
  Reader ScriptsTestsParams Script
mkYieldingScriptR compile = do
  authorisedScriptsSTCS' <- asks authorisedScriptsSTCS
  case compile (Tracing LogInfo DetTracing) authorisedScriptsSTCS' of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'

yieldingMPScriptR :: Reader ScriptsTestsParams Script
yieldingMPScriptR =
  let compile config stcs = compileYieldingMP config stcs 42
   in mkYieldingScriptR compile

yieldingVScriptR :: Reader ScriptsTestsParams Script
yieldingVScriptR = mkYieldingScriptR compileYieldingValidator

yieldingSVScriptR :: Reader ScriptsTestsParams Script
yieldingSVScriptR =
  let compile config stcs = compileYieldingSV config stcs 42
   in mkYieldingScriptR compile

--------------------------------------------------------------------------------
-- Context Builders

-- | Helper that produces a @Reader@ that yields a compiled a redeemerScript, throws an error is compilation fails
mkNominalCaseBuilderR ::
  (Builder a, Semigroup a) =>
  YieldingRedeemer ->
  Reader ScriptsTestsParams a ->
  (a -> ScriptContext) ->
  Reader ScriptsTestsParams (Redeemer, ScriptContext)
mkNominalCaseBuilderR redeemer builder contextBuilder = do
  context <- (<>) <$> authorisedScriptRefInputContext <*> builder
  let toLedgerRedeemer :: YieldingRedeemer -> Redeemer
      toLedgerRedeemer = Redeemer . toBuiltinData
      contextWithOutRefIdx = mkOutRefIndices context
  pure (toLedgerRedeemer redeemer, contextBuilder contextWithOutRefIdx)

mintNominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
mintNominalCaseBuilderR =
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Minting, 1))
   in mkNominalCaseBuilderR redeemer (mintContext redeemer) buildMinting'

spendNominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
spendNominalCaseBuilderR =
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Spending, 0))
   in mkNominalCaseBuilderR redeemer spendContext buildSpending'

rewardNominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
rewardNominalCaseBuilderR =
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Rewarding, 0))
   in mkNominalCaseBuilderR redeemer rewardContext buildRewarding'

--------------------------------------------------------------------------------
-- Context Builders Helpers

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
