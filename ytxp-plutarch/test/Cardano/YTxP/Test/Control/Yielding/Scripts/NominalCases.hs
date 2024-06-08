module Cardano.YTxP.Test.Control.Yielding.Scripts.NominalCases (testNominalCasesR) where

import Cardano.YTxP.Control.Yielding.Scripts (
  compileYieldingMP,
  compileYieldingSV,
  compileYieldingValidator,
 )
import Cardano.YTxP.SDK.Redeemers (AuthorisedScriptIndex (AuthorisedScriptIndex), AuthorisedScriptProofIndex (AuthorisedScriptProofIndex), AuthorisedScriptPurpose (Minting, Rewarding, Spending), YieldingRedeemer (YieldingRedeemer))
import Cardano.YTxP.SDK.SdkParameters (AuthorisedScriptsSTCS (AuthorisedScriptsSTCS))
import Cardano.YTxP.Test.Control.Yielding.Scripts.ScriptsBuilders (
  yieldingMPScriptR,
  yieldingSVScriptR,
  yieldingVScriptR,
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  ScriptsTestsParams (
    ScriptsTestsParams,
    authorisedScriptHash,
    authorisedScriptsManagerHash,
    authorisedScriptsSTCS
  ),
  authorisedScriptRefInputContext,
  mintContext,
  rewardContext,
  spendContext,
 )
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

testNominalCasesR :: Reader ScriptsTestsParams TestTree
testNominalCasesR = do
  -- Mint
  yieldingMPScript <- yieldingMPScriptR
  (mintRedeemer, mintContext') <- mintNominalCaseBuilderR
  -- Spend
  yieldingVScript <- yieldingVScriptR
  (spendRedeemer, spendContext') <- spendNominalCaseBuilderR
  -- Spend
  yieldingSVScript <- yieldingSVScriptR
  (rewardRedeemer, rewardContext') <- rewardNominalCaseBuilderR
  pure $
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
