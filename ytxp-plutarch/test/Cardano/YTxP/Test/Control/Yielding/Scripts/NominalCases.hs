module Cardano.YTxP.Test.Control.Yielding.Scripts.NominalCases (
  testNominalCasesR,
  mintNominalCaseBuilderR,
  spendNominalCaseBuilderR,
  rewardNominalCaseBuilderR,
  oneshotNominalCaseBuilderR,
) where

import Cardano.TestUtils (nominalCaseBasic, txfCEKUnitCase)
import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptIndex (AuthorisedScriptIndex),
  AuthorisedScriptProofIndex (AuthorisedScriptProofIndex),
  AuthorisedScriptPurpose (Minting, Rewarding, Spending),
  YieldingRedeemer (YieldingRedeemer),
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.ScriptsBuilders (
  yieldingScriptR,
  yieldingScriptR',
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  ScriptsTestsParams,
  authorisedScriptRefInputContext,
  mintContext,
  oneshotSpendContext,
  rewardContext,
  spendContext,
 )
import Control.Monad.Reader (Reader)
import Plutus.ContextBuilder (
  Builder,
  buildMinting',
  buildRewarding',
  buildSpending',
  mkOutRefIndices,
  scriptRedeemer,
 )
import PlutusLedgerApi.V3 (ScriptContext)
import Test.Tasty (TestTree, testGroup)

testNominalCasesR :: Reader ScriptsTestsParams TestTree
testNominalCasesR = do
  yieldingScript <- yieldingScriptR
  oneshotYieldingScript <- yieldingScriptR'
  context <- mintNominalCaseBuilderR
  oneshotContext <- oneshotNominalCaseBuilderR
  pure $
    testGroup
      "Nominal Case"
      [ txfCEKUnitCase $
          nominalCaseBasic
            "Yielding Case"
            context
            yieldingScript
      , txfCEKUnitCase $
          nominalCaseBasic
            "Yielding Case (with oneshot backdoor script)"
            context
            oneshotYieldingScript
      , txfCEKUnitCase $
          nominalCaseBasic
            "Backdoor Case"
            oneshotContext
            oneshotYieldingScript
      ]

-- | Helper that produces a @Reader@ that yields a compiled a redeemerScript, throws an error is compilation fails
mkNominalCaseBuilderR ::
  (Builder a, Semigroup a) =>
  YieldingRedeemer ->
  Reader ScriptsTestsParams a ->
  (a -> ScriptContext) ->
  Reader ScriptsTestsParams ScriptContext
mkNominalCaseBuilderR redeemer builder contextBuilder = do
  context <- (<>) <$> authorisedScriptRefInputContext <*> builder
  pure $ contextBuilder $ scriptRedeemer redeemer <> mkOutRefIndices context

oneshotNominalCaseBuilderR ::
  Reader ScriptsTestsParams ScriptContext
oneshotNominalCaseBuilderR =
  buildSpending' . mkOutRefIndices <$> oneshotSpendContext

mintNominalCaseBuilderR ::
  Reader ScriptsTestsParams ScriptContext
mintNominalCaseBuilderR =
  let redeemer =
        YieldingRedeemer
          (AuthorisedScriptIndex 0)
          (AuthorisedScriptProofIndex (Minting, 0))
   in mkNominalCaseBuilderR redeemer (mintContext redeemer) buildMinting'

spendNominalCaseBuilderR ::
  Reader ScriptsTestsParams ScriptContext
spendNominalCaseBuilderR =
  let redeemer =
        YieldingRedeemer
          (AuthorisedScriptIndex 0)
          (AuthorisedScriptProofIndex (Spending, 0))
   in mkNominalCaseBuilderR redeemer spendContext buildSpending'

rewardNominalCaseBuilderR ::
  Reader ScriptsTestsParams ScriptContext
rewardNominalCaseBuilderR =
  let redeemer =
        YieldingRedeemer
          (AuthorisedScriptIndex 0)
          (AuthorisedScriptProofIndex (Rewarding, 0))
   in mkNominalCaseBuilderR redeemer rewardContext buildRewarding'
