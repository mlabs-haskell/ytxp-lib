module Cardano.YTxP.Test.Control.Yielding.Scripts.NominalCases (
  testNominalCasesR,
  mintNominalCaseBuilderR,
  spendNominalCaseBuilderR,
  rewardNominalCaseBuilderR,
) where

import Cardano.YTxP.SDK.Redeemers (AuthorisedScriptIndex (AuthorisedScriptIndex), AuthorisedScriptProofIndex (AuthorisedScriptProofIndex), AuthorisedScriptPurpose (Minting, Rewarding, Spending), YieldingRedeemer (YieldingRedeemer))
import Cardano.YTxP.Test.Control.Yielding.Scripts.ScriptsBuilders (
  yieldingMPScriptR,
  yieldingSVScriptR,
  yieldingVScriptR,
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (ScriptsTestsParams, authorisedScriptRefInputContext, mintContext, rewardContext, spendContext, toLedgerRedeemer)
import Control.Monad.Reader (Reader)
import Convex.TestUtils (nominalCaseBasic, txfCEKUnitCase)
import Plutarch.Context (Builder, buildMinting', buildRewarding', buildSpending', mkOutRefIndices)
import PlutusLedgerApi.V2 (
  Datum (Datum),
  ScriptContext,
  ToData (toBuiltinData),
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
            (toLedgerRedeemer mintRedeemer)
            mintContext'
            yieldingMPScript
      , txfCEKUnitCase $
          nominalCaseBasic
            "Spend Case"
            (Just $ Datum $ toBuiltinData ())
            (toLedgerRedeemer spendRedeemer)
            spendContext'
            yieldingVScript
      , txfCEKUnitCase $
          nominalCaseBasic
            "Reward Case"
            Nothing
            (toLedgerRedeemer rewardRedeemer)
            rewardContext'
            yieldingSVScript
      ]

-- | Helper that produces a @Reader@ that yields a compiled a redeemerScript, throws an error is compilation fails
mkNominalCaseBuilderR ::
  (Builder a, Semigroup a) =>
  YieldingRedeemer ->
  Reader ScriptsTestsParams a ->
  (a -> ScriptContext) ->
  Reader ScriptsTestsParams (YieldingRedeemer, ScriptContext)
mkNominalCaseBuilderR redeemer builder contextBuilder = do
  context <- (<>) <$> authorisedScriptRefInputContext <*> builder
  pure (redeemer, contextBuilder $ mkOutRefIndices context)

mintNominalCaseBuilderR :: Reader ScriptsTestsParams (YieldingRedeemer, ScriptContext)
mintNominalCaseBuilderR =
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Minting, 1))
   in mkNominalCaseBuilderR redeemer (mintContext redeemer) buildMinting'

spendNominalCaseBuilderR :: Reader ScriptsTestsParams (YieldingRedeemer, ScriptContext)
spendNominalCaseBuilderR =
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Spending, 0))
   in mkNominalCaseBuilderR redeemer spendContext buildSpending'

rewardNominalCaseBuilderR :: Reader ScriptsTestsParams (YieldingRedeemer, ScriptContext)
rewardNominalCaseBuilderR =
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Rewarding, 0))
   in mkNominalCaseBuilderR redeemer rewardContext buildRewarding'
