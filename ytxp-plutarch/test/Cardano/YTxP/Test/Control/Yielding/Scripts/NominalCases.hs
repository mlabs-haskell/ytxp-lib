{-# LANGUAGE OverloadedLists #-}

module Cardano.YTxP.Test.Control.Yielding.Scripts.NominalCases (
  testNominalCasesR,
  mintNominalCaseBuilderR,
  spendNominalCaseBuilderR,
  rewardNominalCaseBuilderR,
) where

import Cardano.TestUtils (nominalCaseBasic, txfCEKUnitCase)
import Cardano.YTxP.Control.Yielding.Helper qualified as Helper
import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptIndex (AuthorisedScriptIndex),
  AuthorisedScriptProofIndex (AuthorisedScriptProofIndex),
  AuthorisedScriptPurpose (Minting, Rewarding, Spending),
  YieldingRedeemer (YieldingRedeemer),
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.ScriptsBuilders (
  yieldingScriptR,
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  ScriptsTestsParams,
  authorisedScriptRefInputContext,
  mintContext,
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
  yieldingScript <-
    yieldingScriptR
      [Helper.Spending, Helper.Minting, Helper.Rewarding]
  context' <- mintNominalCaseBuilderR
  pure $
    testGroup
      "Nominal Case"
      [ txfCEKUnitCase $
          nominalCaseBasic
            "Yielding Case"
            context'
            yieldingScript
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
