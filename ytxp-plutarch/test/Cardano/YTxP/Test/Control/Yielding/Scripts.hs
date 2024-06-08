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

-- *** Builders

mkNominalCaseBuilderR ::
  (Builder a, Semigroup a) =>
  YieldingRedeemer ->
  Reader ScriptsTestsParams a ->
  (a -> ScriptContext) ->
  Reader ScriptsTestsParams (Redeemer, ScriptContext)
mkNominalCaseBuilderR redeemer builder contextBuilder = do
  context <- (<>) <$> authorisedScriptRefInputContext <*> builder
  let contextWithOutRefIdx = mkOutRefIndices context
  pure (toLedgerRedeemer redeemer, contextBuilder contextWithOutRefIdx)

mintNominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
mintNominalCaseBuilderR =
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Minting, 1))
   in mkNominalCaseBuilderR redeemer (mintTokenContext redeemer) buildMinting'

spendNominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
spendNominalCaseBuilderR =
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Spending, 0))
   in mkNominalCaseBuilderR redeemer spendTokenContext buildSpending'

rewardNominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
rewardNominalCaseBuilderR =
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Rewarding, 0))
   in mkNominalCaseBuilderR redeemer rewardTokenContext buildRewarding'

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
