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
import Cardano.YTxP.Test.Control.Yielding.Scripts.NominalCases (testNominalCasesR)
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  ScriptsTestsParams (
    ScriptsTestsParams,
    authorisedScriptHash,
    authorisedScriptsManagerHash,
    authorisedScriptsSTCS
  ),
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
testsR =
  let tests' = [testNominalCasesR]
   in testGroup "YieldingScripts" <$> sequence tests'
