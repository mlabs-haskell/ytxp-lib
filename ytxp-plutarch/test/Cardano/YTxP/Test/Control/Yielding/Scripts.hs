{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}
module Cardano.YTxP.Test.Control.Yielding.Scripts (tests) where

import Cardano.YTxP.Control.Yielding.Scripts (
  compileYieldingMP,
 )
import Cardano.YTxP.SDK.Redeemers (AuthorisedScriptIndex (AuthorisedScriptIndex), AuthorisedScriptProofIndex (AuthorisedScriptProofIndex), AuthorisedScriptPurpose (Minting), YieldingRedeemer (YieldingRedeemer))
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
import Plutarch.Context (Builder, MintingBuilder, buildMinting, mintSingletonWith, mkOutRefIndices, referenceInput, script, withMinting, withRefTxId, withReferenceScript, withValue)
import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
  Redeemer (Redeemer),
  ScriptContext,
  ScriptHash (getScriptHash),
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
  (redeemer, context) <- nominalCaseBuilderR
  script' <- scriptR
  let
    nominalCase :: TestTree
    nominalCase =
      testGroup
        "Nominal Case"
        [ txfCEKUnitCase $
            nominalCaseBasic
              "Nominal Case"
              Nothing
              redeemer
              context
              script'
        ]
  pure $
    testGroup
      "YieldingHelper"
      [nominalCase]

scriptR :: Reader ScriptsTestsParams Script
scriptR = do
  authorisedScriptsSTCS' <- asks authorisedScriptsSTCS
  case compileYieldingMP (Tracing LogInfo DetTracing) authorisedScriptsSTCS' 42 of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'

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

mintTokenContext :: YieldingRedeemer -> Reader ScriptsTestsParams MintingBuilder
mintTokenContext redeemer = do
  authorisedMintingPolicy <- asks $ toCurrencySymbol . authorisedScriptHash
  return $
    mintSingletonWith redeemer authorisedMintingPolicy "token name" 42
      <> withMinting authorisedMintingPolicy

-- TODO rename minting or pass the context part for the other nominal cases
nominalCaseBuilderR :: Reader ScriptsTestsParams (Redeemer, ScriptContext)
nominalCaseBuilderR = do
  let redeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Minting, 1))
  context <- (<>) <$> authorisedScriptRefInputContext <*> mintTokenContext redeemer
  let contextWithOutRefIdx = mkOutRefIndices context
  pure (toLedgerRedeemer redeemer, buildMinting mempty contextWithOutRefIdx)

-- TODO do we need this in the sdk?
toLedgerRedeemer :: YieldingRedeemer -> Redeemer
toLedgerRedeemer = Redeemer . toBuiltinData

-- TODO (I guess this util already exist but I don't know where that is)
toCurrencySymbol :: ScriptHash -> CurrencySymbol
toCurrencySymbol = CurrencySymbol . getScriptHash
