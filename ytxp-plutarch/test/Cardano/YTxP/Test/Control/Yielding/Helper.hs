module Cardano.YTxP.Test.Control.Yielding.Helper (tests) where

import Cardano.YTxP.Control.Yielding (AuthorisedScriptIndex (AuthorisedScriptIndex), AuthorisedScriptProofIndex (AuthorisedScriptProofIndex), AuthorisedScriptPurpose (Minting), YieldingRedeemer (YieldingRedeemer))
import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Cardano.YTxP.SDK.SdkParameters (
  -- TODO rename
  YieldListSTCS (YieldListSTCS),
 )
import Data.Either (fromRight)
import Data.String (IsString)
import Plutarch (
  Config (Tracing),
  LogLevel (LogInfo),
  Script,
  TracingMode (DetTracing),
  compile,
 )
import Plutarch.Context (Builder, MintingBuilder, SpendingBuilder, buildMinting, mintSingletonWith, mkOutRefIndices, referenceInput, script, withMinting, withReferenceScript, withSpendingUTXO, withValue)
import Plutarch.Evaluate (evalScript)
import Plutarch.Extra.Script (applyArguments)
import Plutarch.Test.Precompiled (
  tryFromPTerm,
  (@>),
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  ScriptContext,
  ScriptHash,
  singleton,
 )
import PlutusTx (Data)
import PlutusTx qualified
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

-- TODO: this is a draft, the script is recompiled every time
failsWithExpectedTrace :: ClosedTerm a -> [Data] -> String -> TestTree
failsWithExpectedTrace term args expectedTrace =
  case evalScript appliedScript of
    (Left _, _, trace) -> testCase "The script failed, the trace is the expected one" $ show trace @?= expectedTrace
    (Right _, _, _) -> error "A failure is expected"
  where
    -- NOTE: we always compile the script with tracingMode = DetTracing
    debugScript :: Script
    debugScript =
      fromRight
        (error "Couldn't compile the script")
        (compile (Tracing LogInfo DetTracing) term)
    appliedScript = applyArguments debugScript args

tests :: TestTree
tests = yieldingHelperTestFails

yieldingHelperTestFails :: TestTree
yieldingHelperTestFails = failsWithExpectedTrace (yieldingHelper $ YieldListSTCS authorisedScriptSTCS) [PlutusTx.toData testRedeemer, PlutusTx.toData mintFromAuthorisedScript] "HERE WE PUT THE EXPECTED TRACE"

_yieldingHelperTest :: TestTree
_yieldingHelperTest = tryFromPTerm "yielding helper" (yieldingHelper $ YieldListSTCS authorisedScriptSTCS) $ do
  [PlutusTx.toData testRedeemer, PlutusTx.toData mintFromAuthorisedScript] @> "It should mint a token from the yielding script"

testRedeemer :: YieldingRedeemer
testRedeemer = YieldingRedeemer (AuthorisedScriptIndex 0) (AuthorisedScriptProofIndex (Minting, 0))

authorisedScriptValidator :: ScriptHash
authorisedScriptValidator = "11111111111111111111111111111111111111111111111111111111"

authorisedScript :: (IsString a) => a
authorisedScript = "22222222222222222222222222222222222222222222222222222222"

authorisedScriptSTCS :: CurrencySymbol
authorisedScriptSTCS = "bb"

commonContext :: (Monoid a, Builder a) => a -> a
commonContext rest =
  mkOutRefIndices $
    mconcat
      [ referenceInput $
          script authorisedScriptValidator
            <> withValue (singleton authorisedScriptSTCS "" 1)
            <> withReferenceScript "22222222222222222222222222222222222222222222222222222221"
      , rest
      ]

mintTokenCtx :: MintingBuilder
mintTokenCtx = mintSingletonWith testRedeemer authorisedScript "hello" 333 <> withMinting authorisedScript

mintingContext :: MintingBuilder
mintingContext = commonContext mintTokenCtx

_spendingContext :: SpendingBuilder
_spendingContext = commonContext $ withSpendingUTXO (withValue (singleton "cc" "hello" 123))

mintFromAuthorisedScript :: ScriptContext
mintFromAuthorisedScript = buildMinting mempty mintingContext
