module Cardano.YTxP.Test.Control.Yielding.Scripts.ScriptsBuilders (
  yieldingMPScriptR,
  yieldingVScriptR,
  yieldingSVScriptR,
) where

import Cardano.YTxP.Control.Yielding.Scripts (yielding)
import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptPurpose (Minting, Rewarding, Spending),
 )
import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  ScriptsTestsParams,
  authorisedScriptsSTCS,
 )
import Control.Monad.Reader (Reader, asks)
import Data.Text qualified as T
import Plutarch.Internal.Term (
  Config (Tracing),
  LogLevel (LogInfo),
  Script,
  TracingMode (DetTracing),
  compile,
 )
import Plutarch.LedgerApi.V3 (PScriptContext)

-- | Helper that produces a @Reader@ that yields a compiled Script, throws an error is compilation fails
yieldingMPScriptR :: Reader ScriptsTestsParams Script
yieldingMPScriptR = do
  (AuthorisedScriptsSTCS authorisedScriptsSTCS') <- asks authorisedScriptsSTCS
  let
    closedTerm ::
      forall (s :: S).
      Term s (PScriptContext :--> PUnit)
    closedTerm = yielding Minting # pconstant authorisedScriptsSTCS' # pconstant 42
  case compile (Tracing LogInfo DetTracing) closedTerm of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'

yieldingVScriptR :: Reader ScriptsTestsParams Script
yieldingVScriptR = do
  (AuthorisedScriptsSTCS authorisedScriptsSTCS') <- asks authorisedScriptsSTCS
  let
    closedTerm ::
      forall (s :: S).
      Term s (PScriptContext :--> PUnit)
    closedTerm = yielding Spending # pconstant authorisedScriptsSTCS' # pconstant 0
  case compile (Tracing LogInfo DetTracing) closedTerm of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'

yieldingSVScriptR :: Reader ScriptsTestsParams Script
yieldingSVScriptR = do
  (AuthorisedScriptsSTCS authorisedScriptsSTCS') <- asks authorisedScriptsSTCS
  let
    closedTerm ::
      forall (s :: S).
      Term s (PScriptContext :--> PUnit)
    closedTerm = yielding Rewarding # pconstant authorisedScriptsSTCS' # pconstant 42
  case compile (Tracing LogInfo DetTracing) closedTerm of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'
