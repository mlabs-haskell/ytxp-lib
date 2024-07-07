module Cardano.YTxP.Test.Control.Yielding.Scripts.ScriptsBuilders (
  yieldingMPScriptR,
  yieldingVScriptR,
  yieldingSVScriptR,
) where

import Cardano.YTxP.Control.Yielding.Scripts (
  yieldingMP,
  yieldingSV,
  yieldingV,
 )
import Cardano.YTxP.SDK.SdkParameters (AuthorisedScriptsSTCS (AuthorisedScriptsSTCS))
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (ScriptsTestsParams, authorisedScriptsSTCS)
import Control.Monad.Reader (Reader, asks)
import Data.Text qualified as T
import Plutarch (
  Config (Tracing),
  LogLevel (LogInfo),
  Script,
  TracingMode (DetTracing),
  compile,
 )
import Plutarch.LedgerApi (PScriptContext)

-- | Helper that produces a @Reader@ that yields a compiled Script, throws an error is compilation fails
yieldingMPScriptR :: Reader ScriptsTestsParams Script
yieldingMPScriptR = do
  (AuthorisedScriptsSTCS authorisedScriptsSTCS') <- asks authorisedScriptsSTCS
  let
    closedTerm ::
      forall (s :: S).
      Term s (PData :--> (PScriptContext :--> POpaque))
    closedTerm = yieldingMP # pconstant authorisedScriptsSTCS' # pconstant 42
  case compile (Tracing LogInfo DetTracing) closedTerm of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'

yieldingVScriptR :: Reader ScriptsTestsParams Script
yieldingVScriptR = do
  (AuthorisedScriptsSTCS authorisedScriptsSTCS') <- asks authorisedScriptsSTCS
  let
    closedTerm ::
      forall (s :: S).
      Term s (PData :--> (PData :--> (PScriptContext :--> POpaque)))
    closedTerm = yieldingV # pconstant authorisedScriptsSTCS'
  case compile (Tracing LogInfo DetTracing) closedTerm of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'

yieldingSVScriptR :: Reader ScriptsTestsParams Script
yieldingSVScriptR = do
  (AuthorisedScriptsSTCS authorisedScriptsSTCS') <- asks authorisedScriptsSTCS
  let
    closedTerm ::
      forall (s :: S).
      Term s (PData :--> (PScriptContext :--> POpaque))
    closedTerm = yieldingSV # pconstant authorisedScriptsSTCS' # pconstant 42
  case compile (Tracing LogInfo DetTracing) closedTerm of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'
