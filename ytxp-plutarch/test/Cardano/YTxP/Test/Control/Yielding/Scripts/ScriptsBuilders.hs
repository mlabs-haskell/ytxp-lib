module Cardano.YTxP.Test.Control.Yielding.Scripts.ScriptsBuilders (
  yieldingScriptR,
) where

import Cardano.YTxP.Control.Yielding.Scripts (yielding)
import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
 )
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (
  ScriptsTestsParams,
  authorisedScriptPurposes,
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
yieldingScriptR :: Reader ScriptsTestsParams Script
yieldingScriptR = do
  (AuthorisedScriptsSTCS authorisedScriptsSTCS') <- asks authorisedScriptsSTCS
  purposes <- asks authorisedScriptPurposes
  let
    closedTerm ::
      forall (s :: S).
      Term s (PScriptContext :--> PUnit)
    closedTerm = yielding purposes # pconstant authorisedScriptsSTCS' # pconstant 42
  case compile (Tracing LogInfo DetTracing) closedTerm of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right script' -> pure script'
