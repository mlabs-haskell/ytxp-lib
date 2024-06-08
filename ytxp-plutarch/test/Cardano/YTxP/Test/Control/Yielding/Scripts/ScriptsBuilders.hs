module Cardano.YTxP.Test.Control.Yielding.Scripts.ScriptsBuilders (
  yieldingMPScriptR,
  yieldingVScriptR,
  yieldingSVScriptR,
) where

import Cardano.YTxP.Control.Yielding.Scripts (
  compileYieldingMP,
  compileYieldingSV,
  compileYieldingValidator,
 )
import Cardano.YTxP.SDK.SdkParameters (AuthorisedScriptsSTCS)
import Cardano.YTxP.Test.Control.Yielding.Scripts.Utils (ScriptsTestsParams, authorisedScriptsSTCS)
import Control.Monad.Reader (Reader, asks)
import Data.Text (Text)
import Data.Text qualified as T
import Plutarch (
  Config (Tracing),
  LogLevel (LogInfo),
  Script,
  TracingMode (DetTracing),
 )

-- | Helper that produces a @Reader@ that yields a compiled Script, throws an error is compilation fails
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
