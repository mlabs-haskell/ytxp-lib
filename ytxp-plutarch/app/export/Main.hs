module Main (main) where

import Plutarch (
  Config (NoTracing, Tracing),
  LogLevel (LogInfo),
  TracingMode (DetTracing),
 )

import ScriptExport.Export (exportMain)
import ScriptExport.Types (
  insertScriptExportWithLinker,
 )

import qualified Cardano.YTxP as YTxP
import Cardano.YTxP.Control.Yielding.Scripts (scripts)

main :: IO ()
main =
  exportMain $
    mconcat
      [ insertScriptExportWithLinker
          "ytxp-validator"
          (scripts NoTracing)
          YTxP.validatorLinker
      , insertScriptExportWithLinker
          "ytxp-mintingPolicy"
          (scripts NoTracing)
          YTxP.mintingPolicyLinker
      , insertScriptExportWithLinker
          "ytxp-stakeValidator"
          (scripts NoTracing)
          YTxP.stakeValidatorLinker
      , insertScriptExportWithLinker
          "ytxp-validator-tracing"
          (scripts (Tracing LogInfo DetTracing))
          YTxP.validatorLinker
      , insertScriptExportWithLinker
          "ytxp-mintingPolicy-tracing"
          (scripts (Tracing LogInfo DetTracing))
          YTxP.mintingPolicyLinker
      , insertScriptExportWithLinker
          "ytxp-stakeValidator-tracing"
          (scripts (Tracing LogInfo DetTracing))
          YTxP.stakeValidatorLinker
      ]
