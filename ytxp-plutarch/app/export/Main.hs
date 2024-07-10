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

import Cardano.YTxP qualified as YTxP
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
          "ytxp-mp"
          (scripts NoTracing)
          YTxP.mpLinker
      , insertScriptExportWithLinker
          "ytxp-sv"
          (scripts NoTracing)
          YTxP.svLinker
      , insertScriptExportWithLinker
          "ytxp-validator-tracing"
          (scripts (Tracing LogInfo DetTracing))
          YTxP.validatorLinker
      , insertScriptExportWithLinker
          "ytxp-mp-tracing"
          (scripts (Tracing LogInfo DetTracing))
          YTxP.mpLinker
      , insertScriptExportWithLinker
          "ytxp-sv-tracing"
          (scripts (Tracing LogInfo DetTracing))
          YTxP.svLinker
      ]
