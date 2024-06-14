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
          "ytxp"
          (scripts NoTracing)
          YTxP.linker
      , insertScriptExportWithLinker
          "ytxp-tracing"
          (scripts (Tracing LogInfo DetTracing))
          YTxP.linker
      ]
