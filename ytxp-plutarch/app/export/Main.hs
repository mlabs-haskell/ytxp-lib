module Main (main) where

import Plutarch (
  Config (NoTracing),
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
    insertScriptExportWithLinker "ytxp" (scripts NoTracing) YTxP.linker params
