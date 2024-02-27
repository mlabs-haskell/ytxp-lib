{- | Vendored utilities from open source libraries.
See the appropriate License for details on usage.
-}
module Cardano.YTxP.Control.Vendored where

import Data.Text (Text)
import Data.Text qualified as Text
import Plutarch (
  Config (Config, tracingMode),
  TracingMode (DetTracing, NoTracing),
  compile,
 )
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Script (
  Script (Script),
 )
import PlutusLedgerApi.V1 (Data, ExBudget)
import UntypedPlutusCore (
  Program (
    Program,
    _progAnn,
    _progTerm,
    _progVer
  ),
 )
import UntypedPlutusCore.Core.Type qualified as UplcType
import UntypedPlutusCore.Evaluation.Machine.Cek (
  CekUserError (CekEvaluationFailure, CekOutOfExError),
  ErrorWithCause (ErrorWithCause),
  EvaluationError (InternalEvaluationError, UserEvaluationError),
 )

{- | Apply a function to an argument on the compiled 'Script' level.

Vendored from LPE
TODO: Licensing info

 @since 3.8.0
-}
applyScript :: Script -> Script -> Script
applyScript f a =
  if fVer /= aVer
    then error "apply: Plutus Core version mismatch"
    else
      Script
        Program
          { _progTerm = UplcType.Apply () fTerm aTerm
          , _progVer = fVer
          , _progAnn = ()
          }
  where
    (Script Program {_progTerm = fTerm, _progVer = fVer}) = f
    (Script Program {_progTerm = aTerm, _progVer = aVer}) = a
