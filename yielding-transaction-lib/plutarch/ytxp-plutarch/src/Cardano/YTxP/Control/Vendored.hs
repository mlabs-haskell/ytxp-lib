{- | Vendored utilities from open source libraries.
See the appropriate License for details on usage.
-}
module Cardano.YTxP.Control.Vendored (applyScript) where

import Plutarch.Script (Script (Script))
import UntypedPlutusCore (Program (Program, _progAnn, _progTerm, _progVer))
import UntypedPlutusCore.Core.Type qualified as UplcType

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
