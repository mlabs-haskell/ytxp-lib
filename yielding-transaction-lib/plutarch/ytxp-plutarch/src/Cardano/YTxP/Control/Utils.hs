module Cardano.YTxP.Control.Utils
  (unsafeTermFromScript)
  where

import Plutarch.Script (Script (Script))
import UntypedPlutusCore.Core.Type qualified as UPLC
import Plutarch.Internal (RawTerm (RCompiled), TermResult (TermResult)
                         , Term (Term))

-- | Put a compiled script into a plutarch term. The type of the
-- script is arbitrary: THIS IS UNSAFE. Make sure you know the
-- correct type! Ply can help annotate exported scripts with the correct types.
unsafeTermFromScript :: forall (a :: PType). Script -> ClosedTerm a
unsafeTermFromScript (Script script) =
  Term $ const $ pure $ TermResult (RCompiled $ UPLC._progTerm script) []
