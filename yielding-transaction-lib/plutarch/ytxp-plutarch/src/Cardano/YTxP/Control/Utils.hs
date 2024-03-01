module Cardano.YTxP.Control.Utils
  (unsafeTermFromScript)
  where

import Plutarch.Internal (RawTerm (RCompiled), Term (Term),
                          TermResult (TermResult))
import Plutarch.Script (Script (Script))
import UntypedPlutusCore.Core.Type qualified as UPLC

-- | Put a compiled script into a plutarch term. The type of the
-- script is arbitrary: THIS IS UNSAFE. Make sure you know the
-- correct type! Ply can help annotate exported scripts with the correct types.
unsafeTermFromScript :: forall (a :: S -> Type). Script -> forall (s :: S). Term s a
unsafeTermFromScript (Script script) =
  Term $ const $ pure $ TermResult (RCompiled $ UPLC._progTerm script) []
