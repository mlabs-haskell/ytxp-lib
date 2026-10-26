module Cardano.YTxP.Control.Yielding.Scripts (
  PYieldingScript,
  yielding,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext)

--------------------------------------------------------------------------------
-- Plutarch level terms

{- | The yielding validator with its parameters.

Both parameters are @Data@-encoded, so they can be applied with the standard
tooling.
-}
type PYieldingScript =
  PAsData PCurrencySymbol :--> PAsData PInteger :--> PScriptContext :--> PUnit

-- | Yielding Validator.
yielding :: forall (s :: S). Term s PYieldingScript
yielding = plam $ \psymbol _nonce ctx ->
  yieldingHelper # pfromData psymbol # ctx
