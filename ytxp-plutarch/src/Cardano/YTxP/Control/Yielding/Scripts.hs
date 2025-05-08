module Cardano.YTxP.Control.Yielding.Scripts (
  yielding,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext)

--------------------------------------------------------------------------------
-- Plutarch level terms

-- | Yielding Validator
yielding ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol :--> PAsData PInteger :--> PScriptContext :--> PUnit
    )
yielding = plam $ \psymbol _nonce ctx ->
  yieldingHelper # psymbol # ctx
