module Cardano.YTxP.Control.Yielding.Scripts (
  yielding,
  yielding',
) where

import Cardano.YTxP.Control.Yielding.Helper (
  oneshotHelper,
  yieldingHelper,
 )
import Plutarch.Builtin.Unit (punit)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PTxOutRef,
 )

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

-- | Yielding Validator with one shot backdoor
yielding' ::
  forall (s :: S).
  Term
    s
    ( PTxOutRef
        :--> PCurrencySymbol
        :--> PAsData PInteger
        :--> PScriptContext
        :--> PUnit
    )
yielding' = plam $ \oref psymbol _nonce ctx ->
  pif
    (oneshotHelper # oref # ctx)
    punit
    (yieldingHelper # psymbol # ctx)
