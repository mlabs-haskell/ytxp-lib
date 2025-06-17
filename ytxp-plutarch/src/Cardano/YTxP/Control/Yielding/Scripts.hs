module Cardano.YTxP.Control.Yielding.Scripts (
  yielding,
) where

import Cardano.YTxP.Control.Yielding.Helper (
  AuthorisedScriptPurpose,
  yieldingHelper,
 )
import Data.Set (Set)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext)

--------------------------------------------------------------------------------
-- Plutarch level terms

-- | Yielding Validator
yielding ::
  forall (s :: S).
  Set AuthorisedScriptPurpose ->
  Term
    s
    ( PCurrencySymbol :--> PAsData PInteger :--> PScriptContext :--> PUnit
    )
yielding purposes = plam $ \psymbol _nonce ctx ->
  yieldingHelper purposes # psymbol # ctx
