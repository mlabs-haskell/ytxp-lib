module Cardano.YTxP.Control.Yielding.Scripts (
  yielding,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Cardano.YTxP.SDK.Redeemers (AuthorisedScriptPurpose)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext)

yielding ::
  forall (s :: S).
  AuthorisedScriptPurpose ->
  Term
    s
    ( PCurrencySymbol :--> PAsData PInteger :--> PScriptContext :--> PUnit
    )
yielding purpose = plam $ \psymbol _nonce ctx ->
  yieldingHelper purpose # psymbol # ctx
