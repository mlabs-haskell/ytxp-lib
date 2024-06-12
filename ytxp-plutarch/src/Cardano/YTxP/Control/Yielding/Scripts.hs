module Cardano.YTxP.Control.Yielding.Scripts (
  scripts,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Data.Map (fromList)
import Data.Text (Text, unpack)
import Plutarch (Config)
import Plutarch.LedgerApi (PCurrencySymbol, PScriptContext)
import Ply (TypedScriptEnvelope)
import Ply.Plutarch.TypedWriter (TypedWriter, mkEnvelope)
import ScriptExport.ScriptInfo (RawScriptExport (RawScriptExport))

--------------------------------------------------------------------------------
-- Raw Script Export

scripts :: Config -> RawScriptExport
scripts conf =
  RawScriptExport $
    fromList
      [ envelope "djed:yieldingValidator" yieldingValidator
      , envelope "djed:yieldingMP" yieldingMP
      , envelope "djed:yieldingSV" yieldingSV
      ]
  where
    envelope ::
      forall (pt :: S -> Type).
      (TypedWriter pt) =>
      Text ->
      ClosedTerm pt ->
      (Text, TypedScriptEnvelope)
    envelope d t = (d, either (error . unpack) id $ mkEnvelope conf d t)

--------------------------------------------------------------------------------
-- Plutarch level terms

-- | Yielding Validator
yieldingValidator ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol :--> PData :--> PData :--> PScriptContext :--> POpaque
    )
yieldingValidator = plam $ \psymbol _datum redeemer ctx ->
  yieldingHelper # psymbol # redeemer # ctx

-- | Yielding Minting Policy
yieldingMP ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol :--> PInteger :--> PData :--> PScriptContext :--> POpaque
    )
yieldingMP = plam $ \psymbol _nonce redeemer ctx ->
  yieldingHelper # psymbol # redeemer # ctx

{- | Yielding Staking Validator
Compile a yielding staking validator that has been nonced.
The nonce is required because each staking validator can only
be delegated to a single pool; the inclusion of the nonce will change the
script hash.
Since SVs and MPs share the same signature they share the same implementation,
this function is only provided for semantic clarity
-}
yieldingSV ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol :--> PInteger :--> PData :--> PScriptContext :--> POpaque
    )
yieldingSV = yieldingMP
