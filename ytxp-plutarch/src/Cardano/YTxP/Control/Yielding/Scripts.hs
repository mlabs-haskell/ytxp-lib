module Cardano.YTxP.Control.Yielding.Scripts (
  -- * RawScriptExport exporter

  -- scripts,

  -- * Plutarch validators
  yieldingV,
  yieldingMP,
  yieldingSV,
) where

import Cardano.YTxP.Control.Yielding.Helper (yieldingHelper)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext)

-- import ScriptExport.ScriptInfo (RawScriptExport (RawScriptExport))

--------------------------------------------------------------------------------
-- Raw Script Export

{- | Exports the yielding validator, yielding minting policy and yielding staking validator
from a given Plutarch @Config@
scripts :: Config -> RawScriptExport
scripts conf =
  RawScriptExport $
    fromList
      [ envelope "ytxp:yieldingValidator" yieldingV
      , envelope "ytxp:yieldingMintingPolicy" yieldingMP
      , envelope "ytxp:yieldingStakeValidator" yieldingSV
      ]
  where
    envelope ::
      forall (pt :: S -> Type).
      (TypedWriter pt) =>
      Text ->
      ClosedTerm pt ->
      (Text, TypedScriptEnvelope)
    envelope d t = (d, either (error . unpack) id $ mkEnvelope conf d t)
-}

--------------------------------------------------------------------------------
-- Plutarch level terms

-- | Yielding Validator
yieldingV ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol :--> PData :--> PData :--> PScriptContext :--> POpaque
    )
yieldingV = plam $ \psymbol _datum redeemer ctx ->
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

-- | Yielding Staking Validator
yieldingSV ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol :--> PInteger :--> PData :--> PScriptContext :--> POpaque
    )
yieldingSV = yieldingMP
