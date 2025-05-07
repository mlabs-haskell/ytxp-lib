module Utils (
  pscriptHashToCurrencySymbol,
  pmember,
  pcheck,
)
where

import Plutarch.Builtin.Unit (punit)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PMap,
  PScriptHash,
 )
import Plutarch.Unsafe (punsafeCoerce)

-- | Convert a `ScriptHash` to a `CurrencySymbol`, which has the same representation
pscriptHashToCurrencySymbol :: Term s PScriptHash -> Term s PCurrencySymbol
pscriptHashToCurrencySymbol = punsafeCoerce

-- TODO (OPTIMIZE): this can be turned into partial `phasMember` and `placksMember` variants
pmember :: (PIsData k) => Term s (k :--> PMap any k v :--> PBool)
pmember = phoistAcyclic $
  plam $ \key m ->
    precList
      ( \self x xs ->
          pif
            (pfstBuiltin # x #== pdata key)
            (pconstant True)
            (self # xs)
      )
      (const $ pconstant False)
      # pto m

pcheck ::
  forall (s :: S).
  Term s PBool ->
  Term s POpaque
pcheck b =
  pif
    b
    (popaque punit)
    perror
