module Utils (
  pscriptHashToCurrencySymbol,
  pmember,
  pcheck,
)
where

import Plutarch.LedgerApi.V2 (
  PCurrencySymbol (PCurrencySymbol),
  PMap,
  PScriptHash,
 )

-- | Convert a `ScriptHash` to a `CurrencySymbol`, which has the same representation
pscriptHashToCurrencySymbol :: Term s PScriptHash -> Term s PCurrencySymbol
pscriptHashToCurrencySymbol sc = pcon (PCurrencySymbol $ pto sc)

-- TODO (OPTIMIZE): this can be turned into partial `phasMember` and `placksMember` variants
pmember :: (PIsData k) => Term s (k :--> PMap any k v :--> PBool)
pmember = phoistAcyclic $
  plam $ \key m ->
    precList
      ( \self x xs ->
          pif
            (ptraceDebug "pmember: in condition"
                ((pfstBuiltin # x) #== pdata key))
            (ptraceDebug "pmember: in positive branch" $ pconstant True)
            (ptraceDebug "pmember: negative branch" $ self # xs)
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
    (popaque $ pconstant ())
    perror
