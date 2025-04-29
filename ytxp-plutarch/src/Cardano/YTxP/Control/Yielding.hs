{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Module: Caradno.YTxP.Control.Stubs
Description: TODO

Add note on orphan instances. We want to have the haskell types come from the shared SDK
so that other onchain implementations can reuse those same types. This however,
forces us to declare orphan instances for haskell -> plutarch conversion
-}
module Cardano.YTxP.Control.Yielding (
  getAuthorisedScriptHash,
  PAuthorisedScriptPurpose (PMinting, PSpending, PRewarding),
  PYieldingRedeemer,
)
where

import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptIndex (AuthorisedScriptIndex),
  AuthorisedScriptProofIndex (AuthorisedScriptProofIndex),
  AuthorisedScriptPurpose,
  YieldingRedeemer,
 )
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData (PDJust, PDNothing))
import Plutarch.LedgerApi.V2 (
  PCurrencySymbol,
  PScriptHash,
  PTxInInfo,
 )
import Plutarch.Repr.Tag (DeriveAsTag (DeriveAsTag))
import Utils (pmember)

{-
TODO: Add haddock for PAuthorisedScriptIndex
-}

newtype PAuthorisedScriptIndex (s :: S) = PAuthorisedScriptIndex (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PAuthorisedScriptIndex where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PAuthorisedScriptIndex)

deriving via
  DeriveNewtypePLiftable PAuthorisedScriptIndex AuthorisedScriptIndex
  instance
    PLiftable PAuthorisedScriptIndex

{-
TODO: Add haddock for PAuthorisedScriptPurpose
-}

data PAuthorisedScriptPurpose (s :: S) = PMinting | PSpending | PRewarding
  deriving stock (Generic, Enum, Bounded)
  deriving anyclass (PIsData, PEq)
  deriving
    (PlutusType, PLiftable)
    via DeriveAsTag PAuthorisedScriptPurpose

instance SOP.Generic (PAuthorisedScriptPurpose s)

instance PTryFrom PData (PAsData PAuthorisedScriptPurpose)

-- TODO: Why do we need to wrap the args in PAsData here?
-- If we don' there is some issues with the derivation of plutus types
newtype PAuthorisedScriptProofIndex (s :: S)
  = PAuthorisedScriptProofIndex
      ( Term
          s
          (PBuiltinPair (PAsData PAuthorisedScriptPurpose) (PAsData PInteger))
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PAuthorisedScriptProofIndex where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PAuthorisedScriptProofIndex)

-- deriving via
--   DeriveNewtypePLiftable PAuthorisedScriptProofIndex AuthorisedScriptProofIndex
--   instance
--     PLiftable PAuthorisedScriptProofIndex

{-
TODO: Add haddock for PYieldingRedeemer
-}

newtype PYieldingRedeemer (s :: S)
  = PYieldingRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "authorisedScriptIndex" ':= PAuthorisedScriptIndex
               , "authorisedScriptProofIndex" ':= PAuthorisedScriptProofIndex
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PYieldingRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PYieldingRedeemer)

deriving via
  (DeriveDataPLiftable PYieldingRedeemer YieldingRedeemer)
  instance
    PLiftable PYieldingRedeemer

{- | Given a list of reference inputs and a Yielding Redeemer, dig out the authorised script hash
 by:

- Indexing the reference inputs according to the redeemer
- Checking the fetched reference input for the correct AuthorisedScriptsSTCS
- Returning the AuthorisedScriptHash
-}
getAuthorisedScriptHash ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PYieldingRedeemer
        :--> PScriptHash
    )
getAuthorisedScriptHash = phoistAcyclic $
  plam $
    \psymbol txInfoRefInputs redeemer -> unTermCont $ do
      -- TODO (OPTIMIZE): these values only get used once, can be a `let`
      yieldingRedeemer <-
        pletFieldsC @'["authorisedScriptIndex"] redeemer

      let autorisedScriptRefUTxO =
            txInfoRefInputs
              #!! pto (pfromData $ getField @"authorisedScriptIndex" yieldingRedeemer)
          output = pfield @"resolved" # autorisedScriptRefUTxO
          value = pfield @"value" # output

      pure $
        pif
          (pmember # psymbol # pto (pfromData value)) -- TODO (OPTIMIZE): make partial (`has`/`lacks`) variants and use those instead
          ( pmatch (pfield @"referenceScript" # output) $ \case
              PDJust ((pfield @"_0" #) -> autorisedScript) -> autorisedScript
              PDNothing -> (ptraceInfoError "getAuthorisedScriptHash: Reference input does not contain reference script")
          )
          (ptraceInfoError "getAuthorisedScriptHash: Reference input does not contain AuthorisedScriptsSTCS")
