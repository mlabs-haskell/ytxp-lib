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

import Cardano.YTxP.Control.Vendored (DerivePConstantViaEnum (DerivePConstantEnum), PlutusTypeEnumData)
import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptIndex (AuthorisedScriptIndex),
  AuthorisedScriptProofIndex (AuthorisedScriptProofIndex),
  AuthorisedScriptPurpose,
  YieldingRedeemer,
 )
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.LedgerApi.Utils (PMaybeData (PDJust, PDNothing))
import Plutarch.LedgerApi.V2 (
  PCurrencySymbol,
  PScriptHash,
  PTxInInfo,
 )
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
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

instance PUnsafeLiftDecl PAuthorisedScriptIndex where
  type PLifted PAuthorisedScriptIndex = AuthorisedScriptIndex

deriving via
  (DerivePConstantViaNewtype AuthorisedScriptIndex PAuthorisedScriptIndex PInteger)
  instance
    (PConstantDecl AuthorisedScriptIndex)

{-
TODO: Add haddock for PAuthorisedScriptPurpose
-}

data PAuthorisedScriptPurpose (s :: S) = PMinting | PSpending | PRewarding
  deriving stock (Generic, Enum, Bounded)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PAuthorisedScriptPurpose where
  type DPTStrat _ = PlutusTypeEnumData

instance PTryFrom PData (PAsData PAuthorisedScriptPurpose)

instance PUnsafeLiftDecl PAuthorisedScriptPurpose where
  type PLifted PAuthorisedScriptPurpose = AuthorisedScriptPurpose

deriving via
  (DerivePConstantViaEnum AuthorisedScriptPurpose PAuthorisedScriptPurpose)
  instance
    (PConstantDecl AuthorisedScriptPurpose)

{-
TODO: Add haddock for PAuthorisedScriptProofIndex
-}

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

instance PUnsafeLiftDecl PAuthorisedScriptProofIndex where
  type PLifted PAuthorisedScriptProofIndex = AuthorisedScriptProofIndex

deriving via
  (DerivePConstantViaNewtype AuthorisedScriptProofIndex PAuthorisedScriptProofIndex (PBuiltinPair PAuthorisedScriptPurpose PInteger))
  instance
    (PConstantDecl AuthorisedScriptProofIndex)

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
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PYieldingRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PYieldingRedeemer)

instance PUnsafeLiftDecl PYieldingRedeemer where
  type PLifted PYieldingRedeemer = YieldingRedeemer

deriving via
  (DerivePConstantViaData YieldingRedeemer PYieldingRedeemer)
  instance
    (PConstantDecl YieldingRedeemer)

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

      let authorisedScriptRefUTxO =
            txInfoRefInputs
              #!! pto (pfromData $ getField @"authorisedScriptIndex" yieldingRedeemer)
          output = pfield @"resolved" # authorisedScriptRefUTxO
          value = pfield @"value" # output

      ptraceC $ "getAuthorizedScriptHash: txInfoRefInputs = " <> pshow txInfoRefInputs

      pure $
        pif
          (pmember # psymbol # pto (pfromData value)) -- TODO (OPTIMIZE): make partial (`has`/`lacks`) variants and use those instead
          ( pmatch (pfield @"referenceScript" # output) $ \case
              PDJust ((pfield @"_0" #) -> authorisedScript) -> authorisedScript
              PDNothing _ -> (ptraceInfoError "getAuthorisedScriptHash: Reference input does not contain reference script")
          )
          ( ptraceInfoError $
              "getAuthorisedScriptHash: Reference input does not contain AuthorisedScriptsSTCS."
                <> "  Symbol: "
                <> pshow psymbol
                <> "  Value: "
                <> pshow value
          )
