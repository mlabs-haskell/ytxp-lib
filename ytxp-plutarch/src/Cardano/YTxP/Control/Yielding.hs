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

-- TODO rename

import Cardano.YTxP.Control.Vendored (DerivePConstantViaEnum (DerivePConstantEnum), PlutusTypeEnumData)
import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptIndex (AuthorisedScriptIndex),
  AuthorisedScriptProofIndex (AuthorisedScriptProofIndex),
  AuthorisedScriptPurpose,
  YieldingRedeemer,
 )
import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
 )
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.LedgerApi (PMaybeData (PDJust, PDNothing), PScriptHash, PTxInInfo, PValue)
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
  AuthorisedScriptsSTCS ->
  Term
    s
    ( PBuiltinList PTxInInfo
        :--> PYieldingRedeemer
        :--> PScriptHash
    )
getAuthorisedScriptHash authorisedScriptsSTCS = phoistAcyclic $
  plam $
    \txInfoRefInputs redeemer -> unTermCont $ do
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
          (pcontainsAuthorisedScriptSTT authorisedScriptsSTCS # value)
          ( pmatch (pfield @"referenceScript" # output) $ \case
              PDJust ((pfield @"_0" #) -> autorisedScript) -> autorisedScript
              PDNothing _ -> (ptraceInfoError "getAuthorisedScriptHash: Reference input does not contain reference script")
          )
          (ptraceInfoError "getAuthorisedScriptHash: Reference input does not contain AuthorisedScriptsSTCS")

{- | Checks that the given 'PValue' contains the YieldListSTT
TODO (OPTIMIZE): make partial (`has`/`lacks`) variants and use those instead
-}
pcontainsAuthorisedScriptSTT ::
  AuthorisedScriptsSTCS -> Term s (PValue anyKey anyAmount :--> PBool)
pcontainsAuthorisedScriptSTT (AuthorisedScriptsSTCS symbol) =
  plam $ \value ->
    pmember # pconstant symbol # pto value
