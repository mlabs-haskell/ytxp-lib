{- | Module: Caradno.YTxP.Control.Stubs
Description: TODO
-}
module Cardano.YTxP.Control.Yielding (
  getAuthorisedScriptHash,
  PAuthorisedScriptPurpose (PMinting, PSpending, PRewarding),
  PYieldingRedeemer (..),
)
where

import Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptIndex (AuthorisedScriptIndex),
  AuthorisedScriptProofIndex (AuthorisedScriptProofIndex),
  AuthorisedScriptPurpose (Minting, Rewarding, Spending),
  YieldingRedeemer,
 )
import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (LiftError (OtherLiftError))
import Plutarch.LedgerApi.Utils (PMaybeData (PDJust, PDNothing))
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptHash,
  PTxInInfo,
  PTxOut (ptxOut'value),
  ptxInInfo'resolved,
  ptxOut'referenceScript,
 )
import Plutarch.Repr.Tag (DeriveAsTag (DeriveAsTag))
import Utils (pmember)

newtype PAuthorisedScriptIndex (s :: S) = PAuthorisedScriptIndex (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveNewtypePlutusType PAuthorisedScriptIndex)
  deriving
    (PLiftable)
    via (DeriveNewtypePLiftable PAuthorisedScriptIndex AuthorisedScriptIndex)

instance PTryFrom PData (PAsData PAuthorisedScriptIndex)

data PAuthorisedScriptPurpose (s :: S) = PMinting | PSpending | PRewarding
  deriving stock (Generic, Enum, Bounded)
  deriving anyclass (SOP.Generic, PIsData, PEq)
  deriving
    (PlutusType)
    via DeriveAsTag PAuthorisedScriptPurpose

instance PLiftable PAuthorisedScriptPurpose where
  type AsHaskell PAuthorisedScriptPurpose = AuthorisedScriptPurpose
  type PlutusRepr PAuthorisedScriptPurpose = Integer

  haskToRepr = toInteger . SOP.hindex . SOP.from

  reprToPlut idx = PLifted $ popaque $ pconstant @PInteger idx

  plutToRepr p = plutToRepr @PInteger $ coerce p

  reprToHask idx
    | idx == haskToRepr @PAuthorisedScriptPurpose Minting = Right Minting
    | idx == haskToRepr @PAuthorisedScriptPurpose Spending = Right Spending
    | idx == haskToRepr @PAuthorisedScriptPurpose Rewarding = Right Rewarding
    | otherwise = Left (OtherLiftError "Invalid Index")

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
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveNewtypePlutusType PAuthorisedScriptProofIndex)
  deriving
    (PLiftable)
    via (DeriveNewtypePLiftable PAuthorisedScriptProofIndex AuthorisedScriptProofIndex)

instance PTryFrom PData (PAsData PAuthorisedScriptProofIndex)

data PYieldingRedeemer (s :: S) = PYieldingRedeemer
  { authorisedScriptIndex :: Term s (PAsData PAuthorisedScriptIndex)
  , authorisedScriptProofIndex :: Term s (PAsData PAuthorisedScriptProofIndex)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveAsDataStruct PYieldingRedeemer)
  deriving
    (PLiftable)
    via (DeriveDataPLiftable PYieldingRedeemer YieldingRedeemer)

instance PTryFrom PData (PAsData PYieldingRedeemer)

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
      PYieldingRedeemer authorisedScriptIndex _ <- pmatchC redeemer

      let autorisedScriptRefUTxO =
            txInfoRefInputs
              #!! pto (pfromData authorisedScriptIndex)

      scriptRefFields <- pmatchC $ pfromData autorisedScriptRefUTxO
      resolved' <- pletC $ ptxInInfo'resolved scriptRefFields
      resolved <- pmatchC resolved'

      let
        value = ptxOut'value resolved
        referenceScript = ptxOut'referenceScript resolved

      pure $
        pif
          (pmember # psymbol # pto (pfromData value)) -- TODO (OPTIMIZE): make partial (`has`/`lacks`) variants and use those instead
          ( pmatch referenceScript $ \case
              PDJust autorisedScript -> pfromData autorisedScript
              PDNothing ->
                ( ptraceInfoError
                    "getAuthorisedScriptHash: Reference input does not contain reference script"
                )
          )
          ( ptraceInfoError
              "getAuthorisedScriptHash: Reference input does not contain AuthorisedScriptsSTCS"
          )
