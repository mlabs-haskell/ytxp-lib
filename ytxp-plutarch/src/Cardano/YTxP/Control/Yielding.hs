{- | Module: Cardano.YTxP.Control.Yielding
Description: This module provides functionality for managing yielding transactions
in the Yielding Transaction Protocol (YTxP). It includes types and functions for
working with authorised scripts, yielding redeemers, and retrieving script hashes.

The module defines the following key components:

- 'PAuthorisedScriptPurpose': An enumeration representing the purpose of an
  authorised script, which can be minting, spending, or rewarding.

- 'PYieldingRedeemer': A data type representing the redeemer for yielding
  transactions. It includes an index for the authorised script and a proof index.

- 'getAuthorisedScriptHash': A function that retrieves the hash of an authorised
  script based on the provided currency symbol, transaction reference inputs,
  and yielding redeemer.

This module is part of the YTxP SDK and is designed to be used in conjunction
with other modules in the SDK to facilitate the creation and validation of
yielding transactions.
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

-- | A newtype wrapper for 'AuthorisedScriptIndex'.
newtype PAuthorisedScriptIndex (s :: S) = PAuthorisedScriptIndex (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveNewtypePlutusType PAuthorisedScriptIndex)
  deriving
    (PLiftable)
    via (DeriveNewtypePLiftable PAuthorisedScriptIndex AuthorisedScriptIndex)

instance PTryFrom PData (PAsData PAuthorisedScriptIndex)

-- | A data type representing the purpose of an authorised script.
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

-- | A newtype wrapper for 'AuthorisedScriptProofIndex'.
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

-- | A data type representing the redeemer for yielding transactions.
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

{- | Retrieves the hash of an authorised script based on the provided currency symbol,
transaction reference inputs, and yielding redeemer.
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
