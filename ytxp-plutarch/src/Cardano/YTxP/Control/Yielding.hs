{- | Module: Cardano.YTxP.Control.Yielding
Description: This module provides types and utilities for handling yielding scripts in the YTxP protocol.

The module exports types related to authorised scripts and their purposes, as well as
redeemer types for yielding operations. The primary function 'getAuthorisedScriptHash'
provides a way to retrieve the authorised script hash from reference inputs.

The types in this module are designed to work with both Haskell and Plutarch representations,
facilitating seamless integration between on-chain validation and off-chain application logic.
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
  AuthorisedScriptPurpose (Minting, Rewarding, Spending),
  YieldingRedeemer,
 )
import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (LiftError (OtherLiftError))
import Plutarch.LedgerApi.Utils (PMaybeData (PDJust, PDNothing))
import Plutarch.LedgerApi.V2 (
  PCurrencySymbol,
  PScriptHash,
  PTxInInfo,
  PTxOut (ptxOut'value),
  ptxInInfo'resolved,
  ptxOut'referenceScript,
 )
import Plutarch.Repr.Data (DeriveAsDataStruct (DeriveAsDataStruct))
import Plutarch.Repr.Tag (DeriveAsTag (DeriveAsTag))
import Utils (pmember)

{- | Newtype representing an index into an authorised script.

This type is used to reference specific authorised scripts in a collection.
It wraps a 'PInteger' to provide a clear semantic meaning.
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

{- | Represents the purpose of an authorised script.

This type can be one of three possibilities:

- 'PMinting': Indicates the script is used for minting operations
- 'PSpending': Indicates the script is used for spending operations
- 'PRewarding': Indicates the script is used for rewarding operations

The type provides a clear semantic distinction between different script purposes,
which is essential for proper transaction validation and script execution.
-}
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
  deriving (PLiftable) via (DeriveNewtypePLiftable PAuthorisedScriptProofIndex AuthorisedScriptProofIndex)

instance PTryFrom PData (PAsData PAuthorisedScriptProofIndex)

{- | Redeemer type for yielding operations.

This type represents the data needed to validate a yielding transaction.

It contains:
1. 'authorisedScriptIndex': The index referencing the authorised script
2. 'authorisedScriptProofIndex': The proof index for the authorised script

The type provides a consistent interface for handling yielding operations across
both on-chain validation and off-chain transaction construction.
-}
data PYieldingRedeemer (s :: S) = PYieldingRedeemer
  { authorisedScriptIndex :: Term s (PAsData PAuthorisedScriptIndex)
  , authorisedScriptProofIndex :: Term s (PAsData PAuthorisedScriptProofIndex)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveAsDataStruct PYieldingRedeemer)
  deriving (PLiftable) via (DeriveDataPLiftable PYieldingRedeemer YieldingRedeemer)

instance PTryFrom PData (PAsData PYieldingRedeemer)

{- | Retrieves the authorised script hash from reference inputs using a Yielding Redeemer.

This function performs the following steps:

1. Extracts the authorised script index from the redeemer
2. Looks up the reference input at that index
3. Verifies that the reference input contains the correct script
4. Returns the script hash if found, or throws an error otherwise

Parameters:
- `psymbol`: The currency symbol to check against
- `txInfoRefInputs`: List of reference inputs to search
- `redeemer`: The yielding redeemer containing the index information

Returns:
- The authorised script hash if found
- An error if:
  - The reference input is missing
  - The reference input does not contain the expected script
  - The script hash does not match the expected currency symbol
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
              PDNothing -> (ptraceInfoError "getAuthorisedScriptHash: Reference input does not contain reference script")
          )
          (ptraceInfoError "getAuthorisedScriptHash: Reference input does not contain AuthorisedScriptsSTCS")
