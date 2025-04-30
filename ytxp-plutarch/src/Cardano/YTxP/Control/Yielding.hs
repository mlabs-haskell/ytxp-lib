{- | Module: Cardano.YTxP.Control.Yielding
Description: This module provides core types and utilities for handling yielding scripts in the YTxP protocol.

The module exports:
- Types for authorised script indexes and purposes
- Redeemer types for yielding operations
- Utility functions for working with reference inputs

The types in this module are carefully designed to work seamlessly with both Haskell and Plutarch representations,
enabling consistent validation logic both on-chain and off-chain.
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

{- | Newtype representing an index used to find the authorised script

This type serves as a clear semantic wrapper for an index used to locate authorised scripts
in a collection of reference inputs. The specific interpretation of the index depends on:
- Whether the script is a validator
- Whether it's a minting policy
- Whether it's a staking validator

The wrapper provides better type safety and documentation compared to using a raw integer.
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

This type provides a clear enumeration of the possible purposes an authorised script can serve:

- 'PMinting': Scripts involved in minting new assets
- 'PSpending': Scripts that handle spending of assets
- 'PRewarding': Scripts that manage reward distribution

Having explicit purpose types helps prevent invalid script usage and ensures that
the correct validation logic is applied depending on the script's purpose.
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
{-
This pair indicates the authorised script type and the index at which to find proof of the script execution.
This index has a different meaning depending on if the authorised script is a valdiator, minting policy, or staking validator.
-}
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

This type captures all necessary information needed to validate a yielding transaction.

The fields are:

1. 'authorisedScriptIndex': Index pointing to the authorised script in the reference inputs
2. 'authorisedScriptProofIndex': Proof index containing additional verification data

This type provides a uniform interface that works seamlessly both on-chain and off-chain,
ensuring consistent validation logic across different execution contexts.
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
2. Looks up the reference input at that specific index
3. Verifies that the reference input contains the expected script
4. Validates that the script hash matches the expected currency symbol
5. Returns the authorised script hash if all checks pass

Parameters:
- `psymbol`: Currency symbol to verify against
- `txInfoRefInputs`: List of reference inputs to search through
- `redeemer`: The yielding redeemer containing the index information

Returns:
- The authorised script hash if successfully found and validated

Throws an error in the following cases:
- If the reference input at the specified index is missing
- If the reference input does not contain the expected script
- If the script hash does not match the expected currency symbol
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
