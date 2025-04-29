{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Module: Cardano.YTxP.Control.Yielding
Description: This module provides types and utilities for handling yielding scripts in the YTxP protocol.

The module exports types related to authorised scripts and their purposes, as well as
redeemer types for yielding operations. The primary function 'getAuthorisedScriptHash'
provides a way to retrieve the authorised script hash from reference inputs.

The types in this module are designed to work with both Haskell and Plutarch representations,
facilitating code reuse across different implementations.

The module also contains orphan instances necessary for converting between Haskell types
and Plutarch types. These instances are carefully implemented to maintain type safety
and proper serialization between the two languages.
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

The type is designed to be used with 'Plutarch' and provides proper serialization
and deserialization between Haskell and Plutarch representations.
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

{- | Redeemer type for yielding operations.

This type represents the data needed to validate a yielding transaction.
It contains:

1. 'authorisedScriptIndex': The index of the authorised script
2. 'authorisedScriptProofIndex': The proof index for the authorised script

The type is designed to work with both on-chain and off-chain code,
providing a consistent interface for handling yielding operations.
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

{- | Given a list of reference inputs and a Yielding Redeemer, retrieves the authorised script hash.

This function performs the following steps:

1. Extracts the authorised script index from the redeemer
2. Looks up the reference input at that index
3. Verifies that the reference input contains the correct script
4. Returns the script hash if found, or throws an error otherwise

The function is designed to work with both Haskell and Plutarch types seamlessly,
providing a safe and efficient way to handle script hash retrieval.

Parameters:
- `psymbol`: The currency symbol
- `txInfoRefInputs`: List of reference inputs to search
- `redeemer`: The yielding redeemer containing the index information

Returns:
- The authorised script hash if found
- An error if the reference input is missing or invalid
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
