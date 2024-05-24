{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.YTxP.Control.Yielding (
  YieldingRedeemer (YieldingRedeemer),
  -- getAuthorisedScriptHash,
)
where

import Cardano.YTxP.SDK.SdkParameters (
  -- TODO rename
  YieldListSTCS (YieldListSTCS),
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins

-- TODO/QUESTION: copied from YieldList, is this import safe?

import Cardano.YTxP.Control.Vendored (EnumIsData (EnumIsData), PlutusTypeEnumData)
import Plutarch.Api.V2 (PScriptHash, PTxInInfo, PValue)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import PlutusTx.Builtins.Internal qualified as BI
import Utils (pmember, punsafeFromInlineDatum)

-- | Represents an index into a authorised reference script in a TxInReferenceInput list
newtype AuthorisedScriptIndex = AuthorisedScriptIndex Integer
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

newtype PAuthorisedScriptIndex (s :: S) = PAuthorisedScriptIndex (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PAuthorisedScriptIndex where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PAuthorisedScriptIndex)

{- The type of yielded to scripts
-}
data AuthorisedScriptPurpose = Minting | Spending | Delegating | Rewarding
  deriving stock (Enum)
  deriving
    (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    via (EnumIsData AuthorisedScriptPurpose)

data PAuthorisedScriptPurpose (s :: S) = PMinting | PSpending | PDelegating | PRewarding
  deriving stock (Generic, Enum, Bounded)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PAuthorisedScriptPurpose where
  type DPTStrat _ = PlutusTypeEnumData

instance PTryFrom PData (PAsData PAuthorisedScriptPurpose)

{- Index for the yielding redeemer
-}
data AuthorisedScriptProofIndex = AuthorisedScriptProofIndex
  { authorisedScriptPurpose :: AuthorisedScriptPurpose
  , proofIndex :: Integer
  }

PlutusTx.makeIsDataIndexed ''AuthorisedScriptProofIndex [('AuthorisedScriptProofIndex, 0)]

newtype PAuthorisedScriptProofIndex (s :: S)
  = PAuthorisedScriptProofIndex
      ( Term
          s
          (PBuiltinPair (PAsData PAuthorisedScriptPurpose) (PAsData PInteger))
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PAuthorisedScriptProofIndex where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PAuthorisedScriptProofIndex)

{- | The redeemer passed to the yielding minting policy, validator,
and staking validators
-}
data YieldingRedeemer = YieldingRedeemer
  { authorisedScriptIndex :: AuthorisedScriptIndex
  -- ^ Integer The index of the TxInReferenceInput that contains the authorised reference script.
  , authorisedScriptProofIndex :: AuthorisedScriptProofIndex
  -- ^ A tuple containing yielded to script type and the index at which to find proof: this allows us to avoid having to loop through inputs/mints/withdrawls to find the script we want to ensure is run.
  }

PlutusTx.makeIsDataIndexed ''YieldingRedeemer [('YieldingRedeemer, 0)]

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

{- | Given a list of reference inputs and a Yielding Redeemer, dig out the
YieldList by:

TODO: change into `checkAuthorisedScriptHash` the function needs to index the ref tx ins and
check that the utxo at the supplied `AuthorisedScriptIndex` contains the AuthorisedScriptSTCS

- Indexing the reference inputs according to the redeemer
- Checking the fetched reference input for the correct YieldListSTCS
- Decoding its datum (unsafely; the presence of the YieldListSTCS ensure it is authentic and well-formed)
- Looking in the datum at the index in the redeemer and returning the AuthorisedScriptHash
-}

-- getAuthorisedScriptHash ::
--   YieldListSTCS ->
--   Term
--     s
--     ( PBuiltinList PTxInInfo
--         :--> PYieldingRedeemer
--         :--> PBool
--     )
-- getAuthorisedScriptHash yieldListSTCS = phoistAcyclic
--   plam
--   $ \txInfoRefInputs redeemer -> unTermCont $ do
--     -- TODO (OPTIMIZE): these values only get used once, can be a `let`
--     yieldingRedeemer <-
--       pletFieldsC @'["yieldListIndex", "yieldListRefInputIndex"] redeemer

--     let yieldListUTxO =
--           txInfoRefInputs
--             #!! pto (pfromData $ getField @"yieldListRefInputIndex" yieldingRedeemer)
--         output = pfield @"resolved" # yieldListUTxO
--         value = pfield @"value" # output

--     pure $
--       pif
--         (pcontainsYieldListSTT yieldListSTCS # value)
--         ( let datum = pfromData $ pfield @"datum" # output
--               ylDatum = punsafeFromInlineDatum # datum
--               ylIndex = pfromData $ getField @"yieldListIndex" yieldingRedeemer
--            in pconstant False -- getAuthorisedScriptHashByIndex # ylDatum # pto ylIndex
--         )
--         (ptraceError "getAuthorisedScriptHash: Reference input does not contain YieldListSTCS")

{- | Checks that the given 'PValue' contains the YieldListSTT
TODO (OPTIMIZE): make partial (`has`/`lacks`) variants and use those instead
-}
pcontainsYieldListSTT ::
  YieldListSTCS -> Term s (PValue anyKey anyAmount :--> PBool)
pcontainsYieldListSTT (YieldListSTCS symbol) =
  plam $ \value ->
    pmember # pconstant symbol # pto value
