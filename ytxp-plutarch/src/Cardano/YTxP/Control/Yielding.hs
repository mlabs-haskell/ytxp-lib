{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.YTxP.Control.Yielding (
  YieldingRedeemer (YieldingRedeemer),
  getAuthorisedScriptHash,
  PAuthorisedScriptPurpose (PMinting, PSpending, PRewarding),
  AuthorisedScriptIndex (AuthorisedScriptIndex),
  AuthorisedScriptPurpose (Minting, Spending, Rewarding),
  AuthorisedScriptProofIndex (AuthorisedScriptProofIndex),
  PYieldingRedeemer,
)
where

import Cardano.YTxP.SDK.SdkParameters (
  -- TODO rename
  YieldListSTCS (YieldListSTCS),
 )
import PlutusTx qualified

-- TODO/QUESTION: copied from YieldList, is this import safe?

import Cardano.YTxP.Control.Vendored (DerivePConstantViaEnum (DerivePConstantEnum), EnumIsData (EnumIsData), PlutusTypeEnumData)
import Plutarch.Api.V1.Maybe (PMaybeData (PDJust, PDNothing))
import Plutarch.Api.V2 (PScriptHash, PTxInInfo, PValue)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Utils (pmember)

-- | Represents an index into a authorised reference script in a TxInReferenceInput list
newtype AuthorisedScriptIndex = AuthorisedScriptIndex Integer
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

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

{- The type of yielded to scripts
-}
data AuthorisedScriptPurpose = Minting | Spending | Rewarding
  deriving stock (Show, Generic, Enum, Bounded)
  deriving
    (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    via (EnumIsData AuthorisedScriptPurpose)

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

{- Index for the yielding redeemer
-}
newtype AuthorisedScriptProofIndex = AuthorisedScriptProofIndex (AuthorisedScriptPurpose, Integer)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

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

-- TODO: why is this not deriving newtype with AsData?
deriving via
  (DerivePConstantViaNewtype AuthorisedScriptProofIndex PAuthorisedScriptProofIndex (PBuiltinPair PAuthorisedScriptPurpose PInteger))
  instance
    (PConstantDecl AuthorisedScriptProofIndex)

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

instance PUnsafeLiftDecl PYieldingRedeemer where
  type PLifted PYieldingRedeemer = YieldingRedeemer

deriving via
  (DerivePConstantViaData YieldingRedeemer PYieldingRedeemer)
  instance
    (PConstantDecl YieldingRedeemer)

{- | Given a list of reference inputs and a Yielding Redeemer, dig out the authorised script hash
 by:

- Indexing the reference inputs according to the redeemer
- Checking the fetched reference input for the correct YieldListSTCS
- Returning the AuthorisedScriptHash
-}
getAuthorisedScriptHash ::
  YieldListSTCS ->
  Term
    s
    ( PBuiltinList PTxInInfo
        :--> PYieldingRedeemer
        :--> PScriptHash
    )
getAuthorisedScriptHash yieldListSTCS = phoistAcyclic $
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
          (pcontainsAuthorisedScriptSTT yieldListSTCS # value)
          ( pmatch (pfield @"referenceScript" # output) $ \case
              PDJust ((pfield @"_0" #) -> autorisedScript) -> autorisedScript
              PDNothing _ -> (ptraceError "getAuthorisedScriptHash: Reference input does not contain reference script")
          )
          (ptraceError "getAuthorisedScriptHash: Reference input does not contain YieldListSTCS")

{- | Checks that the given 'PValue' contains the YieldListSTT
TODO (OPTIMIZE): make partial (`has`/`lacks`) variants and use those instead
-}
pcontainsAuthorisedScriptSTT ::
  YieldListSTCS -> Term s (PValue anyKey anyAmount :--> PBool)
pcontainsAuthorisedScriptSTT (YieldListSTCS symbol) =
  plam $ \value ->
    pmember # pconstant symbol # pto value
