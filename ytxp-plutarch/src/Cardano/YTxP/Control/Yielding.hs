{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.YTxP.Control.Yielding (
  YieldingRedeemer (YieldingRedeemer),
  getAuthorisedScriptHash,
)
where

import Cardano.YTxP.SDK.SdkParameters (
  -- TODO rename
  YieldListSTCS (YieldListSTCS),
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins

-- TODO/QUESTION: copied from YieldList, is this import safe?
import PlutusTx.Builtins.Internal qualified as BI

import Cardano.YTxP.Control.Vendored (PlutusTypeEnumData)
import Plutarch.Api.V2 (PScriptHash, PTxInInfo, PValue)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Utils (pmember, punsafeFromInlineDatum)

-- | Represents an index into a authorised reference script in a TxInReferenceInput list
newtype AuthorisedScriptIndex = AuthorisedScriptIndex Integer

newtype PAuthorisedScriptIndex (s :: S) = PAuthorisedScriptIndex (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PAuthorisedScriptIndex where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PAuthorisedScriptIndex)

{- The type of yielded to scripts
-}
data AuthorisedScriptPurpose = Minting | Spending | Delegating | Rewarding

data PAuthorisedScriptPurpose (s :: S) = PMinting | PSpending | PDelegating | PRewarding

instance DerivePlutusType PAuthorisedScriptPurpose where
  type DPTStrat _ = PlutusTypeEnumData

instance PTryFrom PData (PAsData PAuthorisedScriptPurpose)

newtype AuthorisedScriptProofIndex = AuthorisedScriptProofIndex (AuthorisedScriptPurpose, Integer)

newtype PAuthorisedScriptProofIndex (s :: S) = PAuthorisedScriptProofIndex (Term s (PBuiltinPair PAuthorisedScriptPurpose PInteger))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

{- | The redeemer passed to the yielding minting policy, validator,
and staking validators
-}
data YieldingRedeemer = YieldingRedeemer
  { yrAuthorisedScriptIndex :: AuthorisedScriptIndex
  -- ^ Integer The index of the TxInReferenceInput that contains the authorised reference script.
  , yrAuthorisedScriptProofIndex :: AuthorisedScriptProofIndex
  -- ^ A tuple containing yielded to script type and the index at which to find proof: this allows us to avoid having to loop through inputs/mints/withdrawls to find the script we want to ensure is run.
  }

newtype PYieldingRedeemer (s :: S)
  = PYieldingRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "yrAuthorisedScriptIndex" ':= PAuthorisedScriptIndex
               , "yrAuthorisedScriptProofIndex" ':= PAuthorisedScriptProofIndex
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PYieldingRedeemer where
  -- TODO/QUESTION: why arent we using PlutusTypeNewtype here?
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PYieldingRedeemer)

--------------------------------------------------------------------------------

-- * Hashes

------------------------------------------------------------

-- ** CustomScriptHash

-- We use this because the plutus-ledger-api ScriptHash isn't
-- type safe.

{- | We use this `CustomScriptHash` instead of `ScriptHash` in
order to ensure that the hash is of length 28.
-}
newtype CustomScriptHash = CustomScriptHash {getCustomScriptHash :: Builtins.BuiltinByteString}
  deriving stock
    ( Show
    , Eq
    )

instance PlutusTx.UnsafeFromData CustomScriptHash where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData b =
    let !args = BI.snd $ BI.unsafeDataAsConstr b
        scriptHash = BI.unsafeDataAsB (BI.head args)
     in CustomScriptHash scriptHash

instance PlutusTx.ToData CustomScriptHash where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CustomScriptHash scriptHash) = PlutusTx.toBuiltinData scriptHash

instance PlutusTx.FromData CustomScriptHash where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData b = do
    scriptHash <- PlutusTx.fromBuiltinData b
    guard (Builtins.lengthOfByteString scriptHash == 28)
    pure $ tryMkCustomScriptHash scriptHash

{- | Note(Nigel): This will likely not compile under `plutus-tx`
due to the use of `error` from the Haskell `Prelude`.
We use `error` from Prelude here as using `traceError` doesn't give back the error message.
See the following issue for more details: https://github.com/IntersectMBO/plutus/issues/3003
-}
{-# INLINEABLE tryMkCustomScriptHash #-}
tryMkCustomScriptHash :: Builtins.BuiltinByteString -> CustomScriptHash
tryMkCustomScriptHash scriptHash
  | Builtins.lengthOfByteString scriptHash /= 28 =
      error "tryMkCustomScriptHash: ScriptHash must have length 28"
  | otherwise = CustomScriptHash scriptHash

------------------------------------------------------------

-- ** AuthorisedScriptHash

-- | A single hash that a yielding script can yield to
newtype AuthorisedScriptHash = AuthorisedScriptHash CustomScriptHash
  deriving stock
    ( Show
    , Generic
    , Eq
    )

newtype PAuthorisedScriptHash (s :: S)
  = PAuthorisedScriptHash (Term s PScriptHash)
  deriving stock
    ( Generic
    )
  deriving anyclass
    ( PlutusType
    , PIsData
    )

instance DerivePlutusType PAuthorisedScriptHash where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PAuthorisedScriptHash)

instance PUnsafeLiftDecl PAuthorisedScriptHash where
  type PLifted PAuthorisedScriptHash = AuthorisedScriptHash

{- | Given a list of reference inputs and a Yielding Redeemer, dig out the
YieldList by:
- Indexing the reference inputs according to the redeemer
- Checking the fetched reference input for the correct YieldListSTCS
- Decoding its datum (unsafely; the presence of the YieldListSTCS ensure it is authentic and well-formed)
- Looking in the datum at the index in the redeemer and returning the AuthorisedScriptHash
-}
getAuthorisedScriptHash ::
  YieldListSTCS ->
  Term
    s
    ( PBuiltinList PTxInInfo
        :--> PYieldingRedeemer
        :--> PAuthorisedScriptHash
    )
getAuthorisedScriptHash yieldListSTCS = phoistAcyclic $
  plam $
    \txInfoRefInputs redeemer -> unTermCont $ do
      -- TODO (OPTIMIZE): these values only get used once, can be a `let`
      yieldingRedeemer <-
        pletFieldsC @'["yieldListIndex", "yieldListRefInputIndex"] redeemer

      let yieldListUTxO =
            txInfoRefInputs
              #!! pto (pfromData $ getField @"yieldListRefInputIndex" yieldingRedeemer)
          output = pfield @"resolved" # yieldListUTxO
          value = pfield @"value" # output

      pure $
        pif
          (pcontainsYieldListSTT yieldListSTCS # value)
          ( let datum = pfromData $ pfield @"datum" # output
                ylDatum = punsafeFromInlineDatum # datum
                ylIndex = pfromData $ getField @"yieldListIndex" yieldingRedeemer
             in getAuthorisedScriptHashByIndex # ylDatum # pto ylIndex
          )
          (ptraceError "getAuthorisedScriptHash: Reference input does not contain YieldListSTCS")

{- | Checks that the given 'PValue' contains the YieldListSTT
TODO (OPTIMIZE): make partial (`has`/`lacks`) variants and use those instead
-}
pcontainsYieldListSTT ::
  YieldListSTCS -> Term s (PValue anyKey anyAmount :--> PBool)
pcontainsYieldListSTT (YieldListSTCS symbol) =
  plam $ \value ->
    pmember # pconstant symbol # pto value
