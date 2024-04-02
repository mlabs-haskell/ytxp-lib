{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.YTxP.Control.Yielding (
  YieldListIndex,
  YieldListRefInputIndex,
  YieldingRedeemer (YieldingRedeemer),
  getYieldedToHash,
)
where

import Cardano.YTxP.Control.Vendored (ptryFromOutputDatum)
import Cardano.YTxP.Control.YieldList (PYieldListDatum (PYieldListDatum),
                                       PYieldedToHash)
import Cardano.YTxP.Control.YieldList.MintingPolicy (YieldListSTCS,
                                                     pcontainsYieldListSTT)
import Plutarch.Api.V2 (KeyGuarantees (Unsorted), PDatum, PDatumHash, PMap,
                        PTxInInfo)
import Plutarch.DataRepr (PDataFields)

-- | Represents an index into a YieldList
newtype YieldListIndex = YieldListIndex Integer -- FIXME which Int/Integer/Positive

newtype PYieldListIndex (s :: S) = PYieldListIndex (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PYieldListIndex where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PYieldListIndex)

{- | Represents an index into reference inputs of a transaction.
The UTxO at this index must contain a YieldListSTT; otherwise, we blow up
-}
newtype YieldListRefInputIndex = YieldListRefInputIndex Integer -- FIXME Int/Integer/Positive

newtype PYieldListRefInputIndex (s :: S) = PYieldListRefInputIndex (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PYieldListRefInputIndex where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData (PAsData PYieldListRefInputIndex)

{- | The redeemer passed to the yielding minting policy, validator,
and staking validators
-}
data YieldingRedeemer = YieldingRedeemer
  { yieldListIndex :: YieldListIndex
  -- ^ The index into the YieldList itself. Used to prevent yielding scripts from
  -- needing to inspect each entry for equality; we _only_ look at this index,
  -- check equality, and blow up if it doesn't match.
  , yieldListRefInputindex :: YieldListRefInputIndex
  -- ^ The index into the reference inputs of the transaction where the correct
  -- YieldList UTxO will be found. We use this to prevent yielding scripts from
  -- needing to inspect each reference UTxO for equality; we only look at this
  -- index, and blow up if it doesn't contain the correct STT.
  }

-- FIXME: Int/Integer/Positive

newtype PYieldingRedeemer (s :: S)
  = PYieldingRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "yieldListIndex" ':= PYieldListIndex
               , "yieldListRefInputIndex" ':= PYieldListRefInputIndex
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
- Indexing the reference inputs according to the redeemer
- Checking the fetched reference input for the correct YieldListSTCS
- Decoding its datum (unsafely; the presence of the YieldListSTCS ensure it is authentic and well-formed)
- Looking in the datum at the index in the redeemer and returning the YieldedToHash
-}
getYieldedToHash ::
  YieldListSTCS ->
  Term
    s
    ( PBuiltinList PTxInInfo
        :--> PMap 'Unsorted PDatumHash PDatum
        :--> PYieldingRedeemer
        :--> PYieldedToHash
    )
getYieldedToHash yieldListSTCS = phoistAcyclic $
  plam $
    \txInfoRefInputs datums redeemer -> unTermCont $ do
      yieldingRedeemer <-
        pletFieldsC @'["yieldListIndex", "yieldListRefInputIndex"] redeemer

      let yieldListUTxO =
            txInfoRefInputs
              #!! (pto $ pfromData $ getField @"yieldListRefInputIndex" yieldingRedeemer)
          output = pfield @"resolved" # yieldListUTxO
          value = pfield @"value" # output

      pure $
        pif
          (pcontainsYieldListSTT yieldListSTCS # value)
          ( unTermCont $ do
              let datum = pfromData $ pfield @"datum" # output
              let yieldListDatum = ptryFromOutputDatum # datum # datums

              PYieldListDatum ((pfield @"yieldedToScripts" #) -> yieldList) <-
                pmatchC (pfromData yieldListDatum)

              pure $
                pfromData $
                  yieldList #!! (pto $ pfromData $ getField @"yieldListIndex" yieldingRedeemer)
          )
          (ptraceError "getYieldedToHash: Reference input does not contain YieldListSTCS")
