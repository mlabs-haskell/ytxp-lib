{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- | Module: Cardano.YTxP.Control.YieldList
Description: Defines shared data types and utilities for YieldList scripts
-}
module Cardano.YTxP.Control.YieldList (
  -- * Hashes
  YieldedToHash (YieldedToValidator, YieldedToMP, YieldedToSV),
  CustomScriptHash,
  tryMkCustomScriptHash,
  PYieldedToHash (PYieldedToValidator, PYieldedToMP, PYieldedToSV),

  -- * Redeemers
  YieldListMPWrapperRedeemer,
  PYieldListMPWrapperRedeemer (PMint, PBurn),

  -- * Datums
  YieldListDatum (YieldListDatum),
  PYieldListDatum (PYieldListDatum),

  -- * Functions
  getYieldedToHashByIndex,
) where

import Cardano.YTxP.Control.Vendored (
  DerivePConstantViaDataList (DerivePConstantViaDataList),
  DerivePConstantViaEnum (DerivePConstantEnum),
  EnumIsData (EnumIsData),
  PlutusTypeDataList,
  PlutusTypeEnumData,
  ProductIsData (ProductIsData),
 )
import Control.Monad (guard)
import Generics.SOP qualified as SOP
import Plutarch.Api.V2 (PScriptHash)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PLifted, PUnsafeLiftDecl)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI

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

{- | Note(Nigel): This will likely not compile under `plutus-tx`
due to the use of `error` from the Haskell `Prelude`.
We use `error` from Prelude here as using `traceError` doesn't give back the error message.
See the following issue for more details: https://github.com/IntersectMBO/plutus/issues/3003
-}
instance PlutusTx.ToData CustomScriptHash where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (CustomScriptHash scriptHash) =
    if Builtins.lengthOfByteString scriptHash == 28
      then PlutusTx.toBuiltinData scriptHash
      else error "ScriptHash must be of length 28"

instance PlutusTx.FromData CustomScriptHash where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData b = do
    scriptHash <- PlutusTx.fromBuiltinData b
    guard (Builtins.lengthOfByteString scriptHash == 28)
    pure $ tryMkCustomScriptHash scriptHash

{-# INLINEABLE tryMkCustomScriptHash #-}
tryMkCustomScriptHash :: Builtins.BuiltinByteString -> CustomScriptHash
tryMkCustomScriptHash scriptHash
  | Builtins.lengthOfByteString scriptHash /= 28 =
      error "tryMkCustomScriptHash: ScriptHash must have length 28"
  | otherwise = CustomScriptHash scriptHash

------------------------------------------------------------

-- ** YieldToHash

{- | A single hash that a yielding script can yield to
A yielded to script can be a validator, minting policy or a stake validator
-}
data YieldedToHash
  = YieldedToValidator CustomScriptHash
  | YieldedToMP CustomScriptHash
  | YieldedToSV CustomScriptHash
  deriving stock
    ( Show
    , Generic
    , Eq
    )

PlutusTx.makeIsDataIndexed
  ''YieldedToHash
  [ ('YieldedToValidator, 0)
  , ('YieldedToMP, 1)
  , ('YieldedToSV, 2)
  ]

data PYieldedToHash (s :: S)
  = PYieldedToValidator (Term s (PDataRecord '["scriptHash" ':= PScriptHash]))
  | PYieldedToMP (Term s (PDataRecord '["scriptHash" ':= PScriptHash]))
  | PYieldedToSV (Term s (PDataRecord '["scriptHash" ':= PScriptHash]))
  deriving stock
    ( Generic
    )
  deriving anyclass
    ( PlutusType
    , PIsData
    )

instance DerivePlutusType PYieldedToHash where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PYieldedToHash)

instance PUnsafeLiftDecl PYieldedToHash where
  type PLifted PYieldedToHash = YieldedToHash

deriving via
  (DerivePConstantViaData YieldedToHash PYieldedToHash)
  instance
    (PConstantDecl YieldedToHash)

--------------------------------------------------------------------------------

-- * Redeemers

-- | Redeemer for `mkYieldListMPWrapper`.
data YieldListMPWrapperRedeemer
  = -- | Mint branch
    Mint
  | -- | Burn branch
    Burn
  deriving stock
    ( Show
    , Generic
    , Enum
    , Bounded
    )
  deriving
    ( PlutusTx.ToData
    , PlutusTx.FromData
    )
    via (EnumIsData YieldListMPWrapperRedeemer)

-- | Plutarch-level version of 'YieldListMPWrapperRedeemer'.
data PYieldListMPWrapperRedeemer (s :: S)
  = PMint
  | PBurn
  deriving stock
    ( Generic
    , Enum
    , Bounded
    )
  deriving anyclass
    ( PlutusType
    , PIsData
    , PEq
    )

instance PTryFrom PData (PAsData PYieldListMPWrapperRedeemer)

instance DerivePlutusType PYieldListMPWrapperRedeemer where
  type DPTStrat _ = PlutusTypeEnumData

instance PUnsafeLiftDecl PYieldListMPWrapperRedeemer where
  type PLifted PYieldListMPWrapperRedeemer = YieldListMPWrapperRedeemer

deriving via
  (DerivePConstantViaEnum YieldListMPWrapperRedeemer PYieldListMPWrapperRedeemer)
  instance
    (PConstantDecl YieldListMPWrapperRedeemer)

--------------------------------------------------------------------------------

-- * Datums

{- | The `YieldListDatum` holds a collection of hashes that YieldingScripts can yield to.
The length of the datum is checked upon creation in `mkYieldListSTMPWrapper` to ensure
that the length of the list does not exceed the max list length passed as a parameter to that script.
-}
newtype YieldListDatum = YieldListDatum
  { yieldedToScripts :: [YieldedToHash]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusTx.ToData, PlutusTx.FromData) via (ProductIsData YieldListDatum)

deriving via
  (DerivePConstantViaDataList YieldListDatum PYieldListDatum)
  instance
    (PConstantDecl YieldListDatum)

newtype PYieldListDatum (s :: S)
  = PYieldListDatum
      ( Term
          s
          ( PDataRecord
              '[ "yieldedToScripts" ':= PBuiltinList (PAsData PYieldedToHash)
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PYieldListDatum where
  type DPTStrat _ = PlutusTypeDataList

instance PUnsafeLiftDecl PYieldListDatum where
  type PLifted _ = YieldListDatum

instance PTryFrom PData (PAsData PYieldListDatum)

--------------------------------------------------------------------------------
-- Helpers

getYieldedToHashByIndex ::
  Term s (PYieldListDatum :--> PInteger :--> PYieldedToHash)
getYieldedToHashByIndex = plam $ \datum n ->
  pmatch datum $ \case
    PYieldListDatum ((pfield @"yieldedToScripts" #) -> yieldList) ->
      pfromData $ yieldList #!! n
