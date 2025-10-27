module Cardano.YTxP.SDK.Redeemers (
  AuthorisedScriptIndex (AuthorisedScriptIndex),
  AuthorisedScriptPurpose (Minting, Spending, Rewarding),
  AuthorisedScriptProofIndex (AuthorisedScriptProofIndex),
  YieldingRedeemer (
    YieldingRedeemer,
    authorisedScriptIndex,
    authorisedScriptProofIndex
  ),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (Arbitrary (arbitrary), arbitraryBoundedEnum)

-- | Represents an index into a authorised reference script in a TxInReferenceInput list
newtype AuthorisedScriptIndex = AuthorisedScriptIndex Integer
  deriving newtype
    ( Show
    , Eq
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    , PlutusTx.Eq
    , Arbitrary
    )

{- The type of yielded to scripts
-}
data AuthorisedScriptPurpose = Minting | Spending | Rewarding
  deriving stock (Show, Generic, Eq, Enum, Bounded)
  deriving anyclass (SOP.Generic)

instance Arbitrary AuthorisedScriptPurpose where
  arbitrary = arbitraryBoundedEnum

-- We write these instances like this to match the plutarch type implementation
-- that uses the fact that a type is an enum to encode it directly with integers onchain
instance PlutusTx.ToData AuthorisedScriptPurpose where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = PlutusTx.toBuiltinData . toInteger . fromEnum

instance PlutusTx.FromData AuthorisedScriptPurpose where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d = case PlutusTx.fromBuiltinData @Integer d of
    PlutusTx.Nothing -> PlutusTx.Nothing
    PlutusTx.Just i ->
      if i PlutusTx.== 0
        then PlutusTx.Just Minting
        else
          if i PlutusTx.== 1
            then PlutusTx.Just Spending
            else
              if i PlutusTx.== 2
                then PlutusTx.Just Rewarding
                else PlutusTx.Nothing

instance PlutusTx.UnsafeFromData AuthorisedScriptPurpose where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d = case PlutusTx.fromBuiltinData d of
    Nothing -> PlutusTx.error ()
    Just i -> i

instance PlutusTx.Eq AuthorisedScriptPurpose where
  {-# INLINEABLE (==) #-}
  Minting == Minting = True
  Spending == Spending = True
  Rewarding == Rewarding = True
  _ == _ = False

{- Index for the yielding redeemer
-}
newtype AuthorisedScriptProofIndex
  = AuthorisedScriptProofIndex (AuthorisedScriptPurpose, Integer)
  deriving newtype
    ( Show
    , Eq
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    , PlutusTx.Eq
    , Arbitrary
    )

{- | The redeemer passed to the yielding minting policy, validator,
and staking validators
-}
data YieldingRedeemer = YieldingRedeemer
  { authorisedScriptIndex :: AuthorisedScriptIndex
  -- ^ Integer The index of the TxInReferenceInput that contains the authorised reference script.
  , authorisedScriptProofIndex :: AuthorisedScriptProofIndex
  -- ^ A tuple containing yielded to script type and the index at which to find proof: this allows us to avoid having to loop through inputs/mints/withdrawls to find the script we want to ensure is run.
  }
  deriving stock (Show, Generic, Eq)

instance Arbitrary YieldingRedeemer where
  arbitrary = YieldingRedeemer <$> arbitrary <*> arbitrary

instance PlutusTx.Eq YieldingRedeemer where
  {-# INLINEABLE (==) #-}
  YieldingRedeemer si spi == YieldingRedeemer si' spi' =
    si PlutusTx.== si' PlutusTx.&& spi PlutusTx.== spi'

PlutusTx.makeIsDataIndexed ''YieldingRedeemer [('YieldingRedeemer, 0)]
