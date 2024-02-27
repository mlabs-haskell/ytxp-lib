{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.YTxP.Control.Yielding (
  YieldListIndex,
  YieldListRefInputIndex,
  YieldingRedeemer,
  getYieldList,
)
where

import Prelude (Integer, error)

-- | Represents an index into a YieldList
newtype YieldListIndex = YieldListIndex Integer -- FIXME which Int/Integer/Positive

{- | Represents an index into reference inputs of a transaction.
The UTxO at this index must contain a YieldListSTT; otherwise, we blow up
-}
newtype YieldListRefInputIndex = YieldListRefInputIndex Integer -- FIXME Int/Integer/Positive

{- | The redeemer passed to the yielding minting policy, validator,
and staking validators
-}
data YieldingRedeemer = YieldingRedeemer
  { yieldListIndex :: YieldListIndex
  -- ^ The index into the YieldList itself. Used to prevent yielding scripts from
  -- needing to inspect each entry for equality; we _only_ look at this index,
  -- check equality, and blow up if it doesn't match.
  , yieldListRefInputindex :: ()
  -- ^ The index into the reference inputs of the transaction where the correct
  -- YieldList UTxO will be found. We use this to prevent yielding scripts from
  -- needing to inspect each reference UTxO for equality; we only look at this
  -- index, and blow up if it doesn't contain the correct STT.
  }

-- FIXME: Int/Integer/Positive

{- | Given a list of reference inputs and a Yielding Redeemer, dig out the
YieldList by:
- Indexing the reference inputs according to the redeemer
- Checking the fetched reference input for the correct YieldListSTCS
- Decoding its datum (unsafely; the presence of the YieldListSTCS ensure it is authentic and well-formed)
- Looking in the datum at the index in the redeemer and returning the YieldedToHash
-}
getYieldList :: () -- FIXME: something like
-- ControlParameters -> [ReferenceInput] -> YieldingRedeemer -> YieldedToHash
getYieldList = error "unimplemented"
