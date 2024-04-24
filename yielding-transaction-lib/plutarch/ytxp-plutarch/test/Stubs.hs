-- | Stubbed out tests. These can be relocated.

module Stubs where

------------------------------------------------------------
-- Instance Tests

-- | Checks that the Make, TryFromData, FromData, and UnsafeFromData behave as expected
-- (including large datum attacks on lists, extra constr fields, ill-formed script hashes, etc)

--------------------------------------------------------------------------------
-- YLSTMP tests

-- | Nominal Case

-- | YLSTMP fails when more than one YieldListSTT minted

-- | YLSTMP fails when the YieldListSTT token name is not empty

-- | YLSTMP fails when the receiving UTxO is not at a script
-- address

-- | YLSTMP fails when the receiving UTxO has tokens besides ada and the YLSTMP

-- | YLSTMP fails when the receiving UTxO does not contain a valid yield list.
-- Checks:
--   - Well-formed hashes
--   - 0 < length yieldList <= maxYieldListSize

-- | YLSTMP fails to burn when `n` UTxOs with YieldListSTTs appear at the input, where n /= 1

-- | YLSTMP fails to burn when a non-zero number of UTxOs with YieldListSTTs appear at the output

-- | YLSTMP fails when there are extra UTxOs at the input

-- | YLSTMP fails when there are extra UTxOs at the output

-- | YLSTMP fails when the input wallet UTxO has a YieldListSTT

-- | YLSTMP fails when the output wallet UTxO has a YieldListSTT

--------------------------------------------------------------------------------
-- YLV Tests

-- | YLV fails to unlock when YLSTT is not burned

-- | YLV fails to unlock when additional UTxOs are present at input

-- | YLV fails to unlock when additional UTxOs are present at output


--------------------------------------------------------------------------------
-- YMP Tests

-- | Nominal case

-- | YMP fails when wrong YLRefInputIndex passed

-- | YMP fails when wrong YLIndex passed

-- | YMP fails if YieldedToHash not found; minting policy case

-- | YMP fails if YieldedToHash not found; staking validator case

-- | YMP fails if YieldedToHash not found; validator case


--------------------------------------------------------------------------------
-- YV Tests

-- | Nominal case

-- | YV fails when wrong YLRefInputIndex passed

-- | YV fails when wrong YLIndex passed

-- | YV fails if YieldedToHash not found; minting policy case

-- | YV fails if YieldedToHash not found; staking validator case

-- | YV fails if YieldedToHash not found; validator case


--------------------------------------------------------------------------------
-- YSV Tests

-- | Nominal case

-- | YSV fails when wrong YLRefInputIndex passed

-- | YSV fails when wrong YLIndex passed

-- | YSV fails if YieldedToHash not found; minting policy case

-- | YSV fails if YieldedToHash not found; staking validator case

-- | YSV fails if YieldedToHash not found; validator case
