-- | Stubbed out tests. These can be relocated.
module Stubs where

------------------------------------------------------------
-- Instance Tests

-- | Checks that the Make, TryFromData, FromData, and UnsafeFromData behave as expected. TODO: these should be in SDK

--------------------------------------------------------------------------------
-- YS Tests

-- | [x] Happy path (These need to be tested for all 3 supported purposes)

-- | YS fails when wrong authorisedScriptIndex passed:

-- \| YS fails when no input is present at the index

-- \| YS fails when input is not a reference script

-- \| YS fails when a non-authorised reference script is present at the index (no STCS in value)

-- | YS fails when wrong authorisedScriptProofIndex passed: (These need to be tested for all 3 supported purposes)

-- \| YS fails when index does not point to any "input" (This can happen either with wrong purpose or index)

-- \| YS fails when index points to "input" with the wrong script hash
