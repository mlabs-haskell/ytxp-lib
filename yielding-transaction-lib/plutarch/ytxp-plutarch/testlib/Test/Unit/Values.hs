module Test.Unit.Values (
    dummySymbolOne,
    dummySymbolTwo,
    dummySymbolThree,
    dummyValueOne,
    dummyValueTwo,
    dummyValueThree,
) where

import PlutusLedgerApi.V1.Value (
    CurrencySymbol (CurrencySymbol),
    Value,
    singleton,
 )
import PlutusLedgerApi.V2 (getLedgerBytes)

-- | Sample symbol for tests
dummySymbolOne :: CurrencySymbol
dummySymbolOne =
    CurrencySymbol $
        getLedgerBytes "00000000000000000000000000000000000000000000000000000000"

-- | Sample symbol for tests
dummySymbolTwo :: CurrencySymbol
dummySymbolTwo =
    CurrencySymbol $
        getLedgerBytes "11111111111111111111111111111111111111111111111111111111"

-- | Sample symbol for tests
dummySymbolThree :: CurrencySymbol
dummySymbolThree =
    CurrencySymbol $
        getLedgerBytes "22222222222222222222222222222222222222222222222222222222"

dummyValueOne :: Value
dummyValueOne = mkSomeValue dummySymbolOne 1

dummyValueTwo :: Value
dummyValueTwo = mkSomeValue dummySymbolTwo 1

-- Same symbol as 'dummyValueOne' but with different amount
dummyValueThree :: Value
dummyValueThree = mkSomeValue dummySymbolOne 4

-- | Make a sample `Value` for tests
mkSomeValue :: CurrencySymbol -> Integer -> Value
mkSomeValue symbol = singleton symbol "Some token"
