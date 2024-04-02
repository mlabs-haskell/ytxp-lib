module Test.Unit.Values (
  dummySymbolOne,
  dummySymbolTwo,
  dummyValueOne,
) where

import PlutusLedgerApi.V1.Value (
  CurrencySymbol (CurrencySymbol),
  Value,
  singleton,
 )
import PlutusLedgerApi.V2 (
  getLedgerBytes,
 )

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

dummyValueOne :: Value
dummyValueOne = mkSomeValue 1

-- | Make a sample `Value` for tests
mkSomeValue :: Integer -> Value
mkSomeValue =
  singleton
    ( CurrencySymbol $
        getLedgerBytes "00000000000000000000000000000000000000000000000000000000"
    )
    "Some token"
