module Test.Unit.Datums (
  dummyDatumOne,
  dummyOutputDatumOne,
) where

import PlutusLedgerApi.V2 (
  Datum (Datum),
  OutputDatum (OutputDatum),
  toBuiltinData,
 )

dummyOutputDatumOne :: OutputDatum
dummyOutputDatumOne = OutputDatum dummyDatumOne

dummyDatumOne :: Datum
dummyDatumOne = Datum $ toBuiltinData (1 :: Integer)
