module Test.Unit.Datums (
  dummyDatumOne,
  dummyDatumTwo,
  dummyOutputDatumOne,
  dummyOutputDatumTwo,
) where

import PlutusLedgerApi.V2 (
  Datum (Datum),
  OutputDatum (OutputDatum),
  toBuiltinData,
 )

dummyOutputDatumTwo :: OutputDatum
dummyOutputDatumTwo = OutputDatum dummyDatumTwo

dummyOutputDatumOne :: OutputDatum
dummyOutputDatumOne = OutputDatum dummyDatumOne

dummyDatumOne :: Datum
dummyDatumOne = Datum $ toBuiltinData (1 :: Integer)

dummyDatumTwo :: Datum
dummyDatumTwo = Datum $ toBuiltinData (2 :: Integer)
