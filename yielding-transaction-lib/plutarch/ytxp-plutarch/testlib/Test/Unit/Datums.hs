module Test.Unit.Datums (
  dummyYieldListEmptyListOutputDatum,
  dummyDatumOne,
  dummyDatumTwo,
  dummyOutputDatumOne,
  dummyOutputDatumTwo,
  dummyOutputDatumThree,
) where

import Cardano.YTxP.Control.YieldList (YieldListDatum (YieldListDatum))
import PlutusLedgerApi.V2 (Datum (Datum), OutputDatum (OutputDatum),
                           toBuiltinData)

dummyYieldListEmptyListOutputDatum :: OutputDatum
dummyYieldListEmptyListOutputDatum = OutputDatum dummyYieldListEmptyListDatum

dummyYieldListEmptyListDatum :: Datum
dummyYieldListEmptyListDatum = Datum $ toBuiltinData dummyYieldListEmptyList

dummyYieldListEmptyList :: YieldListDatum
dummyYieldListEmptyList = YieldListDatum []

dummyOutputDatumThree :: OutputDatum
dummyOutputDatumThree = OutputDatum dummyDatumThree

dummyOutputDatumTwo :: OutputDatum
dummyOutputDatumTwo = OutputDatum dummyDatumTwo

dummyOutputDatumOne :: OutputDatum
dummyOutputDatumOne = OutputDatum dummyDatumOne

dummyDatumOne :: Datum
dummyDatumOne = Datum $ toBuiltinData (1 :: Integer)

dummyDatumTwo :: Datum
dummyDatumTwo = Datum $ toBuiltinData (2 :: Integer)

dummyDatumThree :: Datum
dummyDatumThree = Datum $ toBuiltinData (3 :: Integer)
