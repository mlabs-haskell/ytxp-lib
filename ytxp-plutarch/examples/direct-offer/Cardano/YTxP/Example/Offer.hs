{- HLINT ignore -}

module Cardano.YTxP.Example.Offer (
  POfferDatum (..),
) where

import Plutarch.DataRepr (PDataFields)
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (Sorted))
import Plutarch.LedgerApi.V3 (PPubKeyHash)
import Plutarch.LedgerApi.Value (AmountGuarantees (Positive), PValue)

-- | The datum of the offer component
data POfferDatum (s :: S)
  = POfferDatum
      ( Term
          s
          ( PDataRecord
              '[ "creator" ':= PPubKeyHash
               , "toBuy" ':= PValue 'Sorted 'Positive
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POfferDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POfferDatum
