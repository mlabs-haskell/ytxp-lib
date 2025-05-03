{- HLINT ignore -}

module Cardano.YTxP.Example.Offer (
  POfferDatum (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap (KeyGuarantees (Sorted))
import Plutarch.LedgerApi.V3 (PPubKeyHash)
import Plutarch.LedgerApi.Value (AmountGuarantees (Positive), PValue)

-- | The datum of the offer component
data POfferDatum (s :: S) = POfferDatum
  { creator :: Term s (PAsData PPubKeyHash)
  , toBuy :: Term s (PAsData (PValue 'Sorted 'Positive))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via (DeriveAsDataStruct POfferDatum)

instance PTryFrom PData (PAsData POfferDatum)
