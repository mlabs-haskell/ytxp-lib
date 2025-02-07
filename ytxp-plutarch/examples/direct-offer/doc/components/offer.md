# Offer component

This component holds the value and the price of the offer.

## UTxO Specification

The UTxO reflecting this component has the following attributes:

### Address

The component sits on the address of the YTxP yielding-validator of the protocol.

### Value

Undefined.

### Datum

The following fields exist in the datum of the Offer UTxO.

```hs
data OfferDatum =
  OfferDatum
    { creator :: Address
    , toBuy :: Value
    }
```

We assume that the creator address is a wallet address.

### Reference Script

Undefined.

## States

The Offer does not have meaningful states.

## Participation in Transaction Families

### Introduction

- [Offer Creating](../transaction-families/creating.md)

### Modification

None.

### Termination

- [Offer Executing](../transaction-families/executing.md)
- [Offer Reclaiming](../transaction-families/reclaiming.md)
