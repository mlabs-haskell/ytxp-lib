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
data DirectOfferDatum =
  DirectOfferDatum
    { doCreator :: Address
    , doToBuy :: Value
    }
```

### Reference Script

Undefined.

## States

The Direct Offer does not have meaningful states.

## Participation in Transaction Families

### Introduction

- Offer Creating

### Modification

None.

### Termination

- Offer Executing
- Offer Reclaiming

## Properties

None.
