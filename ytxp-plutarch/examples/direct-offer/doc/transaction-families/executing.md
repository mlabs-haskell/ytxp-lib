# Offer Executing Transaction Family

This transaction family describes the execution of an offer.

## Script Execution Specification

### Transaction Family Parameters

1. `yieldingMPSymbol :: CurrencySymbol`
  The currency symbol of the offer component token

### Script Purpose

This script must fail on any script purpose except `Rewarding`.

### Redeemer

None.

### Reference Inputs

This transaction family does not check the reference inputs to determine whether the script is successful or not.

However, the follow reference inputs should be present to support the YTxP.

1. Authorized Reference Script UTxO (1)
This input must contain the transaction hash for the `AuthorisedScript` script representing this transaction family.

### Inputs

1. Offer (1)
    - The Offer component value has
      - 1 Offer component token

Other inputs may exist.

### Redeemer Map

Undefined.

### Mints

1. Offer component token minting policy (-1)

Other mints may happen.

### Outputs

The outputs of this transaction family are, in order

1. Wallet (1)
    - A single wallet UTxO is produced at the creator address defined in the datum `creator` of the consumed offer component.
    - The wallet UTxO value must equal to the value defined in the datum `toBuy` of the consumed offer component.

Other outputs are permitted.

### Staking Events

The withdrawal of this staking validator.

### Validity Range

Undefined.

### Signatories

Undefined.
