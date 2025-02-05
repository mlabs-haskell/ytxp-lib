# Offer Reclaiming Transaction Family

This transaction family describes the offer revocation.

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

Undefined.

### Staking Events

The withdrawal of this staking validator.

### Validity Range

Undefined.

### Signatories

The transaction is signed by the creator defined in the datum `creator` of the consumed offer component.
