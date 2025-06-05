# Script Removing Transaction Family

This transaction family describes the removal of a script.

## Script Execution Specification

### Transaction Family Parameters

1. `yieldingMPSymbol :: CurrencySymbol`
  The currency symbol of the script component token
2. `signatory :: PubKeyHash`
  The signatory required to authorize the deployment

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

1. Script (1)
    - The Script component value has
      - 1 Script component token

Other inputs may exist.

### Redeemer Map

Undefined.

### Mints

1. Script component token minting policy (-1)

Other mints may happen.

### Outputs

Undefined.

### Staking Events

The withdrawal of this staking validator.

### Validity Range

Undefined.

### Signatories

The transaction must be signed by the signatory specified in the transaction family parameter.
