# Script Deploying Transaction Family

This transaction family describes the deployment of a script.

## Script Execution Specification

### Transaction Family Parameters

1. `yieldingValidatorAddress :: ScriptAddress`
  The address locking yielding UTxOs

2. `yieldingMPSymbol :: CurrencySymbol`
  The currency symbol of the script component token

3. `signatory :: PubKeyHash`
  The signatory required to authorize the deployment

### Script Purpose

This script must fail on any script purpose except `Rewarding`.

### Redeemer

None.

### Reference Inputs

This transaction family does not check the reference inputs to determine whether the script is successful or not.

However, the following reference inputs should be present to support the YTxP.

1. Authorized Reference Script UTxO (1)
This input must contain the transaction hash for the `AuthorisedScript` script representing this transaction family.

### Inputs

Undefined.

### Redeemer Map

Undefined.

### Mints

1. Script component token minting policy (1)

Other mints may happen.

### Outputs

The outputs of this transaction family are

1. Script (1)
    - The Script component is created at the yielding validator script address with no staking address
    - The Script component value has
      - 1 Script component token
    - The datum must:
      - Must have a unit datum

Other outputs are permitted.

### Staking Events

The withdrawal from this staking validator.

### Validity Range

Undefined.

### Signatories

The transaction must be signed by the signatory specified in the transaction family parameter.
