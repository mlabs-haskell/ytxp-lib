# Offer Creating Transaction Family

This transaction family describes the creation of an offer.

## Script Execution Specification

The transactions detailed in this specification are non-composable. This design is due to assumptions regarding the order of outputs. It is defined this way for the sake of performance simplicity.

Additionally, this contract assumes that the creator address of the offer component datum is a wallet address.

### Transaction Family Parameters

1. `yieldingValidatorAddress :: ScriptAddress`
  The address locking yielding UTxOs

2. `yieldingMPSymbol :: CurrencySymbol`
  The currency symbol of the offer component token

### Script Purpose

This script must fail on any script purpose except `Rewarding`.

### Redeemer

None.

### Reference Inputs

This transaction family does not check the reference inputs to determine whether the script is successful or not.

However, the following reference inputs should be present to support the YTxP, as well as for off-chain efficiency.

1. Authorized Reference Script UTxO (1)
This input must contain the transaction hash for the `AuthorisedScript` script representing this transaction family.

2. Offer component token minting policy reference input (1)

### Inputs

Undefined.

### Redeemer Map

Undefined.

### Mints

1. Offer component token minting policy (1)

Other mints may happen.

### Outputs

The outputs of this transaction family are, in order

1. Offer (1)
    - The Offer component is created at the yielding validator script address with no staking address
    - The Offer component value has
      - 1 Offer component token
    - The datum must:
      - Must have creator address
      - Must have a to buy value
    - The Offer component must not carry a reference script

Other outputs are permitted.

### Staking Events

The withdrawal of this staking validator.

### Validity Range

Undefined.

### Signatories

Undefined.
