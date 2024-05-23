# Transaction Family Title

This document specifies the format in which YTxP-style transaction families should be described.
Designers of transaction families should copy this document when specifying new families.
Quoted text indicates what should go in each section, such as:

> Include a brief summary of the transaction family.
> Keep it high-level so that readers can understand what the transaction _does_.
> Indicate the main transaction flows that this transaction family is part of.

Transaction families described using this template should accomplish three goals:

- A description sufficient for non-technical, semi-technical, and technical reviewers to understand what the transaction family does and why it exists.
  Specifically, business stakeholders should understand what the transaction represents; management and product-owners should get a understanding of how "large" of a transaction family this is and be able to make effort estimates for implementation; and auditors, developers, or other specifiers should be able to get a complete picture of the family.
- A description sufficient to implement a `AuthorisedScript` script that encodes the business logic described in the family, as well as to test the behavior of the family in a black-box manner.
- A description sufficient to implement an offchain function that builds and submits a transaction that is part of this family, as well as to test such transaction building and submission in a black-box manner.

Not all sections will be equally relevant for all stakeholders.

In all cases, indicate explicitly whether or not additional items can be added by the transaction submitter or implementor -- i.e., whether or not additional inputs are allowed, or only those specified.

This template can be used to describe transaction families that do not strictly follow the YTxP architecture, but the information required to describe them may differ from the guidelines below.

## Conceptual Specification

> Outline high-level calculations, models, concepts, or intent.

## Script Execution Specification

> This section's subsections detail how the yielded-to script should execute.
> It should include all relevant fields of the `ScriptContext` that are used for validation.
> You should also include whether or not the TxF is "composable".
> If so, you should indicate in what ways ("accepts additional minted tokens", "can spend additional script inputs as long as they are not members of set XYZ").
> If not, indicate exactly how non-composability ("strictness") is achieved (via the redeemer map, sparse merkle trees, examining the mints, strict equality checking of `txInfoWithdrawals`, etc.)

### Transaction Family Parameters

> "Transaction Family Parameters" are those arguments which parameterize _every_ transaction in the family.
> Things like script hashes, global parameters, and so forth.
> These parameters will be relatively static for a given instance of the protocol.
> These parameters may be compiled into the onchain scripts, or may be read in from a reference input storing global protocol parameters in the datum.
> In Haskell, they will usually be passed as part of a ReaderT environment (especially in offchain environments)
> I.e., expect a signature like
> `ReaderT ProtocolParams TxBuilder TxBody`
> In this section, we describe the parameters in "ProtocolParams" that are intended to be used to construct transactions in this family.
> Give their types and their intended use.

### Transaction Family Arguments

> "Transaction Family Arguments" are those arguments which are required to build a specific transaction in the family.
> These would be things like specific user wallets, specific token amounts, specific user-provided redeemers or datums, and so forth.
> These arguments will be dynamic and dependent on the specific request being made.
> In this section, we describe the arguments, give their types and indicate their intended use.

### Scripts and Redeemers Used

> A transaction family specified according to the YTxP Architecture will typically trigger multiple scripts:
>
> - A "AuthorisedScript" script (usually a staking validator)
> - One or more "Yielding" scripts
> - Additional scripts needed for integration with other protocols or non-YTxP Architecture scripts
> Each of these scripts should be listed here.
> If it is not immediately clear why a specific script is used, as brief description can be given.
> Along with each script, the designer should indicate the expected redeemer -- it's type and intended semantics.

### Reference Inputs

> Indicate the reference inputs (including reference script UTxOs) that will or may be included in transactions in this family.
> If their purpose is not immediately obvious, descriptions may be given.

### Inputs

> Indicate the inputs that will be spent in transactions in this family.
> Include their cardinality (1, more than 1, `n`, less than 5, etc.).
> Include wallet UTxOs.
> Inputs with fixed cardinalities should be specified prior to inputs with variadic cardinalities.
> If the exact usage of the input is not obvious, descriptions should be provided.
> This may include a description of the address, datum, redeemer, value, or reference script associated with the input.
> Be sure to indicate explicitly whether or not additional inputs will be allowed.

### Mints

> Indicate the tokens that will be minted or burned during this transaction family
> Include their multiplicity (1, 2, `n`, `== length Foo`, "according to formula X in [the above section]").

### Outputs

> Indicate the outputs that will be produced via transactions in this family.
> Include their cardinality (1, more than 1, `n`, less than 5, etc.).
> Always include at least one wallet UTxO.
> This will almost always be needed to pay "change" to the transaction submitter.
> Outputs with fixed cardinalities should be specified prior to inputs with variadic cardinalities.
> If the exact meaning of an output is not obvious, descriptions should be provided.
> This may include a description of the address, datum, value, or reference script associated with the input.
> Be sure to indicate explicitly whether or not additional outputs will be allowed.

### Staking Events

> Indicate what staking events are permitted in this transaction family

### Validity Range

> Indicate the validity range of this transaction family

### Signatories

> Indicate the required, prohibited, or option

## Asserted Properties

> This section contains asserted properties of the transaction family.
> These properties outline test cases and avenues for formal verification.
> They may include things like:
>
> - Component invariants that should be tested
> - "Exclusivity properties" -- state transitions (such as flipping a boolean flag or introducing/terminating a component) that are _only_ performed by this TxF
> - Mathematical properties of the arithmetic, such as pre-or-post-conditions
> - See [How to specify it!](https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf) by John Hughes -- postconditions, metamorphic, and model-based properties can go here.
