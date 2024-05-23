# Staking Validator Management Flow

This transaction flow is required to set up the rewards accounts for the staking validators, including:

- The (nonced) Yielding Staking Validators
- The yielded-to staking validators that will encode the majority of the transaction families in a YTxP protocol

The specification of these transaction families are loosely defined, because the situations in which you may want to trigger such transactions are varied.
Library implementations may choose to omit one or more if the underlying offchain framework provides sufficient utilities for building such transaction directly.
Nonetheless, transactions in these families _will_ be necessary to work with the YTxP, so library implementors should ensure that these needs are met.
