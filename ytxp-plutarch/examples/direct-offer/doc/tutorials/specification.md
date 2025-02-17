# How to write a YTxP specification

In this tutorial, we learn how to specify a specification for a direct-offer contract.

Prerequisites:

- Understanding of Cardano's EUTxO model and smart contract architecture.
- [A solid understanding of the core concepts of the YTxP.](/docs/README.md)

## What is the direct offer contract?

The direct offer is a simple contract that enables peer-to-peer trading. Users can create an offer order by specifying the return and assigning a value as the exchange without needing a trusted third party. The offer owner can revoke its offer order at any time. Anyone can execute the offer order if the order's conditions are fulfilled.

## How to write the direct offer specification

The specification generally includes one component specification if using the component model, along with one or more transaction family specifications. Additionally, it may contain one or more transaction flow specifications for more complex cases.

### Component

For this contract, we will utilize the component model. In this model, an offer order is represented by a UTxO that includes a datum detailing the offer's specifics, and a state thread token verifies the semantic validity of the offer UTxO.

For the specification, we will detail the existence of a component and describe all its attributes, including the UTxO's address, value, datum, and reference script.

Specifically, we will define the attributes of the offer component in the following way:

- We will not specify any particular values, as the component's value is what the consumer can keep.
- The address must be a YTxP yielding-validator address to ensure that the yielding validator will only validate when an authorized transaction family is also validated.
- The datum consists of two fields: the "owner" and the "to buy" value.
- No scripts are allowed to be referenced.

If a component has multiple states, we will specify that in the documentation. However, the offer component has no meaningful states, so we will not provide any details.

Finally, we will specify which transaction family will introduce, modify, and terminate this component. Please visit to the example of the [offer component specification](/ytxp-plutarch/examples/direct-offer/doc/components/offer.md).

The component specification serves only as information and does not detail the transaction validation for the offer component. Validation of the component is delegated to the authorized transaction families.

### Transaction families
To determine the transaction families involved in the direct offer contract, we need to identify all atomic transactions that can take place.

- Create an order
- Reclaim an order
- Execute an order

These transactions represent the transaction families: Creating, Reclaiming, and Executing.

A transaction family specification outlines the details necessary for transaction validation.

The transaction family itself is an authorized script that can be implemented with any validator types. However, it is recommended to use the staking validator, as it only requires a zero withdrawal to activate this validator.

For the all possible details of how to specify a transaction validation we encourage to use the templates in the ./template/ directory.

Visit the direct offer specification examples for guidance on how to specify validation for the transaction families: [Creating](/ytxp-plutarch/examples/direct-offer/doc/transaction-families/creating.md), [Executing](/ytxp-plutarch/examples/direct-offer/doc/transaction-families/executing.md ), and [Reclaiming](/ytxp-plutarch/examples/direct-offer/doc/transaction-families/reclaiming.md).

### Transaction flows

We do not have any transaction flow for the offer contract. A transaction flow typically outlines one or more transaction families, which is often relevant in the context of a sub-state machine.

## Closing Remarks

You can find the complete specifications for the direct offer [here](/ytxp-plutarch/examples/direct-offer/doc/README.md).
