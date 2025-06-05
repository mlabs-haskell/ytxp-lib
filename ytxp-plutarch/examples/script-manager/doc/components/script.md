# Script component

This component holds the deployed script.

## UTxO Specification

The UTxO reflecting this component has the following attributes:

### Address

The component sits on the address of the YTxP yielding-validator of the protocol.

### Value

Undefined.

### Datum

The component must have a unit datum.

### Reference Script

The content is undefined, although it is expected to contain the deployed script.

## States

The Script component does not have meaningful states.

## Participation in Transaction Families

### Introduction

- [Script deploying](../transaction-families/creating.md)

### Modification

None.

### Termination

- [Script removing](../transaction-families/removing.md)
