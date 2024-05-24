# Yielding Transaction Pattern Library (ytxp-lib)

The Yielding Transaction Pattern is pretty simple.

- A Yielding Script only succeeds if it can find evidence that an _authorized_ "Yielded To" script also succeeded.
- The _authorization_ is determined by examination of the script context.
- The _authorization_ is checked by:
  - Indexing the reference inputs to find evidence that a "Yielded To" script containing a token minted with the `authorisedScriptsSTCS` is being referenced
  - Searching for evidence that this script was triggered
    In practice this will involve either:
    - Looking at the `txInfoWithdrawls` field for a staking validator being triggered with the correct StakingCredential
    - Looking at the `txInfoInputs` field for a UTxO being spent at the correct address
    - Looking at the `txInfoMints` field for a mint with the correct currency symbol

## About this documentation

The YTxP is something that can be implemented using a variety of onchain and offchain frameworks in a variety of programming languages.
This documentation details technical aspects of the types of activities that practitioners of the YTxP architecture will need to do; it is a specification of what a YTxP library should include.
It will describe the library primarily in terms of Haskell, but other languages should be able to easily adapt it.

The YTxP architecture is a principled approach to modeling and describing Cardano protocols via transaction families.
A goal of the architecture is to establish a common formula for such descriptions that is:

- flexible enough to apply to the _business logic_ of every protocol, even if the implementation would need reformulation
- restricted enough to give strong security guarantees
- compositional enough that there is a clear, principled, and sound way to decompose large protocols into distinct parts that can be easily examined both in isolation and as a complete system

This documentation includes some templates in the `./templates/` directory.
These templates are an attempt to establish the above formula.
Designers are encouraged to use these templates as a starting point for describing new functionalities in a protocol.

The YTxP itself is described using these templates.

### Documentation Structure

The documentation structure of a YTxP Protocol (as well as this library) can be made to reflect a reasonable module structure for the implementation of the protocol.
We give a directory for each Transaction Flow (see below) and a single file for each Transaction Family (see below) that is part of that flow.

> Note that transaction families need not be part of only one flow, and transaction flows can be composed or combined monadically; the directory structure given is a canonical association of a family to a flow, but it is not necessarily reflective of the full picture.

## Vocabulary and Nomenclature

## Component UTxO

A &ldquo;component UTxO&rdquo; is a useful model for protocol design. It is a UTxO
that is &ldquo;typed&rdquo; by the token name of its State Thread Token (see below).
The &ldquo;type&rdquo; of a component UTxO gives semantics to its associated
address, value, datum, and reference scripts.

The YTxP does not require using the &ldquo;component UTxO&rdquo; model, but it is
a natural fit.

## State Thread Token (STT)

A token that authenticates the semantic validity of a UTxO.
STTs are often referred to prefixed by the &ldquo;type&rdquo; of a Component UTxO,
such as a &ldquo;Vault State Thread Token&rdquo;.

Semantically, state thread tokens constitute a proof that one of the three
conditions hold for a given transaction:

- When a STT is minted in a transaction, it is a necessary (but perhaps insufficient) condition
    that the &ldquo;introduction rules&rdquo;, &ldquo;initial conditions&rdquo;, or &ldquo;base case&rdquo; governing the creation of
    a Component UTxO are satisfied
- When a STT is continued through a transaction, it is a necessary
    (but perhaps insufficient) condition that the transaction was a &ldquo;valid state transition&rdquo;
    or &ldquo;valid recursive case&rdquo; in terms of protocol semantics
- When an STT is burned, it is a necessary (but perhaps insufficient) condition that
    the &ldquo;elimination rules&rdquo; or &ldquo;terminating case&rdquo; governing a component UTxO were satisfied

### STCS

An abbreviation for &ldquo;State Thread Currency Symbol&rdquo;

### STMP

An abbreviation for &ldquo;State Thread Minting Policy&rdquo;

## Component Token

A distinguished type of STT minted by the Yielding Minting Policy that indicates the &ldquo;type&rdquo; of a Component UTxO via its token name.

## Transaction Flows

A &ldquo;transaction flow&rdquo; is a combination of one-or-more &ldquo;transaction families&rdquo; that have intertwined semantics. These often form a sub-state-machine of the entire protocol; i.e., a loop within the overall state transition graph.

Transaction flows can be composed of other transaction flows as well.

## Transaction Family

&ldquo;Transaction Families&rdquo; define both semantics and constraints on <span class="underline">single</span>, atomic transactions. A transaction is considered to be a &ldquo;section of&rdquo;, &ldquo;member of&rdquo;, or &ldquo;element of&rdquo; a transaction family if it obeys the constraints of the transaction. For instance, wallet-to-wallet transactions form a transaction family &#x2013; any transaction that consumes <span class="underline">only</span> UTxOs at a single pubkey address and produces UTxOs at a single pubkey address would be a section of the transaction family.

Using the YTxP involves modeling a protocol as a series of disjoint transaction families; each transaction enabled by the protocol belongs to <span class="underline">only one</span> transaction family.

Transaction families are referred to by gerunds &#x2013; &ldquo;-ing words&rdquo;. I.e., a transaction family that buys a stablecoin is referred to by the term &ldquo;StableCoin Buying Transaction Family&rdquo;, rather than &ldquo;Buying StableCoin Transaction Family&rdquo; or &ldquo;Buy StableCoin Transaction&rdquo;. The model views transaction families as &ldquo;state machine transitions&rdquo; or &ldquo;methods&rdquo;.

### Transaction Family Hash

Transaction families are implemented as &ldquo;Yielded To&rdquo; scripts. These can be:

- staking validators, which are preferred because a &ldquo;withdraw 0&rdquo; transaction is more functionally &ldquo;pure&rdquo; (side-effect free) than other options
- minting policies, which are known as &ldquo;transaction tokens&rdquo;, and may be useful for legacy reasons or integration with existing protocols. However, these require minting a token, which may be a superfluous side-effect.
- validator hashes, which may be useful for legacy reasons or integration with existing protocols. However, these require spending a UTxO, which may be a superfluous side effect.

See [this](https://github.com/Anastasia-Labs/design-patterns/blob/main/stake-validator/STAKE-VALIDATOR-TRICK.md) for more details on the idea and execution of the
&ldquo;withdraw 0&rdquo;

## Tooling

### Continuous Integration (CI)
The CI for this project runs using [Hercules CI](https://hercules-ci.com). All the pre-commit checks will run in CI.

### Developer Experience (DevEx)
All the commands used for development purposes are exposed through the [Makefile](./Makefile). To see the available commands, you can simply run:

```bash
make
```

### Formatting
The format of most of the source files is checked. You can use individual commands through the `Makefile` or you can simply run:

```bash
make format_lint
```

to apply all the linters and formatters. This might be useful.

**Note:** Some linters cannot automatically fix your code. For example, `markdownlint` may signal that a code block (delimited by ```) does not have the language specified but cannot automatically infer the language of the code. This means that in general, `make format_lint` does not resolve all the problems that pre-commit checks can raise.

### Haddock Documentation

Below are instructions for generating Haddock documentation using different methods:

#### Build

##### Using Make Target
To build documentation directly, utilize the following make target:

```bash
make build_docs
```

After the execution, this command will specify the location of the generated documentation.

##### Using Nix

If you are using nix, the documentation for `ytxp-plutarch library` , generating documentation can be achieved by running:

```bash
nix build .#ytxp-plutarch-lib.doc
```

#### Serve documentation locally

To serve the documentation locally, utilize the following make target:

```bash
make serve_docs
```

The resulting documentation will be accessible within the `result-doc` directory.

## Tests
Tests will run in CI thanks to some specific checks in the Nix flake.

You can run tests:

- Using Nix: `nix flake check`: this will run all the checks, not only the tests;
- Using Cabal directly (assuming it is present in the `$PATH`). See the `Makefile` targets to check the available test suites.
