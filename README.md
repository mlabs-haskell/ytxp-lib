# Yielding Transaction Pattern Library (ytxp-lib)

The YTxP is a flexible and secure architectural approach for modeling Cardano protocols using transaction families.

* [An Introduction to the Concepts Behind YTxP Architecture](https://www.mlabs.city/blog/an-introduction-to-the-concepts-behind-ytxp-architecture)
* [Understand the core concepts of YTxP](/docs/)
* [How to write a YTxP specification](/ytxp-plutarch/examples/direct-offer/doc/tutorials/specification.md)
* [How to set up YTxP](/ytxp-plutarch/examples/direct-offer/doc/tutorials/setup.md)

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

The current Haddock documentation is available [here](https://mlabs-haskell.github.io/ytxp-lib/). Below are instructions for generating local Haddock documentation using various methods:

#### Build

##### Using Make Target
To build documentation directly, utilize the following make target:

```bash
make build_docs
```

After the execution, this command will specify the location of the generated documentation.

#### Serve documentation locally

To serve the documentation locally, utilize the following make target:

```bash
make serve_docs
```

The resulting documentation will be accessible within the `result-doc` directory.

### ytxp Executable

The `ytxp` executable is a command-line tool for compiling the yielding validators for the YTxP.

#### Options

The `ytxp` executable supports the following command-line options:

* `--output` or `-o`: Specifies the output blueprint file. **Required**.
* `--yielding-staking-validator-number` or `-s`: Specifies the number of yielding staking validators. **Optional**.
* `--yielding-minting-policy-number` or `-m`: Specifies the number of yielding minting policies. **Optional**.
* `--yielding-certifying-validator-number` or `-c`: Specifies the number of yielding certifying validators. **Optional**.
* `--yielding-voting-validator-number` or `-v`: Specifies the number of yielding voting validators. **Optional**.
* `--yielding-proposing-validator-number` or `-p`: Specifies the number of yielding proposing validators. **Optional**.
* `--stcs`: Specifies the authorised scripts STCS. **Required**.
* `--initial-nonce`: Specifies the initial nonce value. **Optional**. Default value is `42`.
* `--txf-purpose`: Specifies which script purposes are permitted for authenticated scripts. The possible options include `minting`, `spending`, and `rewarding`. Multiple options can be selected. **Required**
* `--traces`: Enables tracing for the compiler. **Optional**.

#### Example

To generate a blueprint with five yielding staking validators, one yielding minting policy, and a yielding spending validator:

```bash
cabal run ytxp -- -o blueprint.json -s 5 -m 1 --stcs "333333" --txf-purpose rewarding
```

## Tests
Tests will run in CI thanks to some specific checks in the Nix flake.

You can run tests:

* Using Nix: `nix flake check`: this will run all the checks, not only the tests;
* Using Cabal directly (assuming it is present in the `$PATH`). See the `Makefile` targets to check the available test suites.
