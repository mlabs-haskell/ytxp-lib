# Yielding Transaction Pattern Library (ytxp-lib)

The YTxP is a flexible and secure architectural approach for modeling Cardano protocols using transaction families.

* [Understand the core concepts of YTxP](/docs/README.md)
* [How to write a YTxP specification](/ytxp-plutarch/examples/direct-offer/doc/tutorials/specification.md)
* How to set up and configure the `ytxp-lib` and implement a basic protocol using the YTxP.

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

* Using Nix: `nix flake check`: this will run all the checks, not only the tests;
* Using Cabal directly (assuming it is present in the `$PATH`). See the `Makefile` targets to check the available test suites.
