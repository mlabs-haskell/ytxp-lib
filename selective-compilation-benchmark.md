# Selective Compilation Benchmark

This document outlines the results of a script size benchmark conducted by manually compiling various scripts and comparing their sizes.

## Benchmark Command

The following command was used to compile the scripts:

```shell
cabal run ytxp -f dev -- -o blueprint-m.json -m 1 --stcs "333333" --txf-purpose [PURPOSE]
```

## Benchmark Results

The table below summarizes the sizes of the compiled scripts for different combinations of purposes:

| Compilation                 | Size  |
|-----------------------------|-------|
| Minting                     | 2.9K  |
| Spending                    | 3.0K  |
| Rewarding                   | 3.0K  |
| Minting + Spending          | 3.4K  |
| Minting + Rewarding         | 3.4K  |
| Spending + Rewarding        | 3.5K  |
| Minting + Spending + Rewarding | 3.8K  |
