# Agda specification of the Consensus Layer

This directory contains the Agda specification of the Consensus Layer. The current version of the specification only covers block header validation but, since this is work in progress, the specification will eventually cover other consensus-related features such as chain selection, HFC, mempools, etc.

The specification is both *formal* (meaning that is machine-checked and amenable to formal proofs of properties) and *executable* (meaning that can be used as a reference implementation for conformance testing against the real implementation).

## Directory structure

The project is comprised by the following subdirectories:

* `Axiom`: The Agda set theory library

* `Data`: Extensions to the Agda standard library

* `Foreign`: General utilities to automatically generate Haskell code from Agda code

* `Interface`: General-purpose type classes not included in the Agda standard library

* `InterfaceLibrary`: Interfaces exposed by other Cardano components, currently only for the Ledger Layer

  - `Common`: Component-agnostic features (e.g. stake pool distributions)

* `Ledger`: Components borrowed from the Ledger specification, most of them actually not Ledger-specific (e.g. slots, epochs, cryptographic primitives)

* `Reflection`: Extensions to the reflection support in the Agda standard library

* `Tactic`: General-purpose tactics not included in the Agda standard library

* `latex`: Auxiliary $$\LaTeX$$-related files required to generate the PDF version of the specification (e.g. fonts, references and static content)

* `Spec`: Root directory of the specification

  - `<STS>.lagda` (e.g. `TickNonce.lagda` for TICKN): The definition of `<STS>` in a human-readable format

  - `<STS>/Properties.agda` (e.g. `TickNonce/Properties.agda` for TICKN): Proofs of properties about `<STS>`. In particular, this file contains a proof that `<STS>` can be 'computed' by a given function. This means that we have an executable version of `<STS>` which is guaranteed to be correct

  - `PDF.lagda`: Source Agda file from which the corresponding PDF file is generated

  - `Foreign`: Machinery required for the automatic generation of an executable (Haskell) version of the Agda specification

    - `HSConsensus/<STS>.agda` (e.g. `HSConsensus/TickNonce.agda` for TICKN): Contains the code to automatically generate the Haskell types used by `<STS>` and a \*`Step` Haskell function to execute `<STS>`

    - `ExternalFunctions.agda`: Automatically generates a Haskell record of 'external functions'. An external function is a function used by the Agda specification whose Haskell version should be provided by the calling environment. Dummy external functions are also available

    - `HSTypes.agda`: Generates Haskell versions for common Agda types used in the specification, such as sets and maps

    - `HSConsensus.agda`: Top-level Agda module from which the executable specification is generated

  - `hs-src`: A Haskell test suite for the executable specification

  	- `test/<STS>Spec.hs` (e.g. `test/TickNonceSpec.hs` for TICKN): A Haskell program that tests the executable version of `<STS>`

## Generating the PDF file

To generate the specification in PDF format, one can use:

``` bash
> nix build .#agda-spec.docs
```

This command generates the `consensus-spec.pdf` file inside the resulting `pdfs` directory. Alternatively, one can enter the Nix shell:

``` bash
> nix develop .#agda-spec
```

and then use:

``` bash
> make docs
```

This command generates the `consensus-spec.pdf` file inside the resulting `src/dist/pdfs` directory. Also, temporary artifacts resulting from the build (such as `.aux` and `.bbl` files) are stored in `src/latex`. To remove these temporary files, one can use:

``` bash
> make clean
```

## Generating the executable specification

### Building

To build the executable (Haskell) specification, one can use:

``` bash
> nix build .#agda-spec.hsSrc
```

This command compiles the Agda code into Haskell code using the MAlonzo backend, placing the files in the resulting subdirectory `haskell/Spec/MAlonzo/Code`. In addition, it copies the contents of `src/Spec/hs-src` to `haskell/Spec` and completes the `library.other-modules` section of the `cardano-consensus-executable-spec.cabal` file with the names of the Haskell modules just generated.

Alternatively, one can enter the Nix shell:

``` bash
> nix develop .#agda-spec
```

and then use:

``` bash
> make hs
```

This command generates the same results as the previous alternative but using the directory `src/dist` as the temporary directory. As before, one can remove these temporary files by using:

``` bash
> make clean
```

### Testing

To run the test suite for the executable (Haskell) specification, one can use:

``` bash
> nix build .#agda-spec.hsExe --rebuild --print-build-logs 2>&1
```

The test results should show up near the end of the output. Alternatively, one can enter the Nix shell:

``` bash
> nix develop .#agda-spec
```

and then use:

``` bash
> make hsTest
```

## Setup without Nix

It is possible to perform the above-mentioned tasks without the use of Nix, using the instructions below:

- Install Agda version `2.7.0` (e.g. follow the instructions in <https://agda.readthedocs.io/en/v2.7.0/getting-started/installation.html#step-1-install-agda>
).

- In a folder `<LIB>`, clone the dependencies
    + [agda-stdlib](https://github.com/agda/agda-stdlib)
    + [agda-stdlib-classes](https://github.com/agda/agda-stdlib-classes)
    + [agda-stdlib-meta](https://github.com/agda/agda-stdlib-meta)

and checkout the commits/tags found in `nix/agda.nix` (e.g. `v2.1.1` for `agda-stdlib-meta`).

- Create a file `<LIB>/libraries` with the following content:
```
<LIB>/agda-stdlib/standard-library.agda-lib
<LIB>/agda-stdlib-classes/agda-stdlib-classes.agda-lib
<LIB>/agda-stdlib-meta/agda-stdlib-meta.agda-lib
```

- Instead of `agda` use `agda --library-file <LIB>/libraries`. For example, to typecheck `Everything.agda`:
  ```
  cd src/
  agda --library-file <LIB>/libraries Everything.agda
  ```

  To build targets from the Makefile (e.g. `docs`), use:
  ```
  AGDA="agda --library-file <LIB>/libraries" make <TARGET>
  ```
