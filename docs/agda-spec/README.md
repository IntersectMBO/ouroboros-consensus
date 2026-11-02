# Agda specification of the Consensus Layer

Read the [formal specification](SPECIFICATION.md) directly in this repository.
It currently covers block header validation, including Leios endorser block
announcement and certification, but will eventually cover other consensus-related
features such as chain selection, HFC, mempools, etc. The specification is machine-checkable and
executable as a reference implementation for conformance testing.

## Source layout

- `src/Axiom`: The Agda set theory library.
- `src/Data`: Extensions to the Agda standard library.
- `src/Foreign`: General utilities to generate Haskell code from Agda code.
- `src/Interface`: General-purpose type classes not included in the Agda standard library.
- `src/InterfaceLibrary`: Interfaces exposed by other Cardano components,
  currently the ledger.

  - `Common`: Component-agnostic features, such as stake pool distributions.

- `src/Ledger`: Components borrowed from the ledger specification, including
  shared types, slots, epochs, cryptographic primitives, and protocol parameters.
- `src/Reflection`: Extensions to the Agda standard library's reflection support.
- `src/Tactic`: General-purpose tactics not included in the Agda standard library.
- `src/Spec`: Root directory of the consensus specification.

  - `<STS>.lagda.md` (e.g. `TickNonce.lagda.md` for TICKN): The definition of
    `<STS>` alongside its human-readable Markdown explanation.
  - `<STS>/Properties.agda` (e.g. `TickNonce/Properties.agda` for TICKN): Proofs
    of properties about `<STS>`, including a computation and a proof that it
    agrees with the transition rules.
  - `Specification.agda`: Imports the formal specification and its computation
    proofs for typechecking.
  - `Foreign`: Machinery for generating the executable Haskell specification.

    - `HSConsensus/<STS>.agda` (e.g. `HSConsensus/TickNonce.agda` for TICKN):
      Generates Haskell types for `<STS>` and a `*Step` function to execute it.
    - `ExternalFunctions.agda`: Defines the generated Haskell record of external
      functions whose implementations are supplied by the calling environment.
      Dummy implementations are also available.
    - `HSTypes.agda`: Generates Haskell versions of common Agda types, such as
      sets and maps.
    - `HSConsensus.agda`: Top-level module for extracting the executable specification.

  - `hs-src`: Haskell library exports and tests for the executable specification.

    - `Lib.hs`: Exposes the generated Haskell types and transition functions.
    - `test/<STS>Spec.hs` (e.g. `test/TickNonceSpec.hs` for TICKN): Tests the
      executable version of `<STS>`.

- `src/Everything.agda`: Typechecking entry point for the specification and
  Haskell interface.
- `SPECIFICATION.md`: Reading index for the Markdown specification.
- `docs`: Additional specification prose, equations, dependency diagram, and references.

## Checking the specification

From the repository root, enter the development shell:

```bash
nix develop .#agda-spec
```

From `docs/agda-spec`, run:

```bash
make check
```

This checks `src/Everything.agda`, including the formal specification, computation
proofs, and Haskell interface. `make docs` runs the same check; there is no rendering
step or LaTeX dependency. Alternatively, run the typecheck job from the repository
root:

```bash
nix build .#agda-spec.typecheck
```

This job produces only a success marker; read the Markdown specification directly
in the repository.

## Editing the specification

Edit the `.lagda.md` sources directly. Agda checks fenced blocks marked `agda`,
including blocks inside HTML comments. Use comments for imports, module parameters,
and other support code that should be hidden in the rendered specification.
Preserve indentation across code blocks: together they form a single Agda module.

Use relative Markdown links for local references, direct links for citations,
GitHub math syntax for equations, and Mermaid fences for diagrams. After editing,
run `make check`. A rule change may also require updating its `Properties.agda`
computation and proof, the corresponding `Spec/Foreign/HSConsensus` wrapper, and
its Haskell tests.

## Generating and testing the executable specification

From the repository root:

```bash
nix build .#agda-spec.hsSrc
nix build .#agda-spec.hsExe --rebuild --print-build-logs 2>&1
```

The first command extracts Haskell through Agda's MAlonzo backend, copies
`src/Spec/hs-src`, and fills the Cabal file's generated module list. The second
builds and tests that executable specification.

Alternatively, inside the development shell, from `docs/agda-spec`:

```bash
make hs
make hsTest
```

The generated project is in `src/dist/haskell/Spec`. To remove the generated
Haskell sources and build artifacts after building or testing, run the following
from `docs/agda-spec`:

```bash
make clean
```

This removes `src/dist/`. Agda's typechecking cache in `_build/` is retained,
so subsequent checks can reuse it. Checking the Markdown specification with
`make check` or `make docs` does not produce documentation artifacts to clean up.

## Setup without Nix

It is possible to perform the Agda checks and Haskell extraction without the use of Nix, using the instructions below:

- Install Agda version `2.7.0` (e.g. follow the instructions in <https://agda.readthedocs.io/en/v2.7.0/getting-started/installation.html#step-1-install-agda>
)

- In a folder `<LIB>`, clone the dependencies and checkout the respective commits/tags:

  | *Dependency*                                                       | *Tag/commit*                               |
  |--------------------------------------------------------------------|--------------------------------------------|
  | [agda-stdlib](https://github.com/agda/agda-stdlib)                 | `v2.1.1`                                   |
  | [agda-stdlib-classes](https://github.com/agda/agda-stdlib-classes) | `73f4da05aeea040fea4587629f9fd83a8f04e656` |
  | [agda-stdlib-meta](https://github.com/agda/agda-stdlib-meta)       | `v2.1.1`                                   |

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

  To build targets from the Makefile (e.g. `check`), use:
  ```
  AGDA="agda --library-file <LIB>/libraries" make <TARGET>
  ```
