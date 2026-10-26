# A Formal Specification of the Cardano Consensus

This specification describes the Cardano consensus layer. It currently covers
block header validation, including the announcement and certification of Leios
endorser blocks. The rules are formal, machine-checkable, and executable.

The specification is written in literate Agda Markdown (`.lagda.md`). Agda checks
the fenced Agda code blocks, including those inside HTML comments. Some displayed
snippets are presentation copies that omit supporting declarations or proof code.
This keeps the presentation focused on the definitions and rules.

## Reading order

1. [Transition rule dependencies](docs/transition-rules.md)
2. [Cryptographic primitives](src/Ledger/Crypto.lagda.md)
3. [Ledger interface](src/InterfaceLibrary/Ledger.lagda.md)
4. [Block definitions](src/Spec/BlockDefinitions.lagda.md)
5. [TICKN: epoch nonce updates](src/Spec/TickNonce.lagda.md)
6. [UPDN: evolving and candidate nonce updates](src/Spec/UpdateNonce.lagda.md)
7. [OCERT: operational certificate validation](src/Spec/OperationalCertificate.lagda.md)
8. [PRTCL: protocol validation](src/Spec/Protocol.lagda.md)
9. [TICKF: ledger forecasting](src/Spec/TickForecast.lagda.md)
10. [CHAINHEAD: block header validation](src/Spec/ChainHead.lagda.md)
11. [Properties](docs/properties.md)
12. [Leader value calculation](docs/leader-value.md)
13. [References](docs/references.md)

`CHAINHEAD` is the main transition. It calls `TICKF`, `TICKN`, and `PRTCL`;
`PRTCL` calls `UPDN` and `OCERT`. Each transition's `Properties.agda` module
provides a computation and a proof that it agrees with the corresponding rule.
The additional properties discussed in the [properties chapter](docs/properties.md)
are goals for the specification.

See the [README](README.md) for typechecking, Haskell extraction, and testing.

The initial specification was written by Javier Díaz, IOHK Formal Methods Team,
in June 2024.
