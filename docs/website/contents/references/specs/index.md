# Specifications

This section contains structured specifications for Ouroboros Consensus components.
Each spec precisely defines the state, rules, invariants, and transitions that the implementation must satisfy.
The specifications are designed to be the primary reference for both developers and AI agents working on the codebase.

- [Header Validation](./header-validation.md) — block header validation rules

## Specification format

Every specification follows this structure.

**Header.**
The spec name, a one-line summary, and references to the corresponding Agda definition, tech report section, and implementation modules (as module-qualified names).
Stable identifiers allow cross-referencing between specs and from external documents.

**State.**
The environment (read-only context), state (what the transition modifies), and signal (what triggers the transition), expressed as Quint-flavored type definitions.
This mirrors the STS (state transition system) pattern used in the Agda spec and the Cardano formal specifications, but in accessible notation.
For example:

```quint
type TickNonceEnv = {
  etaC: Nonce,   // candidate nonce
  etaPH: Nonce   // previous epoch's last block hash as nonce
}

type TickNonceState = {
  eta0: Nonce,   // epoch nonce
  etaH: Nonce    // previous epoch's last block hash nonce
}
```

**Rules.**
Each rule has a stable identifier (`RULE-<spec>-<n>`), a prose description, preconditions, the transition logic in Quint-flavored pseudocode, and postconditions.
Preconditions are explicit `require` statements — the Quint equivalent of Ethereum's `assert` or IBC's `abortTransactionUnless`.

**Examples.**
Concrete input/output pairs showing the rule in action, with actual values.
These are the primary disambiguator — research shows they are more effective than abstract rule statements at preventing LLM misinterpretation ([Akli et al., 2025](https://arxiv.org/abs/2604.24703)).

**Counter-examples.**
Concrete inputs that must be rejected, with the expected failure reason.
These prevent both over-permissive implementations (accepting invalid inputs) and over-defensive ones (rejecting valid inputs), a failure mode observed in LLM code generation ([Xu et al., 2025](https://openreview.net/forum?id=6pr7BUGkLp)).

**Invariants.**
Cross-cutting properties that must hold across all transitions, with a stable identifier (`INV-<spec>-<n>`).
Each invariant references the corresponding formal property or test where applicable.

## Design principles

These specifications are designed to be consumed by both humans and LLMs.
The format choices are grounded in recent research on how specification structure affects LLM reliability.

**Structured natural language with Quint-flavored pseudocode.**
LLMs are trained primarily on natural language and code.
Structured natural language with pseudocode and bulletized constraints sits squarely on-distribution — it matches the patterns LLMs have seen in textbooks, API documentation, and technical writing.
Research confirms this: when LLMs are allowed to choose their own specification format, they converge on structured NL with pseudocode and checklists, not formal notation ([Xu et al., 2025](https://openreview.net/forum?id=6pr7BUGkLp)).
Formal intermediate representations unfamiliar to the model (e.g., Dafny) actively hurt code generation accuracy because they are off-distribution.

We use [Quint](https://github.com/informalsystems/quint)-flavored pseudocode for data structures and transition rules.
Quint is a specification language designed for distributed protocols that [positions itself as "between English and code"](https://quint.sh/posts/llm_era) — more abstract than implementation code (easier to reason about) but structured unlike prose (mechanically unambiguous).
Its syntax resembles TypeScript, making it readable to anyone familiar with mainstream programming languages.
It is deliberately not Haskell, which avoids the temptation to copy-paste from the implementation and the resulting drift risk.
The pseudocode can be tightened into valid, executable Quint in a later phase.

**Concrete examples over abstract rules.**
Every rule includes concrete input-to-output examples and counter-examples showing rejected inputs.
This addresses the under-specification problem that research identifies as the primary cause of LLM code generation failures ([Akli et al., 2025](https://arxiv.org/abs/2604.24703)).

**References, not copies.**
Specifications reference implementation code via module-qualified names (e.g., `Ouroboros.Consensus.Protocol.Praos:validateHeader`) rather than embedding Haskell snippets.
This eliminates drift between spec and implementation.
The spec is the source of truth for *what* must hold; the code is the source of truth for *how*.

**Token efficiency.**
Specifications minimize redundancy and avoid verbose boilerplate.
Research on LLM-optimized specification formats ([Garcia, 2025](https://arxiv.org/abs/2602.18541)) found that eliminating redundancy can reduce token overhead by 85% while preserving semantic content.

## Prior art

We surveyed specification formats used in comparable blockchain protocol projects:

- [Ethereum consensus specs](https://github.com/ethereum/consensus-specs): Markdown with embedded executable Python.
Types as tables, state transitions as Python functions, preconditions as `assert` statements.
Effective because the spec is executable, but couples the spec to a specific language.
- [IBC/Cosmos specs](https://github.com/cosmos/ibc): Markdown with TypeScript-like pseudocode.
Hybrid approach: formal-ish pseudocode alongside prose.
Uses `abortTransactionUnless()` for preconditions.
- SDD tools ([Kiro](https://kiro.dev/), [spec-kit](https://github.com/github/spec-kit), [Tessl](https://tessl.io/)): Markdown with structured metadata (GIVEN/WHEN/THEN, checklists, `@generate`/`@test` tags).
Application-development oriented; less relevant for protocol specification.

Our format draws from the Ethereum approach (types and transitions in pseudocode, preconditions as explicit checks) and the IBC approach (prose alongside pseudocode, explicit desired properties section), while using Quint-flavored syntax instead of Python or TypeScript.

## Relationship to other artifacts

| Layer | Artifact | Role |
|-------|----------|------|
| L1 | [Quint](https://github.com/informalsystems/quint) formal spec (future) | Model-checkable properties |
| L2 | [Agda spec](../../agda-spec/) (existing) | Machine-checked transition rules |
| **L3** | **This specification** | **Primary reference for developers and LLMs** |
| L4 | [Website documentation](../../) | Explanations, tutorials, how-tos |
| L5 | [Haddock](../haddocks.md) | API-level documentation |

## Why a new specification format?

Ouroboros Consensus has an existing formal specification written in Agda (`docs/agda-spec/`).
It covers header validation through six state transition systems: TICKN, UPDN, TICKF, OCERT, PRTCL, and CHAINHEAD.
The Agda spec is precise and machine-checked, but it has practical limitations that prevent it from serving as the primary specification artifact for developers and AI agents.

### Limitations of the Agda spec

**Accessibility.**
Agda requires familiarity with dependent types, universe polymorphism, and instance resolution.
Few developers can read it; fewer can maintain it.
Every module takes a chain of 5-7 type parameters (`crypto`, `nonces`, `es`, `bs`, `af`, `li`, `rs`), making it impossible to understand a single module in isolation.

**Hidden definitions.**
Critical definitions (record constructors, field accessors, data declarations) live in `\begin{code}[hide]` blocks, invisible in the rendered PDF.
A reader of the published spec sees an incomplete picture unless they read the raw `.lagda` source.

**No concrete examples.**
The spec defines abstract transition rules but never shows a concrete state transition with actual values.
Research on LLM code generation ([Akli et al., 2025](https://arxiv.org/abs/2604.24703)) identifies under-specification as the most damaging defect type for code reliability.
Concrete input/output examples are the most effective disambiguator.

**No conformance testing.**
The Properties modules prove that each transition is computable (deterministic), not that the Haskell implementation matches the spec.
The Agda spec and the codebase are not connected by any test.

**No link to implementation.**
Nothing in the Haskell codebase references the Agda spec.
There is no path from `validateHeader` in Haskell to the corresponding CHAINHEAD rule.

**Off-distribution for LLMs.**
Agda is extremely rare in LLM training data.
Research on specification formats ([Xu et al., 2025](https://openreview.net/forum?id=6pr7BUGkLp)) found that formal intermediate representations unfamiliar to the model (e.g., Dafny) actively hurt code generation accuracy compared to structured natural language, because they are off-distribution.

## References

- Xu, Z. et al. (2025). ["Self-Spec: Model-Authored Specifications for Reliable LLM Code Generation."](https://openreview.net/forum?id=6pr7BUGkLp) OpenReview.
- Akli, A. et al. (2025). ["Defective Task Descriptions in LLM-Based Code Generation."](https://arxiv.org/abs/2604.24703) arXiv:2604.24703.
- Garcia, D. (2025). ["LAPIS: Lightweight API Specification for Intelligent Systems."](https://arxiv.org/abs/2602.18541) arXiv:2602.18541.
- Informal Systems. (2025). ["Reliable Software in the LLM Era."](https://quint.sh/posts/llm_era) Quint blog.
