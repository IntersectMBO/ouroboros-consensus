# Ouroboros Consensus

[![consensus](https://img.shields.io/badge/ouroboros--consensus-0.28.0.0-blue)](https://chap.intersectmbo.org/package/ouroboros-consensus-0.28.0.0/)
[![diffusion](https://img.shields.io/badge/ouroboros--consensus--diffusion-0.24.0.0-blue)](https://chap.intersectmbo.org/package/ouroboros-consensus-diffusion-0.24.0.0/)
[![protocol](https://img.shields.io/badge/ouroboros--consensus--protocol-0.13.0.0-blue)](https://chap.intersectmbo.org/package/ouroboros-consensus-protocol-0.13.0.0/)
[![cardano](https://img.shields.io/badge/ouroboros--consensus--cardano-0.26.0.0-blue)](https://chap.intersectmbo.org/package/ouroboros-consensus-cardano-0.26.0.0/)
[![sop-extras](https://img.shields.io/badge/sop--extras-0.4.1.0-blue)](https://chap.intersectmbo.org/package/sop-extras-0.4.1.0/)
[![strict-sop-core](https://img.shields.io/badge/strict--sop--core-0.1.3.0-blue)](https://chap.intersectmbo.org/package/strict-sop-core-0.1.3.0/)

[![docs](https://img.shields.io/badge/Documentation-yellow)][webpage]

Implementation of the Ouroboros family of consensus algorithms.

---

# ✨ What Is Ouroboros? (Beginner-Friendly Overview)

**Ouroboros is the Proof-of-Stake (PoS) consensus protocol that powers the Cardano blockchain.**
It ensures that:

* The blockchain keeps growing correctly
* Transactions are validated securely
* Blocks are produced in a decentralized way

Here is a simple explanation of how it works.

## 🔷 How Ouroboros Works — Explained for Beginners

Ouroboros works by dividing time into **epochs** and **slots**.

### **1. Epochs and Slots**

* An **epoch** is a large time period (e.g., 5 days on Cardano mainnet)
* Each epoch is divided into many **slots** (1 second each)
* Each slot has the potential for one block to be created

### **2. Slot Leaders**

In each slot, the protocol randomly chooses a **slot leader**.
Slot leaders:

* Are usually stake pools
* Are chosen based on how much ADA they control (more stake = higher chance)
* Create and validate blocks during their assigned slot

### **3. Proof-of-Stake Selection**

Ouroboros uses cryptographic randomness to pick leaders. This randomness:

* Is unpredictable
* Cannot be manipulated by attackers
* Ensures fairness across all stake pools

### **4. Chain Selection Rule**

Unlike Proof-of-Work where the longest chain wins, Ouroboros uses:

**The chain with the most accumulated stake from valid block producers wins.**

This ensures:

* Security
* Resistance to long-range attacks
* Consensus that remains decentralized even if some nodes go offline

### **5. Finality and Safety**

* Once a block has enough confirmations, it becomes practically irreversible
* Ouroboros is proven secure in academic peer-reviewed papers

---

# Libraries and Executables

This repository provides four packages with dependencies among them:

``` mermaid
flowchart TD
    D[ouroboros-consensus-diffusion] --> C
    A[ouroboros-consensus-cardano] --> B[ouroboros-consensus-protocol]
    A --> C
    B --> C[ouroboros-consensus]
```

The many test-suites in the repository create additional dependency links.

It also provides four executables:

* **db-analyser** – Inspect ChainDBs for correctness and performance
* **db-synthesizer** – Generate synthetic chains for benchmarking
* **db-truncater** – Truncate immutable databases
* **immdb-server** – Serve a local immutable DB via network

To list all cabal components:

```bash
for f in $(find ouroboros-consensus* *sop* -type f -name "*.cabal"); do
    printf "Components of package %s:\n" $f;
    grep -E "^(library|test-suite|executable|benchmark)" $f --color=never | column -t | sort | sed 's/^/\t/'
done
```

---

# Building the Project

```bash
cabal build all
```

Run executables:

```bash
cabal run db-analyser
```

---

# Testing the Project

```bash
cabal test all
```

Run a specific suite:

```bash
cabal run ouroboros-consensus:test:consensus-test -- <args>
```

Or:

```bash
cabal test ouroboros-consensus:test:consensus-test --test-show-details=direct
```

---

# Using Consensus as a Dependency

To use CHaP, add this to the top of `cabal.project`:

```
repository cardano-haskell-packages
  url: https://chap.intersectmbo.org
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee
```

Then run:

```bash
cabal update
```

Specify index-state:

```
index-state:
  , hackage.haskell.org      2023-04-12T00:00:00Z
  , cardano-haskell-packages 2023-04-23T00:00:00Z
```

> ⚠️ **Warning:** Internal testing sublibraries are unstable and may break without version bumps.

---

# How to Contribute

See the [CONTRIBUTING.md](CONTRIBUTING.md) file.

---

# Submitting Issues

Open an issue at:
[https://github.com/IntersectMBO/ouroboros-consensus/issues](https://github.com/IntersectMBO/ouroboros-consensus/issues)

---

# Documentation

* **Haddocks:** API reference
* **Website:** High-level docs and architecture
* **Technical Report:** Deep design reasoning and formal properties
* **Agda Specification:** Formal consensus modeling

[webpage]: https://ouroboros-consensus.cardano.intersectmbo.org/
