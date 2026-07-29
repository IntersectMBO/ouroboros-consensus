# Three Mempool Specifications

All under `ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Mempool/`:

- **`Mempool.lagda.md`** — Praos-era mempool. Current implementation.
  Single sequence of transactions, single ledger state, `snapshotTxsAfter`
  streaming to peers, fairness FIFO for remote vs. local admission.

- **`MempoolLeios.lagda.md`** — Linear Leios adaptation, aligned with
  CIP-164. Still a single tier. Adds `heldEB` with speculative `ebLedger`,
  `seeRBBody`, `seeRBCert` (Scenario A / B), `discardEB`, extended
  `syncWithLedger`, and `forgeBlock` returning `(RB, Maybe EB)`. EBs reach
  the ledger via a vote certificate carried in a later RB — RBs never
  contain an EB directly.

- **`MempoolLeiosPricing.lagda.md`** — Tiered-pricing extension on top of
  Leios. Two tiers (priority, standard). Tier-aware admission, forging, and
  peer exchange. Includes a light-load EB-suppression rule and a
  priority-fee refund for priority txs that end up in an EB.

Each file is self-contained. The pricing doc marks its deltas from Leios
with `-- CHG:` / `-- NEW:` comments in the Agda block.

---

# Pricing Mempool: Key Features vs. Leios

- **Two tiers** — priority → RB body, standard → announced EB
- **Two caps** — `priorityCap` = one RB; `standardCap` = CIP-164 per-EB caps
- **Layered validation** — `ledger` → `ebLedger` → `priorityUpdatedLedger`
  → `standardUpdatedLedger`; each tx validates against its tier's post-state
- **Cascading admission** — a priority tx always revalidates the standard tier
- **Forging** — priority overflow spills into the EB, ahead of standard txs
- **Fees** — priority tx landing in an EB pays the standard fee;
  difference refunded (no change address ⇒ donated to treasury)
- **EB suppression** — no EB below the fullness floor `ebFloor` (= ½ RB);
  forced anyway if no EB announced for `ageScape` slots (age escape)
- **Tier-aware peer exchange** — inbound txs tagged; outbound pull
  priority-first; per-tier cursors
- **Chain events unchanged** — Scenario B tick-and-rename, `discardEB`,
  etc. as in Leios, applied per tier
