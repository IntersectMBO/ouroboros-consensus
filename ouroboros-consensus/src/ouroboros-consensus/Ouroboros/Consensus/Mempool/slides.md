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
  Leios. Two tiers (fast, slow). Tier-aware admission, forging, and
  peer exchange. Includes a light-load EB-suppression rule and a
  fast-fee refund for fast txs that end up in an EB.

Each file is self-contained. The pricing doc marks its deltas from Leios
with `-- CHG:` / `-- NEW:` comments in the Agda block.

---

# Pricing Mempool: Key Changes vs. Leios

- **Two tiers.** `fastTxs` destined for the RB body, `slowTxs` for
  the announced EB. Separate caps: `fastCap` (one RB `TxMeasure`) and
  `slowCap` (CIP-164 per-EB caps: `S_EB`, `S_EB-tx`, per-EB Plutus).

- **Layered ledger stack.** `ledger` (chain tip) → `ebLedger : Maybe`
  (with `heldEB` applied) → `fastUpdatedLedger` (fasts on top)
  → `slowUpdatedLedger` (slows on top). Each new tx validates
  against its tier's post-state.

- **Fast admission cascades.** `addTx Fast t` validates against
  `fastUpdatedLedger`, then unconditionally revalidates `slowTxs`
  on the new post-state. Commutativity-based option-1 admission is
  tracked as a future variant, pending the WIP proof at
  `IntersectMBO/formal-ledger-specifications:polina/commutativity`.

- **Scenario B zero-cost rename.** When a cert for our `heldEB` lands,
  both tiers' working states are preserved bit-identically; only the
  ledger-stack fields shift. O(1) transition.

- **`forgeBlock` reapplies against `ledger`.** Safe to call regardless of
  `heldEB`. Fast overflow (fast txs that don't fit `fastCap`)
  flows into the EB body ahead of slows.

- **Fee on an EB-landed fast tx.** The ledger charges/refunds on the
  tier the tx *actually* lands in (EB ⇒ slow), not its claimed tier:
  with a `feeChangeAddr` it's charged the slow fee and refunded
  `txFee − slowCoeff·minfee`; with none, the excess above `minfee` is
  donated to the treasury. The admission check used its claimed (fast)
  tier. The mempool only preserves the tier tag; the ledger does the split.

- **EB suppression under light load.** No EB when the **EB body** is below
  the fullness floor `ebFloor` in every dimension — byte size, reference-script
  bytes, ExUnits mem, and ExUnits CPU. `ebFloor` = **½ a full RB** (a candidate
  protocol parameter), so there's no `/2` / rounding. This is a *lower* bound,
  distinct from `slowCap` — the CIP-164 per-EB *capacity* (`S_EB`, …) that bounds
  the EB body from above. Exact complement of the ledger's EB validity check
  (`sdChecks` for `EB`, via `BBODY`/`DIVUP`): both measure the EB body against
  `ebFloor` over the same four dimensions (ref-script bytes via the ledger's
  `totalRefScriptSize`), so an EB is suppressed here iff the ledger would reject
  it. (The "small in *every* dimension" quantifier is probably up for
  discussion; enforcing the CIP-164 per-EB capacity ledger-side is a TODO.)

- **Peer exchange.** Inbound routes by tier tag on each tx.
  Outbound pulls fast-first: keep asking peers for fast txs;
  fall back to slows only when fast requests come back empty.
  `snapshotTxsAfter` becomes a per-tier cursor pair.

- **Explicit `discardEB`.** The pricing model's one expensive undo of a
  prior `addEB` — cascades through both tiers.
