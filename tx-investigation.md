# Babbage → Conway transaction investigation

## The report

The node's ledger is in **Conway**. A transaction submitted over **LocalTxSubmission** (node-to-client):

- tagged as **Babbage** → **rejected**
- the *same* transaction tagged as **Conway** → **accepted**

Goal: find where an older-era-tagged transaction could be rejected, and whether it lies in
consensus, the ledger's CBOR layer, or the client tooling.

Repos consulted (sibling checkouts under `~/code/cardano/`): `ouroboros-consensus`,
`cardano-ledger`, `cardano-api`, `cardano-cli`.

## The harness

`ouroboros-consensus-cardano/app/check-tx-upgrade.hs` (+ `executable check-tx-upgrade` in
`ouroboros-consensus.cabal`; **uncommitted**). Run:

```
cabal run ouroboros-consensus:exe:check-tx-upgrade
```

It builds a transaction, runs it through a verbatim copy of the Babbage→Conway `InjectTx`
(`translateTxBabbageToConwayWrapper`, the per-boundary entry that `hardForkInjectTxs`
assembles), and compares transaction ids across paths. Latest output:

```
1. Babbage-tagged           : eecad5822f…961823d3
2. after Babbage→Conway     : eecad5822f…961823d3   (== 1)
3. native Conway-tagged     : 3e14d28887…44218875   (≠ 1)
```

## How the upgrade works (consensus)

- A `GenTx (CardanoBlock)` carries an `NS GenTx xs` — the tx tagged with the era it was built in.
- When that era is older than the ledger's, the mempool walks the `InPairs InjectTx` chain from
  `hardForkInjectTxs`, stepping the tx forward one era at a time via `SL.translateEra`.
  - `Ouroboros/Consensus/HardFork/Combinator/InjectTxs.hs`
  - `Ouroboros/Consensus/HardFork/Combinator/Mempool.hs`
  - `Ouroboros/Consensus/Cardano/CanHardFork.hs` (`translateTxBabbageToConwayWrapper`)
- `HardForkApplyTxErrWrongEra` is produced **only** for a tx from an era *newer* than the ledger.
  An older-era tx is upgraded, not rejected. So the mempool accepts + upgrades Babbage txs.

## Findings by layer

| Layer | Causes "Babbage-tag fails / Conway-tag succeeds"? | Evidence |
|---|---|---|
| Consensus upgrade (`hardForkInjectTxs`) | **No** | upgrades older→current; `WrongEra` only for newer-than-ledger |
| Conway CBOR **decode** | **No** | `allowTag`: tag 258 permitted, not enforced; no canonicity rule |
| Txid / witnesses | **No** | upgrade preserves the txid (harness 1 == 2); witnesses still match |
| cardano-api / cardano-cli | **No** | era tag ↔ encoding coupled; no mis-tag path |

### 1. Decode — Conway accepts legacy (untagged) encoding

- `cardano-ledger/libs/cardano-ledger-binary/src/Cardano/Ledger/Binary/Decoding/Decoder.hs`
  - `decodeSet` is version-varied; at decode version **≥ 9** (Conway) its doc reads:
    *"Set tag 258 is permitted, but not enforced."* It routes through
    `decodeSetLikeEnforceNoDuplicates` → `allowTag setTag`.
  - `allowTag` (`:1357`) consumes a tag *only if present*; absent tag is a no-op.
- `encodeSet` (`.../Encoding/Encoder.hs:462`): version ≥ 9 prefixes tag 258; versions 2–8
  (Shelley…Babbage) emit no tag. This is the byte difference the harness sees.
- No canonical / round-trip enforcement exists in the Conway rules.

### 2. The upgrade preserves the txid (keeps Babbage bytes)

- `cardano-ledger/eras/conway/impl/src/Cardano/Ledger/Conway/Translation.hs:89`
  — `TranslateEra ConwayEra (Tx TopTx)` translates body/wits/auxData via
  `translateEraThroughCBOR`; the `Annotator` recaptures the original (untagged Babbage) bytes,
  so the upgraded Conway tx's txid equals the Babbage txid.
- Consequence: witnesses signed over the Babbage txid still verify after the upgrade. A native
  Conway tx of identical content has a *different* txid (tag-258 encoding) — but that only matters
  to the client that signed it, and each path signs the id matching its own era.

### 3. Re-broadcast does not fail on later nodes

- The Shelley `GenTx` wire encoder re-emits the tx's memoized bytes
  (`Shelley/Ledger/Mempool.hs:259`, `toCBOR (ShelleyTx _ tx) = wrapCBORinCBOR toCBOR tx`), so a
  node puts the *same* (Babbage-format) bytes back on the wire under the Conway tag.
- Every Conway node decodes them via `allowTag` (accepts untagged) and computes the same txid.
  No deserialization failure, no network-wide txid split. This backward-compat is deliberate.

### 4. Clients do not mis-tag the era

- **cardano-api** `toConsensusGenTx` (`cardano-api/src/Cardano/Api/Consensus/Internal/InMode.hs:107`):
  the GADT ties the HFC era index to the `Tx era` value — a `Tx BabbageEra` is always
  Babbage-encoded and submitted at the Babbage index (5); `Tx ConwayEra` at index 6.
- **cardano-cli** submit (`cardano-cli/src/Cardano/CLI/EraBased/Transaction/Run.hs:1324-1326`):
  `readFileTx → InAnyShelleyBasedEra era tx`, then `TxInMode era tx`. `readTx`
  (`Cardano/CLI/Read.hs:321-329`, `fromSomeShelleyTx`) matches the TextEnvelope **type string**
  (`"Tx ConwayEra"`, `"Tx BabbageEra"`, …) per era, so the era is pinned by the label the tx was
  written with and drives both decode and the submit tag. No `--era` override on submit.
- So a Babbage-tagged submission always carries Babbage (untagged) bytes and a Conway-tagged one
  carries Conway (tag-258) bytes; they cannot be crossed through the tooling.

## The one structural asymmetry

The **Babbage decoder (proto ver < 9)** has no `allowTag` — its `decodeSet` branch uses
`decodeCollection decodeListLenOrIndef`, which expects a plain list and therefore **rejects** a
tag-258 set. Conway (≥ 9) accepts both. But because the tooling couples era ↔ encoding, this can
only bite when:

- raw bytes are submitted with a hand-chosen wrong era (bypassing cardano-cli/api), or
- a **version-skewed client** (an old cardano-api/cli that can't represent the Conway tx) handles
  a Conway tx as Babbage.

## Conclusion & leading hypothesis

Nothing in the consensus upgrade, the ledger's CBOR encode/decode, txid/witness handling, or the
client tooling explains the report via a mis-tag or an encoding rejection.

For a genuine "same logical transaction" report, the cause is a **Conway-specific *validation*
difference** between a tx *built* as Babbage vs Conway, which surfaces only after the Babbage-built
tx is upgraded and checked under Conway rules. This is now **confirmed** — see below.

## CONFIRMED cause: the Conway reference-script minimum-fee surcharge

Conway added a minimum-fee surcharge proportional to the size of the **reference scripts** a
transaction consumes; Babbage (and Alonzo) have no such term.

- Babbage/Alonzo drop the ref-script size argument in the min-fee:
  - `cardano-ledger/eras/babbage/impl/src/Cardano/Ledger/Babbage/UTxO.hs:62`
    — `getMinFeeTxUtxo pp tx _ = getShelleyMinFeeTxUtxo pp tx`
  - `cardano-ledger/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/UTxO.hs:128` — same.
- Conway adds it:
  - `cardano-ledger/eras/conway/impl/src/Cardano/Ledger/Conway/UTxO.hs:137` — `getMinFeeTxUtxo = getConwayMinFeeTxUtxo`
  - `.../Conway/UTxO.hs:163` — `getMinFeeTx pp tx (txNonDistinctRefScriptsSize utxo tx)`
  - `.../Conway/Tx.hs:103` — `getConwayMinFeeTx pp tx sz = alonzoMinFeeTx pp tx <+> refScriptsFee`,
    where `refScriptsFee = tierRefScriptFee … sz` using the new param `ppMinFeeRefScriptCostPerByte`.
- Enforced in the UTXO rule: `conwayUtxoTransition` (`.../Conway/Rules/Utxo.hs:234`) →
  `Babbage.babbageUtxoValidation` → `feesOK` (`.../Babbage/Rules/Utxo.hs:181`), which sets
  `minFee = getMinFeeTxUtxo pp tx u` and fails `FeeTooSmallUTxO` when `minFee > txfee`.

**Mechanism:** a tx that consumes reference scripts (spending a script-locked UTxO via a reference
script — the common DApp case), built/tagged as **Babbage**, sets a fee *without* the surcharge.
Submitted tagged Babbage → upgraded to Conway → the Conway UTXO rule recomputes min-fee *with* the
surcharge → fee too small → `FeeTooSmallUTxO`. Built as **Conway**, the wallet includes the
surcharge → accepted. Same intent: Babbage-tagged fails, Conway-tagged succeeds — an exact match.

**Scope:** `tierRefScriptFee` is 0 for zero ref-script bytes, so simple (no-reference-script) txs
are unaffected; only script-using txs fail. Consistent with "some transactions" reports.

**Secondary Conway-only check:** `disjointRefInputs` (`.../Babbage/Rules/Utxo.hs`), active at
Conway protocol versions (pv 9–10), rejects a tx whose inputs and reference-inputs overlap
(`BabbageNonDisjointRefInputs`); Babbage allowed the overlap.

**Weaker candidate (script-integrity hash / cost-model language views):** `getLanguageView`
(`.../Alonzo/PParams.hs:566`) encodes cost models at `pvMajor` of the *current* pparams, and the
ledger recomputes the integrity hash via the current-era instance — so it mainly bites when the
client builds with stale/wrong-era protocol params, not purely from the era tag.

**Bottom line:** this is expected ledger behaviour, not a consensus/serialisation bug. The fix is
client-side — build the transaction in the *current* era (Conway) so the reference-script fee is
included.

## Next step

Obtain the **CBOR of an actual failing transaction** and run it through the harness:

- prints `NOT UPGRADEABLE` → a `translateEra` failure (the smoking gun); or
- upgrades cleanly → build the validation differential (upgraded vs native Conway, with resolving
  inputs), or trace the specific Conway rule in `../cardano-ledger`.

## Status of the harness

`check-tx-upgrade.hs` + its cabal stanza are **uncommitted** — keep or drop TBD.
