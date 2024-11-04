The options for how we see to proceed.

1) retain the status quo

2) upstream block counting into the Ledger governance actions related to eras

3) remove block counting, but keep double stability (ie permits the HFC interpreter to say Just Nothing Just)

    3a) do not add HLQ mitigations
    3b) add HLQ mitigations
    3c) more exotic/subtle: change block counting to be a disjunct instead of a conjunct prevents Just -> Nothing (since that only arises when switching to a denser chain)

4) remove block counting, remove double stability, and add HLQ mitigations

Option 4 is elaborated below.

-----

A proposal for a simplified HFC, eg neither Counting Blocks nor requiring Double Stability.

- Suppose a majority of stake is held by healthy caught-up honest nodes that are well-connected.
  Thus those honest nodes exhibit Praos Common Prefix (CP) and Praos Chain Growth (CG).

- In particular, CP ensures that the k+1st youngest block on the selection of any of those nodes is also already on the selection of all the other nodes and will remain there forever.
  The block might not yet be the k+1st youngest on the other nodes' selections.
  But none of these nodes will never need to switch away from its k+1st youngest block.

- Accordingly, the ChainSync mini protocol client should disconnect from any upstream peer whose intersection with the node's selection's tip is more than k+1 blocks back.

- Assume the ledger rules prevent any block from altering the leader schedule before a delay of one CG stability window and that, accordingly, ChainSync validates upstream peers' headers by forecasting the leadership schedule from the intersection of the peer's chain and the node's selection.

    - In particular, this implies that the Ledger's governance for transitioning to the next era can also be forecasted by at least a stability window, since era transitions may affect the leader schedule.

    - According to CG, this ensures the nodes will be able to validate at least k+1 blocks of an honest upstream peer's chain, which would suffice to incur the deepest intersection possibly allowed by CP.
      Even so, the above rule is a reasonable behavior when the peer's chain violates CG: the number of validatable headers after the intersection is proportional to the severity of the CG violation.

- According to Ouroboros Chronos, the ChainSync mini protocol client should disconnect from the upstream peer if the header is from the far future and otherwise briefly pause until it is no longer from the future.

    - (This is the H of HLQ.)

    - The ChainSync client should annotate the validated header with the wall clock translation (eg UTC time) of the header's slot's onset (translated accorded to its own chain).
      This information can be used by performance monitors, etc.

    - Node initialization from the on-disk block database is the only other means by which blocks arrive at the node, so the re-application/re-validation logic used there should also yield wall clock annotations on the node's selected header chain.

- The node should use its current selection's tip in order to determine whether it is time to mint a block.
  If the wall clock reaches the onset of a slot before the node determines it leads that slot, the node should not mint a block in that slot.
  Moreover, after minting a block, the node should not mint again until the wall clock reaches the end of the slot of the block it most recently minted.

    - (This is the L of HLQ.)

    - According to CG and the Praos epoch structure (eg 3:4:4), a node will never switch chains such that the leader schedule changes.
      Similarly, a chain switch will never change the era of the current epoch.

    - Even so, the above rules suffice to prevent the honest node from equivocating elections when CG is violated.

    - It would be some what simpler to instead only mint according to the leadership schedule of the k+1 youngest block.
      However, when CG is violated, this rule alone does not prevent an honest node from equivocating its elections.
      The rules above are simple enough and also clearly prevent such equivocation.

- The interface exposed to clients by the node should make it extremely explicit when a subsequent chain switch might change the node's answer to a query (eg which upcoming slots the node leads).

    - (This is the Q of HLQ.)

    - In the extreme, those queries could be restricted to forecasts from the node's k+1 youngest block, thereby ensuring no chain switch could alter the answer except monotonically improving it from "unknown" to "known".
      In less extreme cases, the documentation could say "you might see non-monotonic answers if using an acquired ledger state that does not have more than k blocks past the voting deadline".
      A middle ground might be annotating the answer with the corresponding probability from IOG's latest official publication of the table of settlement times.

- In the Praos and Genesis nodes, there is no use of wall clock times beyond the header future check, slot leadership check, and queries discussed above.
  However, Peras's votes and Leios's non-ranking blocks are subject to a leader schedule but their designs do not yet necessarily bind themselves to the particular chain that determines that election's parameters (ie the relevant leader schedule, including the onsets of its slots) (TODO maybe IBs already do).
  Therefore, it is not obvious how equivocation detection could necessarily avoid false alarms, at least not in the presence of CG violations.
