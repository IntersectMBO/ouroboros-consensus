<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Patch

- The mempool will now carry its own forker instead of acquiring one on each
  revalidation. This particularly implies that the mempool will no longer
  re-sync under the hood while trying to add a transaction, and only the
  background thread will perform such a re-sync.

- The ledger state that triggers a re-sync in the background thread will be the
  one used to revalidate the mempool. Previously, another read for the new tip
  would be performed when starting the sync, which could lead to confusing
  (although innocuous) situations in which:

   - the tip changes from A to B
   - the background thread sees the tip changed from A, so it records the new tip (B) and triggers the re-sync
   - the tip changes again from B to C before the syncing process reads the tip again
   - the mempool is re-synced with C
   - the background thread would now see that the tip changed from B, so it records the new tip (C) and triggers the re-sync
   - the mempool is re-synced **AGAIN** with C

  This sequence of actions can't happen again.

- The mempool now has its own registry in which it allocates forkers. The
  background thread was moved to this inner registry such that it can access the
  mempool internal registry, but an action to cancel it will still live in the
  outer registry, to ensure the thread is closed before we attempt to close the
  mempool internal registry. Otherwise we would run into a race condition if the
  background thread would attempt a resync while the internal registry was being
  closed.

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->

### Breaking

- Removed `getLedgerTablesAtFor` from the ChainDB API. Clients now have to
  actually open a forker and manage it.
