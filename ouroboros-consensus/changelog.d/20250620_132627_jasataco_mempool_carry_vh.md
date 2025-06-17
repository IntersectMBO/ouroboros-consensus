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
