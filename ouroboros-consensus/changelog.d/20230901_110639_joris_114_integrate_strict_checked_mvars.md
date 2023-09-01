<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

### Patch

- Use `strict-checked-vars-0.1.0.4`.

### Non-Breaking

- Add `StrictMVar`s with default `NoThunks` invariants
    `Ouroboros.Consensus.Util.NormalForm.StrictMVar`.

### Breaking

- Replace `StrictSVar`s by `StrictMVar`s where possible.

