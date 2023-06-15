<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

<!--
### Patch

- A bullet item for the Patch category.

-->
### Non-Breaking

- Add a new `Control.Concurrent.Class.MonadMVar.Strict.NoThunks` module, which
  provides `StrictMVar`s (from the `strict-mvar` package) with `NoThunks`
  invariants checks. These checks can be enabled using a package flag
  `+checkmvarinvariants`.

### Breaking

- Rename the `StrictMVar` type to `StrictSVar`. Rename related definitions and
  variables to mention `SVar` instead of `MVar`. Rename the `StrictMVar` module
  to `StrictSVar`.
