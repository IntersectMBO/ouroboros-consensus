# Strict Mutable Variables with invariant checking

The `strict-mvar-checked` package provides a strict interface to mutable
variables (`MVar`) with invariant checking. It builds on top of `strict-mvar`
and `io-classes`, and thus it provides the interface for `MVar`s implementations
from both
[base](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Concurrent-MVar.html)
and [io-sim](https://github.com/input-output-hk/io-sim).

## Checked and unchecked `StrictMVar`s

There are currently two variant implementations of `StrictMVar`s.
* From `strict-mvar`: `Control.Concurrent.Class.MonadMVar.Strict`
* From `strict-mvar-checked`: `Control.Concurrent.Class.MonadMVar.Strict.Checked`

The _unchecked_ module provides the simplest implementation of a `StrictMVar`: a
light wrapper around lazy MVars that forces values to WHNF before they are put
into the MVar. The _checked_ module does the exact same thing, but it has the
additional feature that the user can provide an invariant that is checked each
time a new value is placed inside the MVar. The checked module is a drop-in
replacement for the unchecked module, though invariants will be trivially true
in that case. Non-trivial invariants can be set when create a new (empt) `MVar`.

```haskell
newMVarWithInvariant :: MonadMVar m
                     => (a -> Maybe String)
                     -> a
                     -> m (StrictMVar m a)

newEmptyMVarWithInvariant :: MonadMVar m
                          => (a -> Maybe String)
                          -> m (StrictMVar m a)
```

**Note:** though the checked module is a drop-in replacement for the unchecked
module, the `StrictMVar` type from `*.Strict` and the `StrictMVar` type from
`*.Strict.Checked` do not share the same internal representation, and so they
are distinct types. This means we can't make mixed use of the checked and
unchecked modules.

## Guarantees for invariant checking

Although all functions that modify a checked `StrictMVar` will check the
invariant, we do *not* guarantee that the value inside the `StrictMVar` always
satisfies the invariant. Instead, we *do* guarantee that if the `StrictMVar` is
updated with a value that does not satisfy the invariant, an exception is thrown
*after* the new value is written to the `StrictMVar`. The reason for this weaker
guarantee is that leaving an `MVar` empty can lead to very hard to debug
"blocked indefinitely" problems.