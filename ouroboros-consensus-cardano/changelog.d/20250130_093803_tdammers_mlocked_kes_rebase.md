### Breaking

- Use new mlocked KES API for all internal KES sign key handling.
- Add finalizers to all block forgings (required by `ouroboros-consensus`).
- Change `HotKey` to manage not only KES sign keys, but also the corresponding
  OpCerts. This is in preparation for KES agent connectivity: with the new
  design, the KES agent will provide both KES sign keys and matching OpCerts
  together, and we need to be able to dynamically replace them both together.
- Add finalizer to `HotKey`. This takes care of securely forgetting any KES
  keys the HotKey may still hold, and will be called automatically when the
  owning block forging terminates.
- Change `ShelleyLeaderCredentials` to not contain the KES sign key itself
  anymore. Instead, the `CanBeLeader` data structure now contains a
  `praosCanBeLeaderCredentialsSource` field, which specifies how to obtain the
  actual credentials (OpCert and KES SignKey).
- The `KesKey` data type in `unstable-cardano-tools` has been renamed to
  `UnsoundPureKesKey`, to reflect the fact that it uses the old, unsound KES
  API (which does not use mlocking or secure forgetting).
