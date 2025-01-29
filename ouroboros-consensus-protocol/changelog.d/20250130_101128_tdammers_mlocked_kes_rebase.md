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
- Change `PraosCanBeLeader` to not contain the KES sign key itself anymore.
  Instead, it now contains a `PraosCredentialsSource` field, which
  specifies how to obtain the actual credentials (OpCert and KES SignKey). For
  now, the only supported method is passing an OpCert and an
  UnsoundPureSignKeyKES, presumably loaded from disk
  (`PraosCredentialsUnsound`); future iterations will add support for
  connecting to a KES agent.
