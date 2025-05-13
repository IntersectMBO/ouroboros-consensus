### Breaking

- Use new mlocked KES API for all internal KES sign key handling.
- Add finalizers to all block forgings (required by `ouroboros-consensus`).
- Change `ShelleyLeaderCredentials` to not contain the KES sign key itself
  anymore. Instead, the `CanBeLeader` data structure now contains a
  `praosCanBeLeaderCredentialsSource` field, which specifies how to obtain the
  actual credentials (OpCert and KES SignKey).
