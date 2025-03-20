### Breaking

- Query `GetProposedPParamsUpdates` has been deprecated and is not
  supported in `ShelleyNodeToClientV12`, which maps to
  `NodeToClientV_20`. This query became redundant once the chain
  transitioned to the Conway era. Moreover, the query constructor is
  kept with a dummy response for previous `NodeToClientV_X` versions,
  meaning that performing such a query in a node that is still not in
  Conway will return dummy values.
