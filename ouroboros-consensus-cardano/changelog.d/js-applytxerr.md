### Breaking

- The serialization of `ApplyTxErr` for eras in between Shelley and
  Babbage (both included) has changed from using an indefinite-length
  list to using a definite-length list. This is still conformant to
  the CDDL spec (as CDDL does not explicit whether lists are encoded
  with definite or indefinite lengths), but it might require a change
  in client codecs to account for this change.
