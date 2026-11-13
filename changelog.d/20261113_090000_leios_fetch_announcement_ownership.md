- Own accepted Leios announcement bookkeeping and forged-payload submissions
  independently of the announcing/forging thread. Bound metadata and local
  producer queues separately, coalesce matching forge submissions through
  confirmed settlement, and drain or cancel their workers before storage closes.
