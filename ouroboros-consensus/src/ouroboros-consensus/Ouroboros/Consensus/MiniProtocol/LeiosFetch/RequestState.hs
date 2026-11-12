{-# LANGUAGE NamedFieldPuns #-}

-- | Physical request ownership, independent of durable job completion.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#requests-and-admission@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState
  ( CommandKey (..)
  , AttemptKey (..)
  , Work (..)
  , workPoint
  , Lease (..)
  , Usage (..)
  , Ledger
  , AdmissionError (..)
  , emptyLedger
  , leases
  , usage
  , copies
  , reserve
  , release
  , removePeer
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config

-- | Local correlation only: original peer registration, client invocation,
-- then a checked nonwrapping request ordinal. None of these go on the wire.
data AttemptKey peer = AttemptKey !peer !Natural !Word64
  deriving (Eq, Ord, Show)

-- | Logical work whose copies retain distinct physical reservations.
data Work point = Body !point | Job !point !Int
  deriving (Eq, Ord, Show)

-- | EB identified by either a body request or one of its TX jobs.
workPoint :: Work point -> point
workPoint (Body point) = point
workPoint (Job point _) = point

-- | Compact immutable reservation metadata; never response bytes.
data Lease point = Lease
  { leaseWork :: !(Work point)
  , leaseBytes :: !Word32
  }
  deriving (Eq, Show)

-- | Cached peer totals; byte arithmetic is widened before admission.
data Usage = Usage
  { transactionBytes :: !Word64
  , bodyRequests :: !Word32
  , requestCount :: !Word32
  -- ^ All queued/sent bodies and TX jobs, including zero-byte jobs. The fixed
  -- command limit bounds increments; only exact live-lease removal decrements.
  }
  deriving (Eq, Show)

-- | Live leases are the authority for exactly-once settlement, including
-- teardown followed by an old response callback. An absent peer total cannot
-- serve as that guard, especially in a body-only session.
data Ledger peer point = Ledger
  { leases :: !(Map (AttemptKey peer) (Lease point))
  , totals :: !(Map peer Usage)
  }
  deriving (Eq, Show)

-- | Authoritative physical-reservation refusal. Temporary TX pressure differs
-- from a job that exceeds the peer's entire budget, even with no other leases.
data AdmissionError
  = DuplicateAttempt
  | SamePeerCopy
  | RequestCountLimit
  | BodyCountLimit
  | TransactionCreditLimit
  | RequestExceedsCredit
  deriving (Eq, Show)

emptyLedger :: Ledger peer point
emptyLedger = Ledger Map.empty Map.empty

usage :: Ord peer => peer -> Ledger peer point -> Usage
usage peer = Map.findWithDefault (Usage 0 0 0) peer . totals

copies :: Eq point => Work point -> Ledger peer point -> Int
copies work = Map.size . Map.filter ((== work) . leaseWork) . leases

-- | Reserve a complete job or one body. Runtime admission establishes
-- progressive hedge eligibility. This ledger enforces per-peer budgets and
-- same-peer/work exclusion until physical settlement. No byte overshoot is permitted.
reserve ::
  (Ord peer, Eq point) =>
  LeiosFetchStaticEnv ->
  Bool ->
  AttemptKey peer ->
  Lease point ->
  Ledger peer point ->
  Either AdmissionError (Ledger peer point)
reserve cfg big key@(AttemptKey peer _ _) lease@(Lease work bytes) st
  | Map.member key (leases st) = Left DuplicateAttempt
  | any sameSource (Map.toList (leases st)) = Left SamePeerCopy
  | Job{} <- work, fromIntegral bytes > cap = Left RequestExceedsCredit
  | requestCount old >= fromIntegral maxOutstandingRequests = Left RequestCountLimit
  | Body{} <- work, bodyRequests old >= maxBodiesPerPeer cfg = Left BodyCountLimit
  | Job{} <- work, transactionBytes old + fromIntegral bytes > cap = Left TransactionCreditLimit
  | otherwise =
      Right
        st
          { leases = Map.insert key lease (leases st)
          , totals = Map.insert peer added (totals st)
          }
 where
  old = usage peer st
  cap = fromIntegral $
    if big then maxRequestedBytesSizePerBigLedgerPeer cfg else maxRequestedBytesSizePerPeer cfg
  counted = old{requestCount = requestCount old + 1}
  added = case work of
    Body{} -> counted{bodyRequests = bodyRequests old + 1}
    Job{} -> counted{transactionBytes = transactionBytes old + fromIntegral bytes}
  sameSource (AttemptKey p _ _, Lease w _) = p == peer && w == work

-- | Release only the original live key, returning whether it owned a lease.
release :: Ord peer => AttemptKey peer -> Ledger peer point -> (Bool, Ledger peer point)
release key@(AttemptKey peer _ _) st = case Map.lookup key (leases st) of
  Nothing -> (False, st)
  Just Lease{leaseWork, leaseBytes} ->
    let old = usage peer st
        counted = old{requestCount = requestCount old - 1}
        new = case leaseWork of
          Body{} -> counted{bodyRequests = bodyRequests old - 1}
          Job{} -> counted{transactionBytes = transactionBytes old - fromIntegral leaseBytes}
        updated =
          if new == Usage 0 0 0
            then Map.delete peer (totals st)
            else Map.insert peer new (totals st)
     in (True, st{leases = Map.delete key (leases st), totals = updated})

-- | Physical cleanup only. Existing outstanding-state removal remains the
-- sole owner of job-pool unpicking, so physical cleanup cannot repeat it.
removePeer :: Ord peer => peer -> Ledger peer point -> Ledger peer point
removePeer peer st =
  st
    { leases = Map.filterWithKey (\(AttemptKey p _ _) _ -> p /= peer) (leases st)
    , totals = Map.delete peer (totals st)
    }

-- | Invocation and ordinal in a peer-owned queue; its owner supplies the peer.
data CommandKey = CommandKey !Natural !Word64 deriving (Eq, Ord, Show)
