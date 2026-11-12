{-# LANGUAGE NamedFieldPuns #-}

-- | Physical request ownership, independent of durable job completion.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#requests-and-admission@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState
  ( AttemptKey (..)
  , Work (..)
  , workPoint
  , Lease (..)
  , Usage (..)
  , Ledger
  , AdmissionError (..)
  , emptyLedger
  , leases
  , owners
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
--
-- The owner index is the exact inverse of the live leases: each work/peer
-- entry holds that peer's original attempt key. Queued requests count; settled
-- descriptors without leases do not. Same-peer/work exclusion spans invocations,
-- and empty work buckets are removed. Mutations update all three maps together.
data Ledger peer point = Ledger
  { leases :: !(Map (AttemptKey peer) (Lease point))
  , totals :: !(Map peer Usage)
  , owners :: !(Map (Work point) (Map peer (AttemptKey peer)))
  -- ^ Live owners grouped by work, then original peer identity. This immutable
  -- view includes no response payload or cached peer-blockage classification.
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
emptyLedger = Ledger Map.empty Map.empty Map.empty

usage :: Ord peer => peer -> Ledger peer point -> Usage
usage peer = Map.findWithDefault (Usage 0 0 0) peer . totals

copies :: Ord point => Work point -> Ledger peer point -> Int
copies work = maybe 0 Map.size . Map.lookup work . owners

-- | Reserve a complete job or one body. Runtime admission establishes
-- progressive hedge eligibility. This ledger enforces per-peer budgets and
-- same-peer/work exclusion until physical settlement. No byte overshoot is permitted.
reserve ::
  (Ord peer, Ord point) =>
  LeiosFetchStaticEnv ->
  Bool ->
  AttemptKey peer ->
  Lease point ->
  Ledger peer point ->
  Either AdmissionError (Ledger peer point)
reserve cfg big key@(AttemptKey peer _ _) lease@(Lease work bytes) st
  | Map.member key (leases st) = Left DuplicateAttempt
  | maybe False (Map.member peer) (Map.lookup work (owners st)) = Left SamePeerCopy
  | Job{} <- work, fromIntegral bytes > cap = Left RequestExceedsCredit
  | requestCount old >= fromIntegral maxOutstandingRequests = Left RequestCountLimit
  | Body{} <- work, bodyRequests old >= maxBodiesPerPeer cfg = Left BodyCountLimit
  | Job{} <- work, transactionBytes old + fromIntegral bytes > cap = Left TransactionCreditLimit
  | otherwise =
      Right
        st
          { leases = Map.insert key lease (leases st)
          , totals = Map.insert peer added (totals st)
          , owners = Map.insertWith Map.union work (Map.singleton peer key) (owners st)
          }
 where
  old = usage peer st
  cap = fromIntegral $
    if big then maxRequestedBytesSizePerBigLedgerPeer cfg else maxRequestedBytesSizePerPeer cfg
  counted = old{requestCount = requestCount old + 1}
  added = case work of
    Body{} -> counted{bodyRequests = bodyRequests old + 1}
    Job{} -> counted{transactionBytes = transactionBytes old + fromIntegral bytes}

-- | Release only the original live key, returning whether it owned a lease.
-- Prove that lease still exists before removing its owner entry: a late release
-- cannot erase a replacement invocation's ownership of the same work.
release :: (Ord peer, Ord point) => AttemptKey peer -> Ledger peer point -> (Bool, Ledger peer point)
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
     in ( True
        , st
            { leases = Map.delete key (leases st)
            , totals = updated
            , owners = removeOwner peer leaseWork (owners st)
            }
        )

-- | Physical cleanup only. Existing outstanding-state removal remains the
-- sole owner of job-pool unpicking, so physical cleanup cannot repeat it.
removePeer :: (Ord peer, Ord point) => peer -> Ledger peer point -> Ledger peer point
removePeer peer st =
  let (removed, retained) = Map.partitionWithKey (\(AttemptKey owner _ _) _ -> owner == peer) (leases st)
   in st
        { leases = retained
        , totals = Map.delete peer (totals st)
        , owners = Map.foldl' (\index lease -> removeOwner peer (leaseWork lease) index) (owners st) removed
        }

-- Both callers have identified the actual live leases they remove. Delete the
-- work bucket with its last owner, including body-only and zero-byte leases.
removeOwner ::
  (Ord peer, Ord point) =>
  peer ->
  Work point ->
  Map (Work point) (Map peer (AttemptKey peer)) ->
  Map (Work point) (Map peer (AttemptKey peer))
removeOwner peer = Map.update $ \bucket ->
  let remaining = Map.delete peer bucket
   in if Map.null remaining then Nothing else Just remaining
