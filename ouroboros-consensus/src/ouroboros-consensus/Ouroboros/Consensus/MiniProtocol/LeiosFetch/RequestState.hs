{-# LANGUAGE NamedFieldPuns #-}

-- | Physical request ownership, independent of durable job completion.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#requests-and-admission@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState
  ( AttemptKey (..)
  , Work (..)
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

data AdmissionError = DuplicateAttempt | SamePeerCopy | PeerLimit
  deriving (Eq, Show)

emptyLedger :: Ledger peer point
emptyLedger = Ledger Map.empty Map.empty

usage :: Ord peer => peer -> Ledger peer point -> Usage
usage peer = Map.findWithDefault (Usage 0 0) peer . totals

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
  | not fits = Left PeerLimit
  | otherwise =
      Right
        st
          { leases = Map.insert key lease (leases st)
          , totals = Map.insert peer added (totals st)
          }
 where
  old = usage peer st
  fits = case work of
    Body{} -> bodyRequests old < maxBodiesPerPeer cfg
    Job{} -> transactionBytes old + fromIntegral bytes <= fromIntegral cap
  cap = if big then maxRequestedBytesSizePerBigLedgerPeer cfg else maxRequestedBytesSizePerPeer cfg
  added = case work of
    Body{} -> old{bodyRequests = bodyRequests old + 1}
    Job{} -> old{transactionBytes = transactionBytes old + fromIntegral bytes}
  sameSource (AttemptKey p _ _, Lease w _) = p == peer && w == work

-- | Release only the original live key, returning whether it owned a lease.
release :: Ord peer => AttemptKey peer -> Ledger peer point -> (Bool, Ledger peer point)
release key@(AttemptKey peer _ _) st = case Map.lookup key (leases st) of
  Nothing -> (False, st)
  Just Lease{leaseWork, leaseBytes} ->
    let old = usage peer st
        new = case leaseWork of
          Body{} -> old{bodyRequests = bodyRequests old - 1}
          Job{} -> old{transactionBytes = transactionBytes old - fromIntegral leaseBytes}
        updated = if new == Usage 0 0 then Map.delete peer (totals st) else Map.insert peer new (totals st)
     in (True, st{leases = Map.delete key (leases st), totals = updated})

-- | Physical cleanup only. Existing outstanding-state removal remains the
-- sole owner of job-pool unpicking, so physical cleanup cannot repeat it.
removePeer :: Ord peer => peer -> Ledger peer point -> Ledger peer point
removePeer peer st =
  st
    { leases = Map.filterWithKey (\(AttemptKey p _ _) _ -> p /= peer) (leases st)
    , totals = Map.delete peer (totals st)
    }
