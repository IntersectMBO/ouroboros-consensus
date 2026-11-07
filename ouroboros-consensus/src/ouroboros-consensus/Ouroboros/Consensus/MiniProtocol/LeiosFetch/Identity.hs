-- | Identities shared by the fetch kernel and the atomic request ledger.
--
-- Rationale (repository-root path):
-- @docs/fetch-policy/cancellation-contract.md#3-request-identity@.
-- That section maps these identities to the cancellation contract.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
  ( LeiosPeerLifetimeId (..)
  , FetchSessionToken (..)
  , AcquisitionGeneration (..)
  , WireOrdinal (..)
  , RequestKey (..)
  ) where

import Data.Word (Word64)
import Numeric.Natural (Natural)

-- | One peer registration. Natural counters cannot wrap and alias a callback
-- from a previous registration, even when the peer address is reused. A
-- storage callback may outlive a mini-protocol or a connection; a later
-- registration of that same peer must not receive its update. The kernel
-- re-exports this type for existing clients.
data LeiosPeerLifetimeId peer = MkLeiosPeerLifetimeId !peer !Natural
  deriving (Eq, Ord, Show)

-- | One invocation of the fetch mini-protocol within a peer registration.
newtype FetchSessionToken = FetchSessionToken Natural
  deriving (Eq, Ord, Show)

-- | Local identity of one acquisition of an EB point. Reacquiring the point
-- after retirement gets a fresh generation. Atomic descriptors and storage
-- claims capture it so their old settlement cannot acquire the replacement.
-- Legacy requests capture no generation; their bridge pins it later at write
-- submission. This identity is local and is not sent on the wire.
newtype AcquisitionGeneration = AcquisitionGeneration Natural
  deriving (Eq, Ord, Show)

-- | Consecutive sequence number of a reply-bearing client request within one
-- LeiosFetch session, starting at zero. Body, TX-batch and cancellation requests
-- share this sequence; replies are matched by FIFO position.
--
-- This wrapper distinguishes sequence numbers from other identifiers and bounds
-- their wire representation. Its constructor does not validate session state.
-- The enforced protocol invariants are:
--
-- * Each request uses the exact next number, without reuse or wraparound.
-- * A cancel targets an earlier data request within the bounded recent window,
--   at most once per target. It cannot target another cancel.
-- * Recent target history stays bounded independently of the numeric values.
--
-- "Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission" allocates these
-- numbers in @admitData@ and @admitCancel@, preserving numbers for future cancels.
-- "Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Inbound" enforces incoming
-- sequences and targets in @checkIncomingMessage@. Separate request, byte and
-- preparation limits bound resource use by otherwise valid request/cancel traffic.
newtype WireOrdinal = WireOrdinal Word64
  deriving (Eq, Ord, Show)

-- | Peer/session-qualified request identity captured by atomic callbacks. An
-- ordinal can recur in a later session; the full key distinguishes old callbacks
-- from requests in a replacement registration or mini-protocol invocation.
data RequestKey peer = RequestKey !(LeiosPeerLifetimeId peer) !FetchSessionToken !WireOrdinal
  deriving (Eq, Ord, Show)
