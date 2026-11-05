-- | Identities shared by the fetch kernel and the atomic request ledger.
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

newtype AcquisitionGeneration = AcquisitionGeneration Natural
  deriving (Eq, Ord, Show)

newtype WireOrdinal = WireOrdinal Word64
  deriving (Eq, Ord, Show)

-- | Every callback captures the full identity, never just the wire ordinal.
data RequestKey peer = RequestKey !(LeiosPeerLifetimeId peer) !FetchSessionToken !WireOrdinal
  deriving (Eq, Ord, Show)
