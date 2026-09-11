{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Consensus.MiniProtocol.Util.Idling (Idling (..), noIdling) where

import GHC.Generics (Generic)
import Ouroboros.Consensus.Util.IOLike (IOLike, NoThunks)

-- | Interface for a mini-protocol client to record whether it is caught up with
-- a peer.
--
-- "Idling" follows the existing ChainSync terminology: it starts when the peer
-- sends @MsgAwaitReply@ and stops when the peer supplies new data. Object
-- Diffusion uses the same terminology and semantics for its object-ID stream.
-- It does not mean that the typed protocol is in a state named @StIdle@.
--
-- The actions update the idling flag in @ChainSyncState@ or in the
-- corresponding Object Diffusion inbound state, respectively.
data Idling m = Idling
  { idlingStart :: !(m ())
  -- ^ Record that the client has reached the peer's current front.
  , idlingStop :: !(m ())
  -- ^ Record that the peer has supplied new data.
  }
  deriving stock Generic

deriving anyclass instance IOLike m => NoThunks (Idling m)

-- | No-op implementation, for tests.
noIdling :: Applicative m => Idling m
noIdling =
  Idling
    { idlingStart = pure ()
    , idlingStop = pure ()
    }
