{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Consensus.MiniProtocol.Util.Idling where

import GHC.Generics (Generic)
import Ouroboros.Consensus.Util.IOLike (IOLike, NoThunks)

-- | Interface for the ChainSync client to manipulate the idling flag in
-- 'ChainSyncState', and the PerasCertDiffusion client in 'PerasCertDiffusionInboundState'.
data Idling m = Idling
  { idlingStart :: !(m ())
  -- ^ Mark the peer as being idle.
  , idlingStop :: !(m ())
  -- ^ Mark the peer as not being idle.
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
