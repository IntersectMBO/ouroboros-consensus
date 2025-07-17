{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.PerasCertDB.API
  ( PerasCertDB (..)
  ) where

import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Peras.Weight
import Ouroboros.Consensus.Util.IOLike

data PerasCertDB m blk = PerasCertDB
  { addCert :: PerasCert blk -> m ()
  , getWeightSnapshot :: STM m (PerasWeightSnapshot blk)
  , garbageCollect :: SlotNo -> m ()
  -- ^ Garbage-collect state older than the given slot number.
  , closeDB :: m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDB" (PerasCertDB m blk)
