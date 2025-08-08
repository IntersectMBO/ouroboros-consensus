{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.PerasCertDB.API
  ( PerasCertDB (..)
  , AddPerasCertResult (..)
  ) where

import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Peras.Weight
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (WithFingerprint (..))

data PerasCertDB m blk = PerasCertDB
  { addCert :: PerasCert blk -> m AddPerasCertResult
  , getWeightSnapshot :: STM m (WithFingerprint (PerasWeightSnapshot blk))
  -- ^ Return the Peras weights in order compare the current selection against
  -- potential candidate chains, namely the weights for blocks not older than
  -- the current immutable tip. It might contain weights for even older blocks
  -- if they have not yet been garbage-collected.
  --
  -- The 'Fingerprint' is updated every time a new certificate is added, but it
  -- stays the same when certificates are garbage-collected.
  , garbageCollect :: SlotNo -> m ()
  -- ^ Garbage-collect state older than the given slot number.
  , closeDB :: m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDB" (PerasCertDB m blk)

data AddPerasCertResult = AddedPerasCertToDB | PerasCertAlreadyInDB
  deriving stock (Show, Eq)
