{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.PerasCertDB.API
  ( PerasCertDB (..)
  , PerasWeightSnapshot (..)
  , boostedWeightForPoint
  , boostedWeightForFragment
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

data PerasCertDB m blk = PerasCertDB
  { addCert :: PerasCert blk -> m ()
  , getWeightSnapshot :: STM m (PerasWeightSnapshot blk)
  , garbageCollect :: SlotNo -> m ()
  -- ^ Garbage-collect state older than the given slot number.
  , closeDB :: m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDB" (PerasCertDB m blk)

newtype PerasWeightSnapshot blk = PerasWeightSnapshot
  { getPerasWeightSnapshot :: Map (Point blk) PerasWeight
  }
  deriving stock (Show, Eq)
  deriving Generic
  deriving newtype NoThunks

boostedWeightForPoint ::
  forall blk.
  StandardHash blk =>
  PerasWeightSnapshot blk -> Point blk -> PerasWeight
boostedWeightForPoint (PerasWeightSnapshot weightByPoint) pt =
  Map.findWithDefault mempty pt weightByPoint

boostedWeightForFragment ::
  forall blk.
  HasHeader blk =>
  PerasWeightSnapshot blk ->
  AnchoredFragment blk ->
  PerasWeight
boostedWeightForFragment weightSnap frag =
  -- TODO think about whether this could be done in sublinear complexity
  -- probably should write microbenchmarks at some point to see if this is a bottleneck
  foldMap
    (boostedWeightForPoint weightSnap)
    (blockPoint <$> AF.toOldestFirst frag)
