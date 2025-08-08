{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Config.SecurityParam
  ( SecurityParam (..)
  , maxRollbackWeight
  ) where

import Cardano.Binary
import Cardano.Ledger.BaseTypes.NonZero
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.SupportsPeras (PerasWeight (..))
import Quiet

-- | Protocol security parameter
--
-- In longest-chain protocols, we interpret this as the number of rollbacks we
-- support.
--
-- i.e., k == 1: we can roll back at most one block
--       k == 2: we can roll back at most two blocks, etc
--
-- NOTE: This talks about the number of /blocks/ we can roll back, not
-- the number of /slots/.
--
-- In weightiest-chain protocols (Ouroboros Peras), we interpret this as the
-- maximum amount of weight we can roll back.
--
-- i.e. k == 30: we can roll back at most 30 unweighted blocks, or two blocks
-- each having additional weight 14.
newtype SecurityParam = SecurityParam {maxRollbacks :: NonZero Word64}
  deriving (Eq, Generic, NoThunks, ToCBOR, FromCBOR)
  deriving Show via Quiet SecurityParam

-- | The maximum amount of weight we can roll back.
maxRollbackWeight :: SecurityParam -> PerasWeight
maxRollbackWeight = PerasWeight . unNonZero . maxRollbacks
