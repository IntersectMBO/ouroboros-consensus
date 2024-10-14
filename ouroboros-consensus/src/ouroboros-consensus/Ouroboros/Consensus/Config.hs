{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Config (
    -- * The top-level node configuration
    TopLevelConfig (..)
  , castTopLevelConfig
  , mkTopLevelConfig
    -- ** Checkpoints map
  , CheckpointsMap (..)
  , DiffusionPipeliningSupport (..)
  , castCheckpointsMap
  , emptyCheckpointsMap
    -- ** Derived extraction functions
  , configBlock
  , configCodec
  , configConsensus
  , configLedger
  , configStorage
    -- ** Additional convenience functions
  , configSecurityParam
    -- * Re-exports
  , module Ouroboros.Consensus.Config.SecurityParam
  ) where

import           Data.Coerce
import           Data.Map.Strict (Map)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Top-level config
-------------------------------------------------------------------------------}

-- | The top-level node configuration
data TopLevelConfig blk = TopLevelConfig {
      topLevelConfigProtocol    :: !(ConsensusConfig (BlockProtocol blk))
    , topLevelConfigLedger      :: !(LedgerConfig blk)
    , topLevelConfigBlock       :: !(BlockConfig blk)
    , topLevelConfigCodec       :: !(CodecConfig blk)
    , topLevelConfigStorage     :: !(StorageConfig blk)
    , topLevelConfigCheckpoints :: !(CheckpointsMap blk)
    }
  deriving (Generic)

instance ( ConsensusProtocol (BlockProtocol blk)
         , NoThunks (LedgerConfig  blk)
         , NoThunks (BlockConfig   blk)
         , NoThunks (CodecConfig   blk)
         , NoThunks (StorageConfig blk)
         , NoThunks (HeaderHash    blk)
         ) => NoThunks (TopLevelConfig blk)

-- | Checkpoints are block hashes that are expected to be present in the honest
-- historical chain.
--
-- Each checkpoint is associated with a 'BlockNo', and any block with a
-- 'BlockNo' in the checkpoints map is expected to have the corresponding hash.
--
newtype CheckpointsMap blk = CheckpointsMap {
      unCheckpointsMap :: Map BlockNo (HeaderHash blk)
    }
  deriving (Generic, Monoid, Semigroup)

-- | Configure consensus layer how to handle some cases of invalid data
-- when processing mini protocol communication in the presence of diffusion
-- pipelining. See also 'Ouroboros.Consensus.Block.BlockSupportsDiffusionPipelining'
--
data DiffusionPipeliningSupport = DiffusionPipeliningOn | DiffusionPipeliningOff
  deriving (Show)

instance ( NoThunks (HeaderHash    blk)
         ) => NoThunks (CheckpointsMap blk)

emptyCheckpointsMap :: CheckpointsMap blk
emptyCheckpointsMap = mempty

mkTopLevelConfig ::
     ConsensusConfig (BlockProtocol blk)
  -> LedgerConfig   blk
  -> BlockConfig    blk
  -> CodecConfig    blk
  -> StorageConfig  blk
  -> CheckpointsMap blk
  -> TopLevelConfig blk
mkTopLevelConfig prtclCfg ledgerCfg blockCfg codecCfg storageCfg checkpointsMap =
    TopLevelConfig prtclCfg ledgerCfg blockCfg codecCfg storageCfg checkpointsMap

configConsensus :: TopLevelConfig blk -> ConsensusConfig (BlockProtocol blk)
configConsensus = topLevelConfigProtocol

configLedger :: TopLevelConfig blk -> LedgerConfig blk
configLedger = topLevelConfigLedger

configBlock  :: TopLevelConfig blk -> BlockConfig  blk
configBlock = topLevelConfigBlock

configCodec  :: TopLevelConfig blk -> CodecConfig  blk
configCodec = topLevelConfigCodec

configStorage  :: TopLevelConfig blk -> StorageConfig blk
configStorage = topLevelConfigStorage

configSecurityParam :: ConsensusProtocol (BlockProtocol blk)
                    => TopLevelConfig blk -> SecurityParam
configSecurityParam = protocolSecurityParam . configConsensus

castTopLevelConfig ::
     ( Coercible (ConsensusConfig (BlockProtocol blk))
                 (ConsensusConfig (BlockProtocol blk'))
     , LedgerConfig blk ~ LedgerConfig blk'
     , Coercible (BlockConfig   blk) (BlockConfig   blk')
     , Coercible (CodecConfig   blk) (CodecConfig   blk')
     , Coercible (StorageConfig blk) (StorageConfig blk')
     , Coercible (HeaderHash    blk) (HeaderHash    blk')
     )
  => TopLevelConfig blk -> TopLevelConfig blk'
castTopLevelConfig TopLevelConfig{..} = TopLevelConfig{
      topLevelConfigProtocol    = coerce topLevelConfigProtocol
    , topLevelConfigLedger      = topLevelConfigLedger
    , topLevelConfigBlock       = coerce topLevelConfigBlock
    , topLevelConfigCodec       = coerce topLevelConfigCodec
    , topLevelConfigStorage     = coerce topLevelConfigStorage
    , topLevelConfigCheckpoints = coerce topLevelConfigCheckpoints
    }

castCheckpointsMap ::
     Coercible (HeaderHash blk) (HeaderHash blk')
  => CheckpointsMap blk -> CheckpointsMap blk'
castCheckpointsMap = coerce
