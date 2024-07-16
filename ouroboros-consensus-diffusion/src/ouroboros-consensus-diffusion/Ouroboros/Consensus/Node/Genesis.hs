{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.Genesis (
    -- * 'GenesisConfig'
    GenesisConfig (..)
  , GenesisConfigFlags (..)
  , LoEAndGDDConfig (..)
  , defaultGenesisConfigFlags
  , disableGenesisConfig
  , enableGenesisConfigDefault
  , mkGenesisConfig
    -- * NodeKernel helpers
  , GenesisNodeKernelArgs (..)
  , mkGenesisNodeKernelArgs
  , setGetLoEFragment
  ) where

import           Control.Monad (join)
import           Data.Maybe (fromMaybe)
import           Data.Traversable (for)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (CSJConfig (..), CSJEnabledConfig (..),
                     ChainSyncLoPBucketConfig (..),
                     ChainSyncLoPBucketEnabledConfig (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck
                     (HistoricityCutoff (..))
import qualified Ouroboros.Consensus.Node.GsmState as GSM
import           Ouroboros.Consensus.Storage.ChainDB (ChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.BlockFetch
                     (GenesisBlockFetchConfiguration (..))

-- | Whether to en-/disable the Limit on Eagerness and the Genesis Density
-- Disconnector.
data LoEAndGDDConfig a =
    LoEAndGDDEnabled !a
  | LoEAndGDDDisabled
  deriving stock (Eq, Generic, Show, Functor, Foldable, Traversable)

-- | Aggregating the various configs for Genesis-related subcomponents.
--
-- Usually, 'enableGenesisConfigDefault' or 'disableGenesisConfig' can be used.
-- See the haddocks of the types of the individual fields for details.
data GenesisConfig = GenesisConfig
  { gcBlockFetchConfig         :: !GenesisBlockFetchConfiguration
  , gcChainSyncLoPBucketConfig :: !ChainSyncLoPBucketConfig
  , gcCSJConfig                :: !CSJConfig
  , gcLoEAndGDDConfig          :: !(LoEAndGDDConfig ())
  , gcHistoricityCutoff        :: !(Maybe HistoricityCutoff)
  } deriving stock (Eq, Generic, Show)

-- | Genesis configuration flags and low-level args, as parsed from config file or CLI
data GenesisConfigFlags = GenesisConfigFlags
  { gcfEnableCSJ           :: Bool
  , gcfEnableLoEAndGDD     :: Bool
  , gcfEnableLoP           :: Bool
  , gcfBulkSyncGracePeriod :: Maybe Integer
  , gcfBucketCapacity      :: Maybe Integer
  , gcfBucketRate          :: Maybe Integer
  , gcfCSJJumpSize         :: Maybe Integer
  } deriving stock (Eq, Generic, Show)

defaultGenesisConfigFlags :: GenesisConfigFlags
defaultGenesisConfigFlags = GenesisConfigFlags
  { gcfEnableCSJ            = True
  , gcfEnableLoEAndGDD      = True
  , gcfEnableLoP            = True
  , gcfBulkSyncGracePeriod  = Nothing
  , gcfBucketCapacity       = Nothing
  , gcfBucketRate           = Nothing
  , gcfCSJJumpSize          = Nothing
  }

enableGenesisConfigDefault :: GenesisConfig
enableGenesisConfigDefault = mkGenesisConfig $ Just defaultGenesisConfigFlags

-- | Disable all Genesis components, yielding Praos behavior.
disableGenesisConfig :: GenesisConfig
disableGenesisConfig = mkGenesisConfig Nothing

mkGenesisConfig :: Maybe GenesisConfigFlags -> GenesisConfig
mkGenesisConfig Nothing = -- disable Genesis
  GenesisConfig
    { gcBlockFetchConfig = GenesisBlockFetchConfiguration
        { gbfcBulkSyncGracePeriod = 0 -- no grace period when Genesis is disabled
        }
    , gcChainSyncLoPBucketConfig = ChainSyncLoPBucketDisabled
    , gcCSJConfig                = CSJDisabled
    , gcLoEAndGDDConfig          = LoEAndGDDDisabled
    , gcHistoricityCutoff        = Nothing
    }
mkGenesisConfig (Just GenesisConfigFlags{..}) =
  GenesisConfig
    { gcBlockFetchConfig = GenesisBlockFetchConfiguration
        { gbfcBulkSyncGracePeriod
        }
    , gcChainSyncLoPBucketConfig = if gcfEnableLoP
        then ChainSyncLoPBucketEnabled ChainSyncLoPBucketEnabledConfig
          { csbcCapacity
          , csbcRate
          }
        else ChainSyncLoPBucketDisabled
    , gcCSJConfig = if gcfEnableCSJ
        then CSJEnabled CSJEnabledConfig
          { csjcJumpSize
          }
        else CSJDisabled
    , gcLoEAndGDDConfig = if gcfEnableLoEAndGDD
        then LoEAndGDDEnabled ()
        else LoEAndGDDDisabled
    , -- Duration in seconds of one Cardano mainnet Shelley stability window
      -- (3k/f slots times one second per slot) plus one extra hour as a
      -- safety margin.
      gcHistoricityCutoff = Just $ HistoricityCutoff $ 3 * 2160 * 20 + 3600
    }
  where
    -- The minimum amount of time during which the Genesis BlockFetch logic will
    -- download blocks from a specific peer (even if it is not performing well
    -- during that period).
    defaultBulkSyncGracePeriod = 10 -- seconds

    -- LoP parameters. Empirically, it takes less than 1ms to validate a header,
    -- so leaking one token per 2ms is conservative. The capacity of 100_000
    -- tokens corresponds to 200s, which is definitely enough to handle long GC
    -- pauses; we could even make this more conservative.
    defaultCapacity = 100_000 -- number of tokens
    defaultRate     = 500 -- tokens per second leaking, 1/2ms

    defaultCSJJumpSize = 3 * 2160 * 20 -- mainnet forecast range

    gbfcBulkSyncGracePeriod = fromInteger $ fromMaybe defaultBulkSyncGracePeriod gcfBulkSyncGracePeriod
    csbcCapacity            = fromInteger $ fromMaybe defaultCapacity gcfBucketCapacity
    csbcRate                = fromInteger $ fromMaybe defaultRate gcfBucketRate
    csjcJumpSize            = fromInteger $ fromMaybe defaultCSJJumpSize gcfCSJJumpSize

-- | Genesis-related arguments needed by the NodeKernel initialization logic.
data GenesisNodeKernelArgs m blk = GenesisNodeKernelArgs {
    -- | A TVar containing an action that returns the 'ChainDB.GetLoEFragment'
    -- action. We use this extra indirection to update this action after we
    -- opened the ChainDB (which happens before we initialize the NodeKernel).
    -- After that, this TVar will not be modified again.
    gnkaGetLoEFragment :: !(LoEAndGDDConfig (StrictTVar m (ChainDB.GetLoEFragment m blk)))
  }

-- | Create the initial 'GenesisNodeKernelArgs" (with a temporary
-- 'ChainDB.GetLoEFragment' that will be replaced via 'setGetLoEFragment') and a
-- function to update the 'ChainDbArgs' accordingly.
mkGenesisNodeKernelArgs ::
     forall m blk. (IOLike m, GetHeader blk)
  => GenesisConfig
  -> m ( GenesisNodeKernelArgs m blk
       , Complete ChainDbArgs m blk -> Complete ChainDbArgs m blk
       )
mkGenesisNodeKernelArgs gcfg = do
    gnkaGetLoEFragment <- for (gcLoEAndGDDConfig gcfg) $ \() ->
        newTVarIO $ pure $
          -- Use the most conservative LoE fragment until 'setGetLoEFragment'
          -- is called.
          ChainDB.LoEEnabled $ AF.Empty AF.AnchorGenesis
    let updateChainDbArgs = case gnkaGetLoEFragment of
          LoEAndGDDDisabled -> id
          LoEAndGDDEnabled varGetLoEFragment -> \cfg ->
            cfg { ChainDB.cdbsArgs =
                  (ChainDB.cdbsArgs cfg) { ChainDB.cdbsLoE = getLoEFragment }
                }
            where
              getLoEFragment = join $ readTVarIO varGetLoEFragment
    pure (GenesisNodeKernelArgs {gnkaGetLoEFragment}, updateChainDbArgs)

-- | Set 'gnkaGetLoEFragment' to the actual logic for determining the current
-- LoE fragment.
setGetLoEFragment ::
     forall m blk. (IOLike m, GetHeader blk)
  => STM m GSM.GsmState
  -> STM m (AnchoredFragment (Header blk))
     -- ^ The LoE fragment.
  -> StrictTVar m (ChainDB.GetLoEFragment m blk)
  -> m ()
setGetLoEFragment readGsmState readLoEFragment varGetLoEFragment =
    atomically $ writeTVar varGetLoEFragment getLoEFragment
  where
    getLoEFragment :: ChainDB.GetLoEFragment m blk
    getLoEFragment = atomically $ readGsmState >>= \case
        -- When the Honest Availability Assumption cannot currently be
        -- guaranteed, we should not select any blocks that would cause our
        -- immutable tip to advance, so we return the most conservative LoE
        -- fragment.
        GSM.PreSyncing ->
          pure $ ChainDB.LoEEnabled $ AF.Empty AF.AnchorGenesis
        -- When we are syncing, return the current LoE fragment.
        GSM.Syncing    ->
          ChainDB.LoEEnabled <$> readLoEFragment
        -- When we are caught up, the LoE is disabled.
        GSM.CaughtUp   ->
          pure ChainDB.LoEDisabled
