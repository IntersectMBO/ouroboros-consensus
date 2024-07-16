{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.Genesis (
    -- * 'GenesisConfig'
    GenesisConfig (..)
  , LoEAndGDDConfig (..)
  , disableGenesisConfig
  , enableGenesisConfigDefault
    -- * NodeKernel helpers
  , GenesisNodeKernelArgs (..)
  , mkGenesisNodeKernelArgs
  , setGetLoEFragment
  ) where

import           Control.Monad (join)
import           Data.Maybe (isJust)
import           Data.Traversable (for)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (CSJConfig (..), CSJEnabledConfig (..),
                     ChainSyncLoPBucketConfig (..),
                     ChainSyncLoPBucketEnabledConfig (..))
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
import           System.Environment (lookupEnv)
import           System.IO.Unsafe (unsafePerformIO)

-- | Whether to en-/disable the Limit on Eagerness and the Genesis Density
-- Disconnector.
data LoEAndGDDConfig a =
    LoEAndGDDEnabled !a
  | LoEAndGDDDisabled
  deriving stock (Show, Functor, Foldable, Traversable)

-- | Aggregating the various configs for Genesis-related subcomponents.
data GenesisConfig = GenesisConfig
  { gcBlockFetchConfig         :: !GenesisBlockFetchConfiguration
  , gcChainSyncLoPBucketConfig :: !ChainSyncLoPBucketConfig
  , gcCSJConfig                :: !CSJConfig
  , gcLoEAndGDDConfig          :: !(LoEAndGDDConfig ())
  }

-- TODO justification/derivation from other parameters
enableGenesisConfigDefault :: GenesisConfig
enableGenesisConfigDefault = unsafePerformIO $ do
  enableGenesis   <- isJust <$> lookupEnv "ENABLE_GENESIS"
  enableLoP       <- (enableGenesis ||) . isJust <$> lookupEnv "ENABLE_LoP"
  enableCSJ       <- (enableGenesis ||) . isJust <$> lookupEnv "ENABLE_CSJ"
  enableLoEAndGDD <- (enableGenesis ||) . isJust <$> lookupEnv "ENABLE_LoEGDD"

  let defaultBulkSyncGracePeriod = 10 -- seconds
      defaultCapacity            = 100_000 -- number of tokens
      defaultRate                = 500 -- tokens per second leaking, 1/2ms
      defaultCSJJumpSize         = 3 * 2160 * 20 -- mainnet forecast range

  gbfcBulkSyncGracePeriod <- maybe defaultBulkSyncGracePeriod
    (fromInteger . read) <$> lookupEnv "BLOCKFETCH_GRACE_PERIOD"
  csbcCapacity <- maybe defaultCapacity
    (fromInteger . read) <$> lookupEnv "LOP_CAPACITY"
  csbcRate <- maybe defaultRate
    (fromInteger . read) <$> lookupEnv "LOP_RATE"
  csjcJumpSize <- maybe defaultCSJJumpSize
    (fromInteger . read) <$> lookupEnv "CSJ_JUMP_SIZE"

  pure $ GenesisConfig
    { gcBlockFetchConfig = GenesisBlockFetchConfiguration
        { gbfcBulkSyncGracePeriod
        }
    , gcChainSyncLoPBucketConfig = if enableLoP
        then ChainSyncLoPBucketEnabled ChainSyncLoPBucketEnabledConfig
          { csbcCapacity
          , csbcRate
          }
        else ChainSyncLoPBucketDisabled
    , gcCSJConfig = if enableCSJ
        then CSJEnabled CSJEnabledConfig
          { csjcJumpSize
          }
        else CSJDisabled
    , gcLoEAndGDDConfig = if enableLoEAndGDD
        then LoEAndGDDEnabled ()
        else LoEAndGDDDisabled
    }
{-# NOINLINE enableGenesisConfigDefault #-}

-- | Disable all Genesis components, yielding Praos behavior.
disableGenesisConfig :: GenesisConfig
disableGenesisConfig = GenesisConfig
    { gcBlockFetchConfig = GenesisBlockFetchConfiguration
        { gbfcBulkSyncGracePeriod = 0 -- no grace period when Genesis is disabled
        }
    , gcChainSyncLoPBucketConfig = ChainSyncLoPBucketDisabled
    , gcCSJConfig                = CSJDisabled
    , gcLoEAndGDDConfig          = LoEAndGDDDisabled
    }

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
        -- When the Honest Availability Assumption cannot currently be guaranteed, we should not select
        -- any blocks that would cause our immutable tip to advance, so we
        -- return the most conservative LoE fragment.
        GSM.PreSyncing ->
          pure $ ChainDB.LoEEnabled $ AF.Empty AF.AnchorGenesis
        -- When we are syncing, return the current LoE fragment.
        GSM.Syncing    ->
          ChainDB.LoEEnabled <$> readLoEFragment
        -- When we are caught up, the LoE is disabled.
        GSM.CaughtUp   ->
          pure ChainDB.LoEDisabled
