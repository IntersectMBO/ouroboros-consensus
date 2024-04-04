{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.Genesis (
    GenesisConfig (..)
  , GenesisNodeKernelArgs (..)
  , GenesisSwitch (..)
  , defaultGenesisConfig
  , mkGenesisNodeKernelArgs
  , setGetLoEFragment
  ) where

import           Control.Monad (join)
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

-- We have multiple other Genesis-related types of a similar shape ('LoE', LoP
-- and CSJ configs), maybe unify?
data GenesisSwitch a =
    GenesisDisabled
  | GenesisEnabled !a
  deriving stock (Show, Functor, Foldable, Traversable)

-- | Aggregating the various configs for Genesis-related subcomponents.
data GenesisConfig = GenesisConfig {
    gcsChainSyncLoPBucketConfig :: !ChainSyncLoPBucketConfig
  , gcsCSJConfig                :: !CSJConfig
  }

-- TODO justification/derivation from other parameters
defaultGenesisConfig :: GenesisConfig
defaultGenesisConfig = GenesisConfig {
      gcsChainSyncLoPBucketConfig = ChainSyncLoPBucketEnabled ChainSyncLoPBucketEnabledConfig {
          csbcCapacity = 100_000 -- number of tokens
        , csbcRate     = 500 -- tokens per second leaking, 2/ms
        }
    , gcsCSJConfig = CSJEnabled CSJEnabledConfig {
          csjcJumpSize = 3 * 2160 * 20 -- mainnet forecast range
        }
    }

-- | Genesis-related arguments needed by the NodeKernel initialization logic.
data GenesisNodeKernelArgs m blk = GenesisNodeKernelArgs {
    -- | A TVar containing an action that returns the 'ChainDB.GetLoEFragment'
    -- action. We use this extra indirection to update this action after we
    -- opened the ChainDB (which happens before we initialize the NodeKernel).
    -- After that, this TVar will not be modified again.
    gnkaGetLoEFragment :: !(StrictTVar m (ChainDB.GetLoEFragment m blk))
  }

-- | Create the initial 'GenesisNodeKernelArgs" (with a temporary
-- 'ChainDB.GetLoEFragment' that will be replaced via 'setGetLoEFragment') and a
-- function to update the 'ChainDbArgs' accordingly.
mkGenesisNodeKernelArgs ::
     forall m blk a. (IOLike m, GetHeader blk)
  => GenesisSwitch a
  -> m ( GenesisSwitch (GenesisNodeKernelArgs m blk)
       , Complete ChainDbArgs m blk -> Complete ChainDbArgs m blk
       )
mkGenesisNodeKernelArgs = \case
    GenesisDisabled  -> pure (GenesisDisabled, id)
    GenesisEnabled{} -> do
      varGetLoEFragment <- newTVarIO $ pure $
        -- Use the most conservative LoE fragment until 'setGetLoEFragment' is
        -- called.
        ChainDB.LoEEnabled $ AF.Empty AF.AnchorGenesis
      let getLoEFragment        = join $ readTVarIO varGetLoEFragment
          updateChainDbArgs cfg = cfg { ChainDB.cdbsArgs =
               (ChainDB.cdbsArgs cfg) { ChainDB.cdbsLoE = getLoEFragment }
            }
          gnka = GenesisEnabled $ GenesisNodeKernelArgs varGetLoEFragment
      pure (gnka, updateChainDbArgs)

-- | Set 'gnkaGetLoEFragment' to the actual logic for determining the current
-- LoE fragment.
setGetLoEFragment ::
     forall m blk. (IOLike m, GetHeader blk)
  => STM m GSM.GsmState
  -> STM m (AnchoredFragment (Header blk))
     -- ^ The LoE fragment.
  -> GenesisNodeKernelArgs m blk
  -> m ()
setGetLoEFragment readGsmState readLoEFragment ctx =
    atomically $ writeTVar (gnkaGetLoEFragment ctx) getLoEFragment
  where
    getLoEFragment :: ChainDB.GetLoEFragment m blk
    getLoEFragment = atomically $ readGsmState >>= \case
        -- When the HAA can currently not be guaranteed, we should not select
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
