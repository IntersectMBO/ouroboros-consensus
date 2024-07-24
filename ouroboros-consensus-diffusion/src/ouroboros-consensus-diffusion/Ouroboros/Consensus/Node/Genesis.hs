{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
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
  , LoEAndGDDNodeKernelArgs (..)
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
data GenesisConfig = GenesisConfig
  { gcBlockFetchConfig         :: !GenesisBlockFetchConfiguration
  , gcChainSyncLoPBucketConfig :: !ChainSyncLoPBucketConfig
  , gcCSJConfig                :: !CSJConfig
  , gcLoEAndGDDConfig          :: !(LoEAndGDDConfig LoEAndGDDParams)
  } deriving stock (Eq, Generic, Show)

-- | Genesis configuration flags and low-level args, as parsed from config file or CLI
data GenesisConfigFlags = GenesisConfigFlags
  { gcfEnableCSJ            :: Bool
  , gcfEnableLoEAndGDD      :: Bool
  , gcfEnableLoP            :: Bool
  , gcfBulkSyncGracePeriod  :: Maybe Integer
  , gcfBucketCapacity       :: Maybe Integer
  , gcfBucketRate           :: Maybe Integer
  , gcfCSJJumpSize          :: Maybe Integer
  , gcfGDDRateLimit         :: Maybe DiffTime
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
  , gcfGDDRateLimit         = Nothing
  }

enableGenesisConfigDefault :: GenesisConfig
enableGenesisConfigDefault = mkGenesisConfig $ Just $ defaultGenesisConfigFlags

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
        then LoEAndGDDEnabled LoEAndGDDParams{lgpGDDRateLimit}
        else LoEAndGDDDisabled
    }
  where
    -- TODO justification/derivation from other parameters
    defaultBulkSyncGracePeriod = 10 -- seconds
    defaultCapacity            = 100_000 -- number of tokens
    defaultRate                = 500 -- tokens per second leaking, 1/2ms
    -- 3 * 2160 * 20 works in more recent ranges of slots, but causes syncing to
    -- block in byron.
    defaultCSJJumpSize         = 3 * 2160 - 1
    defaultGDDRateLimit        = 1.0 -- seconds

    gbfcBulkSyncGracePeriod = fromInteger $ fromMaybe defaultBulkSyncGracePeriod gcfBulkSyncGracePeriod
    csbcCapacity            = fromInteger $ fromMaybe defaultCapacity gcfBucketCapacity
    csbcRate                = fromInteger $ fromMaybe defaultRate gcfBucketRate
    csjcJumpSize            = fromInteger $ fromMaybe defaultCSJJumpSize gcfCSJJumpSize
    lgpGDDRateLimit            = fromMaybe defaultGDDRateLimit gcfGDDRateLimit

newtype LoEAndGDDParams = LoEAndGDDParams
  { -- | How often to evaluate GDD. 0 means as soon as possible.
    -- Otherwise, no faster than once every T seconds, where T is the
    -- value of the field.
    lgpGDDRateLimit :: DiffTime
  } deriving stock (Eq, Generic, Show)

-- | Genesis-related arguments needed by the NodeKernel initialization logic.
data GenesisNodeKernelArgs m blk = GenesisNodeKernelArgs {
    gnkaLoEAndGDDArgs :: !(LoEAndGDDConfig (LoEAndGDDNodeKernelArgs m blk))
  }

data LoEAndGDDNodeKernelArgs m blk = LoEAndGDDNodeKernelArgs {
    -- | A TVar containing an action that returns the 'ChainDB.GetLoEFragment'
    -- action. We use this extra indirection to update this action after we
    -- opened the ChainDB (which happens before we initialize the NodeKernel).
    -- After that, this TVar will not be modified again.
    lgnkaLoEFragmentTVar :: !(StrictTVar m (ChainDB.GetLoEFragment m blk))
  , lgnkaGDDRateLimit   :: DiffTime
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
    gnkaLoEAndGDDArgs <- for (gcLoEAndGDDConfig gcfg) $ \p -> do
        loeFragmentTVar <- newTVarIO $ pure $
          -- Use the most conservative LoE fragment until 'setGetLoEFragment'
          -- is called.
          ChainDB.LoEEnabled $ AF.Empty AF.AnchorGenesis
        pure LoEAndGDDNodeKernelArgs
          { lgnkaLoEFragmentTVar = loeFragmentTVar
          , lgnkaGDDRateLimit = lgpGDDRateLimit p
          }
    let updateChainDbArgs = case gnkaLoEAndGDDArgs of
          LoEAndGDDDisabled -> id
          LoEAndGDDEnabled lgnkArgs -> \cfg ->
            cfg { ChainDB.cdbsArgs =
                  (ChainDB.cdbsArgs cfg) { ChainDB.cdbsLoE = getLoEFragment }
                }
            where
              getLoEFragment = join $ readTVarIO $ lgnkaLoEFragmentTVar lgnkArgs
    pure (GenesisNodeKernelArgs{gnkaLoEAndGDDArgs}, updateChainDbArgs)

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
