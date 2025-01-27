{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tools.DBImmutaliser.Run (
    Opts (..)
  , run
    -- * Setup
  , DBDirs (..)
  , withDBs
    -- * Immutalise
  , TraceImmutalisationEvent (..)
  , immutalise
  ) where

import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import           Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import           Control.ResourceRegistry
import           Control.Tracer (Tracer, stdoutTracer, traceWith)
import           Data.Foldable (for_)
import           Data.Functor.Contravariant ((>$<))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup (Arg (..), ArgMax, Max (..))
import           Data.Traversable (for)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Paths as Paths
import           Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB,
                     ImmutableDbArgs (..), Tip, tipToPoint)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.VolatileDB (VolatileDB,
                     VolatileDbArgs (..))
import qualified Ouroboros.Consensus.Storage.VolatileDB.API as VolatileDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl as VolatileDB
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block (MaxSlotNo)
import           System.FS.API (SomeHasFS (..))
import           System.FS.API.Types (MountPoint (..))
import           System.FS.IO (ioHasFS)

data Opts = Opts {
    dbDirs     :: DBDirs FilePath
  , configFile :: FilePath
  }

run :: Opts -> IO ()
run Opts {dbDirs, configFile} = do
    let dbDirs' = SomeHasFS . ioHasFS . MountPoint <$> dbDirs
        args    = Cardano.CardanoBlockArgs configFile Nothing
    ProtocolInfo{pInfoConfig = cfg} <- mkProtocolInfo args
    withRegistry $ \registry ->
      withDBs cfg registry dbDirs' $
        immutalise (configBlock cfg) (show >$< stdoutTracer)

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

data DBDirs a = DBDirs {
    immDBDir :: a
  , volDBDir :: a
  }
  deriving stock (Functor, Foldable, Traversable)

withDBs ::
     forall m blk a.
     ( IOLike m
     , ConvertRawHash blk
     , LedgerSupportsProtocol blk
     , ImmutableDB.ImmutableDbSerialiseConstraints blk
     , VolatileDB.VolatileDbSerialiseConstraints blk
     , NodeInitStorage blk
     )
  => TopLevelConfig blk
  -> ResourceRegistry m
  -> DBDirs (SomeHasFS m)
  -> (ImmutableDB m blk -> VolatileDB m blk -> m a)
  -> m a
withDBs cfg registry dbDirs f =
    ImmutableDB.withDB (ImmutableDB.openDB immDBArgs runWithTempRegistry) $ \immDB ->
    VolatileDB.withDB  (VolatileDB.openDB  volDBArgs runWithTempRegistry) $ \volDB -> do
      f immDB volDB
  where
    codecCfg   = configCodec   cfg
    storageCfg = configStorage cfg

    immDBArgs :: Complete ImmutableDbArgs m blk
    immDBArgs = ImmutableDB.defaultArgs {
          immCheckIntegrity = nodeCheckIntegrity storageCfg
        , immChunkInfo      = nodeImmutableDbChunkInfo storageCfg
        , immCodecConfig    = codecCfg
        , immRegistry       = registry
        , immHasFS          = immDBDir dbDirs
        }

    volDBArgs :: Complete VolatileDbArgs m blk
    volDBArgs = VolatileDB.defaultArgs {
          volCheckIntegrity = nodeCheckIntegrity (configStorage cfg)
        , volCodecConfig    = codecCfg
        , volHasFS          = volDBDir dbDirs
        }

{-------------------------------------------------------------------------------
  Immutalise
-------------------------------------------------------------------------------}

data TraceImmutalisationEvent blk =
    TraceStartImmutalisation
      -- | Tip of the ImmutableDB.
      (WithOrigin (Tip blk))
      -- | 'MaxSlotNo' of the VolatileDB.
      MaxSlotNo
  | -- | No blocks in the VolatileDB extend the immutable tip.
    TraceNoVolatileCandidate
  | -- | Found a volatile candidate extending the immutable tip.
    TraceFoundCandidate
      -- | Hash of the candidate tip.
      (HeaderHash blk)
      -- | Blocks to be added to the ImmutableDB.
      Int
      -- | 'SelectView' of the candidate tip.
      (SelectView (BlockProtocol blk))
  | TraceCopiedtoImmutableDB
      -- | New tip of the ImmutableDB.
      (WithOrigin (Tip blk))

deriving stock instance
  ( ConsensusProtocol (BlockProtocol blk)
  , StandardHash blk
  ) => Show (TraceImmutalisationEvent blk)

-- data ImmutalisationException =

-- | Copy a specific chain from the given 'VolatileDB' to the 'ImmutableDB',
-- such that other tools that only work with an 'ImmutableDB' can process the
-- corresponding blocks.
--
-- This function requires exclusive access to the databases.
--
-- Currently, this will pick the best (according to the 'SelectView') chain
-- extending the immutable tip, and it will _not_ do any kind of validation.
--
-- Future work might include customizing this behavior, like:
--
--   * picking the best _valid_ chain (requires reading a ledger snapshot and/or
--     replaying)
--
--   * picking a chain that contains particular points (user input)
immutalise ::
     forall m blk.
     ( IOLike m
     , BlockSupportsProtocol blk
     )
  => BlockConfig blk
  -> Tracer m (TraceImmutalisationEvent blk)
  -> ImmutableDB m blk
  -> VolatileDB m blk
  -> m ()
immutalise bcfg tracer immDB volDB = do
    immTip       <- atomically $ ImmutableDB.getTip immDB
    volMaxSlotNo <- atomically $ VolatileDB.getMaxSlotNo volDB
    traceWith tracer $ TraceStartImmutalisation immTip volMaxSlotNo

    succsOf <- atomically $ VolatileDB.filterByPredecessor volDB
    let candidates =
          Paths.maximalCandidates succsOf Nothing (tipToPoint immTip)
    candidatesAndTipHdrs <- for candidates $ \candidate -> do
      tipHdr <-
        VolatileDB.getKnownBlockComponent volDB GetHeader (NE.last candidate)
      pure (candidate, tipHdr)
    let mBestCandidate ::
             Maybe (ArgMax (SelectView (BlockProtocol blk)) (NonEmpty (HeaderHash blk)))
        mBestCandidate = mconcat $ do
          (candidate, tipHdr) <- candidatesAndTipHdrs
          pure $ Just $ Max $ Arg (selectView bcfg tipHdr) candidate

    case mBestCandidate of
      Nothing                       -> do
        traceWith tracer TraceNoVolatileCandidate
      Just (Max (Arg sv candidate)) -> do
        traceWith tracer $ TraceFoundCandidate
          (NE.last candidate)
          (NE.length candidate)
          sv

        -- Copy the candidate blocks from volDB to immDB.
        for_ candidate $ \hdrHash -> do
          blk <- VolatileDB.getKnownBlockComponent volDB GetBlock hdrHash
          ImmutableDB.appendBlock immDB blk

        newImmTip <- atomically $ ImmutableDB.getTip immDB
        traceWith tracer $ TraceCopiedtoImmutableDB newImmTip
