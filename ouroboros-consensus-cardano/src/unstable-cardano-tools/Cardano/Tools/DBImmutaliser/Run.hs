{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tools.DBImmutaliser.Run
  ( Opts (..)
  , run

    -- * Setup
  , DBDirs (..)
  , withDBs

    -- * Immutalise
  , TraceImmutalisationEvent (..)
  , immutalise
  ) where

import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Control.Monad (unless)
import Control.ResourceRegistry
import Control.Tracer (Tracer (..), stdoutTracer, traceWith)
import Data.Foldable (for_)
import Data.Functor.Contravariant ((>$<))
import Data.List (intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, listToMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Dot
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Node.InitStorage
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Paths as Paths
import Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import Ouroboros.Consensus.Storage.ImmutableDB
  ( ImmutableDB
  , ImmutableDbArgs (..)
  , Tip
  , tipToPoint
  )
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Storage.VolatileDB
  ( VolatileDB
  , VolatileDbArgs (..)
  )
import qualified Ouroboros.Consensus.Storage.VolatileDB.API as VolatileDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl as VolatileDB
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block (MaxSlotNo)
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)

data Opts = Opts
  { dbDirs :: DBDirs FilePath
  , configFile :: FilePath
  , verbose :: Bool
  , dotOut :: Maybe FilePath
  , dryRun :: Bool
  }

run :: Opts -> IO ()
run Opts{dbDirs, configFile, verbose, dotOut, dryRun} = do
  let dbDirs' = SomeHasFS . ioHasFS . MountPoint <$> dbDirs
      args = Cardano.CardanoBlockArgs configFile Nothing
  ProtocolInfo{pInfoConfig = cfg} <- mkProtocolInfo args
  withRegistry $ \registry ->
    withDBs cfg registry dbDirs' $
      immutalise (configBlock cfg) (tracer <> dotTracer) dryRun
 where
  tracer = prettyTrace verbose >$< stdoutTracer
  dotTracer = Tracer $ \case
    TraceAllCandidates candidates -> do
      let dot = dotCandidates $ fst <$> candidates
      whenJust dotOut $ flip Dot.encodeToFile dot
    _ -> pure ()

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

data DBDirs a = DBDirs
  { immDBDir :: a
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
  ) =>
  TopLevelConfig blk ->
  ResourceRegistry m ->
  DBDirs (SomeHasFS m) ->
  (ImmutableDB m blk -> VolatileDB m blk -> m a) ->
  m a
withDBs cfg registry dbDirs f =
  ImmutableDB.withDB (ImmutableDB.openDB immDBArgs runWithTempRegistry) $ \immDB ->
    VolatileDB.withDB (VolatileDB.openDB volDBArgs runWithTempRegistry) $ \volDB -> do
      f immDB volDB
 where
  codecCfg = configCodec cfg
  storageCfg = configStorage cfg

  immDBArgs :: Complete ImmutableDbArgs m blk
  immDBArgs =
    ImmutableDB.defaultArgs
      { immCheckIntegrity = nodeCheckIntegrity storageCfg
      , immChunkInfo = nodeImmutableDbChunkInfo storageCfg
      , immCodecConfig = codecCfg
      , immRegistry = registry
      , immHasFS = immDBDir dbDirs
      }

  volDBArgs :: Complete VolatileDbArgs m blk
  volDBArgs =
    VolatileDB.defaultArgs
      { volCheckIntegrity = nodeCheckIntegrity (configStorage cfg)
      , volCodecConfig = codecCfg
      , volHasFS = volDBDir dbDirs
      }

{-------------------------------------------------------------------------------
  Immutalise
-------------------------------------------------------------------------------}

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
  ) =>
  BlockConfig blk ->
  Tracer m (TraceImmutalisationEvent blk) ->
  -- | Dry run?
  Bool ->
  ImmutableDB m blk ->
  VolatileDB m blk ->
  m ()
immutalise bcfg tracer dryRun immDB volDB = do
  immTip <- atomically $ ImmutableDB.getTip immDB
  volMaxSlotNo <- atomically $ VolatileDB.getMaxSlotNo volDB
  traceWith tracer $ TraceStartImmutalisation immTip volMaxSlotNo

  (succsOf, getBlockInfo) <-
    atomically $
      (,) <$> VolatileDB.filterByPredecessor volDB <*> VolatileDB.getBlockInfo volDB
  let candidates =
        Paths.maximalCandidates succsOf Nothing (tipToPoint immTip)

      -- All blocks that are reachable from the immutable tip. There might be
      -- further blocks in the VolatileDB, but the public API currently does
      -- not provide a way to observe them.
      reachableBlocks :: [VolatileDB.BlockInfo blk]
      reachableBlocks =
        fmap (fromJust . getBlockInfo) $
          Set.toAscList $
            foldMap (Set.fromList . NE.toList) candidates
  traceWith tracer $ TraceReachableBlocks reachableBlocks

  candidatesAndTipHdrs <- for candidates $ \candidate -> do
    tipHdr <-
      VolatileDB.getKnownBlockComponent volDB GetHeader (NE.last candidate)
    pure (candidate, tipHdr)
  let sortedCandidates ::
        [(NonEmpty (HeaderHash blk), SelectView (BlockProtocol blk))]
      sortedCandidates = sortOn (Down . snd) $ do
        (candidate, tipHdr) <- candidatesAndTipHdrs
        pure (candidate, selectView bcfg tipHdr)

  traceWith tracer $ TraceAllCandidates sortedCandidates

  case sortedCandidates of
    [] -> do
      traceWith tracer TraceNoVolatileCandidate
    (candidate, sv) : _ -> do
      traceWith tracer $
        TraceCandidateToImmutalise
          (NE.last candidate)
          (NE.length candidate)
          sv

      unless dryRun $ do
        -- Copy the candidate blocks from volDB to immDB.
        for_ candidate $ \hdrHash -> do
          blk <- VolatileDB.getKnownBlockComponent volDB GetBlock hdrHash
          ImmutableDB.appendBlock immDB blk

        newImmTip <- atomically $ ImmutableDB.getTip immDB
        traceWith tracer $ TraceCopiedtoImmutableDB newImmTip

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data TraceImmutalisationEvent blk
  = TraceStartImmutalisation
      -- | Tip of the ImmutableDB.
      (WithOrigin (Tip blk))
      -- | 'MaxSlotNo' of the VolatileDB.
      MaxSlotNo
  | TraceReachableBlocks
      -- | The set of volatile blocks reachable from the immutable tip.
      [VolatileDB.BlockInfo blk]
  | -- | No blocks in the VolatileDB extend the immutable tip.
    TraceNoVolatileCandidate
  | -- | Found these volatile candidates extending the immutable tip.
    TraceAllCandidates
      -- | Each candidate is represented by its block hashes of the candidate
      -- after the immutable tip, and the 'SelectView' of the candidate tip. The
      -- candidates are sorted by this 'SelectView' in decreasing order.
      [(NonEmpty (HeaderHash blk), SelectView (BlockProtocol blk))]
  | -- | The volatile candidate to immutalise.
    TraceCandidateToImmutalise
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
  ) =>
  Show (TraceImmutalisationEvent blk)

prettyTrace ::
  forall blk.
  ( ConsensusProtocol (BlockProtocol blk)
  , StandardHash blk
  ) =>
  -- | verbose?
  Bool ->
  TraceImmutalisationEvent blk ->
  String
prettyTrace verbose = \case
  TraceStartImmutalisation immTip volMaxSlot ->
    "Start immutalisation: ImmutableDB tip at "
      <> show immTip
      <> ", VolatileDB max slot at "
      <> show volMaxSlot
  TraceReachableBlocks reachableBlocks ->
    "Number of volatile blocks reachable from ImmutableDB tip: "
      <> show (length reachableBlocks)
      <> " (VolatileDB might contain more blocks)"
      <> if verbose then "\nAll hashes:\n" <> unlines (render <$> reachableBlocks) else ""
   where
    render :: VolatileDB.BlockInfo blk -> String
    render bi = intercalate "\t" [show biHash, show biSlotNo, show biBlockNo]
     where
      VolatileDB.BlockInfo
        { VolatileDB.biHash
        , VolatileDB.biSlotNo
        , VolatileDB.biBlockNo
        } = bi
  TraceNoVolatileCandidate ->
    "No volatile candidate found for immutalisation"
  TraceAllCandidates candidates ->
    unlines $
      "Number of candidates: " <> show (length candidates)
        : concat [selectViewInfo | verbose]
   where
    selectViewInfo =
      "All candidates:"
        : [ unlines
              [ " - Length: " <> show (NE.length c)
              , "   Tip hash: " <> show (NE.last c)
              , "   " <> show sv
              ]
          | (c, sv) <- candidates
          ]
  TraceCandidateToImmutalise tipHash numBlocks sv ->
    "Immutalising volatile candidate of length "
      <> show numBlocks
      <> " with tip hash "
      <> show tipHash
      <> if verbose then " and tip select view " <> show sv else ""
  TraceCopiedtoImmutableDB newImmTip ->
    "Copied to ImmutableDB, new tip is " <> show newImmTip

-- | Construct a 'Dot.DotGraph' out of a list of candidates.
dotCandidates :: forall hash. Show hash => [NonEmpty hash] -> Dot.DotGraph
dotCandidates candidates =
  Dot.DotGraph Dot.Strict Dot.Directed Nothing $ do
    candidate <- fmap renderHash . NE.toList <$> candidates
    (from, to) <- zip ("ImmTip" : candidate) candidate
    let fromTo = Dot.ListTwo (toNode from) (toNode to) []
    pure $ Dot.StatementEdge $ Dot.EdgeStatement fromTo []
 where
  toNode :: String -> Dot.EdgeElement
  toNode l = Dot.EdgeNode $ Dot.NodeId (Dot.Id $ T.pack l) Nothing

  -- Render a shortened hash like in git, i.e. the smallest prefix length
  -- such that the hashes still are unique.
  renderHash :: hash -> String
  renderHash = take prefix . show
   where
    prefix =
      fromJust $
        listToMaybe $
          [ k
          | k <- [4 ..]
          , Set.size (Set.map (take k) allHashes) == Set.size allHashes
          ]

    allHashes :: Set String
    allHashes =
      foldMap (Set.fromList . fmap show . NE.toList) candidates
