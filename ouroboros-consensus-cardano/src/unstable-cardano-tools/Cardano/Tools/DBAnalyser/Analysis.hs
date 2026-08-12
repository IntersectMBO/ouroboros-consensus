{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Tools.DBAnalyser.Analysis
  ( AnalysisEnv (..)
  , AnalysisName (..)
  , AnalysisResult (..)
  , AnalysisStartFrom (..)
  , LedgerApplicationMode (..)
  , Limit (..)
  , NumberOfBlocks (..)
  , SStartFrom (..)
  , SomeAnalysis (..)
  , StartFrom (..)
  , runAnalysis
  ) where

import qualified Cardano.Slotting.Slot as Slotting
import qualified Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.FileWriting as F
import qualified Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint as DP
import Cardano.Tools.DBAnalyser.CSV
  ( computeAndWriteLine
  , writeHeaderLine
  )
import Cardano.Tools.DBAnalyser.HasAnalysis (HasAnalysis)
import qualified Cardano.Tools.DBAnalyser.HasAnalysis as HasAnalysis
import Cardano.Tools.DBAnalyser.Leios
  ( ClosureApplied (..)
  , announcementAtPoint
  , applyBlockAtTip
  , applyBlockToTipForker
  , applyClosure
  , blockWithCertifiedEbTxs
  , certifiedEbHash
  , certifiedEbTxSizes
  , closureKeySets
  , parentAnnouncement
  , readEbClosure
  , verifyCertRb
  )
import Cardano.Tools.DBAnalyser.Types
import Control.Monad (join, unless, void, when)
import Control.Monad.Except (runExcept)
import Control.ResourceRegistry
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.Bifunctor (bimap)
import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Singletons
import Data.Word (Word16, Word32, Word64)
import qualified Debug.Trace as Debug
import qualified GHC.Stats as GC
import LeiosDemoDb (LeiosDbConnection)
import LeiosDemoTypes
  ( BytesSize
  , HasLeiosVoting (..)
  , LeiosPoint
  , maxEBClosureSize
  )
import NoThunks.Class (noThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.HeaderValidation
  ( HasAnnTip (..)
  , HeaderState (..)
  , headerStatePoint
  , revalidateHeader
  , tickHeaderState
  , validateHeader
  )
import Ouroboros.Consensus.Ledger.Abstract
  ( ApplyBlock (getBlockKeySets, reapplyBlockLedgerResult)
  , applyBlockLedgerResult
  )
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( LedgerSupportsMempool
  )
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol (..)
  )
import Ouroboros.Consensus.Ledger.Tables.Utils
import qualified Ouroboros.Consensus.Mempool as Mempool
import Ouroboros.Consensus.Mempool.Impl.Common
import Ouroboros.Consensus.Protocol.Abstract (LedgerView)
import Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ResolveLeiosBlock
  , headerLeiosAnnouncement
  )
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.SizeInBytes
import qualified System.IO as IO

{-------------------------------------------------------------------------------
  Run the requested analysis
-------------------------------------------------------------------------------}

runAnalysis ::
  forall blk.
  ( HasAnalysis blk
  , LedgerSupportsMempool.HasTxId (LedgerSupportsMempool.GenTx blk)
  , LedgerSupportsMempool.HasTxs blk
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  , CanStowLedgerTables (LedgerState blk)
  ) =>
  AnalysisName -> SomeAnalysis blk
runAnalysis analysisName = case go analysisName of
  SomeAnalysis p analysis -> SomeAnalysis p $ \env@AnalysisEnv{tracer} -> do
    traceWith tracer (StartedEvent analysisName)
    result <- analysis env
    traceWith tracer DoneEvent
    pure result
 where
  go :: AnalysisName -> SomeAnalysis blk
  go ShowSlotBlockNo = mkAnalysis $ showSlotBlockNo
  go CountTxOutputs = mkAnalysis $ countTxOutputs
  go ShowBlockHeaderSize = mkAnalysis $ showHeaderSize
  go ShowBlockTxsSize = mkAnalysis $ showBlockTxsSize
  go ShowEBBs = mkAnalysis $ showEBBs
  go OnlyValidation = mkAnalysis @StartFromPoint $ \_ -> pure Nothing
  go (StoreLedgerStateAt slotNo lgrAppMode) = mkAnalysis $ storeLedgerStateAt slotNo lgrAppMode
  go CountBlocks = mkAnalysis $ countBlocks
  go (CheckNoThunksEvery nBks) = mkAnalysis $ checkNoThunksEvery nBks
  go TraceLedgerProcessing = mkAnalysis $ traceLedgerProcessing
  go (ReproMempoolAndForge nBks) = mkAnalysis $ reproMempoolForge nBks
  go (BenchmarkLedgerOps mOutfile lgrAppMode) = mkAnalysis $ benchmarkLedgerOps mOutfile lgrAppMode
  go (GetBlockApplicationMetrics nrBlocks mOutfile) = mkAnalysis $ getBlockApplicationMetrics nrBlocks mOutfile

  mkAnalysis ::
    forall startFrom.
    SingI startFrom =>
    Analysis blk startFrom -> SomeAnalysis blk
  mkAnalysis = SomeAnalysis (Proxy @startFrom)

type Analysis blk startFrom = AnalysisEnv IO blk startFrom -> IO (Maybe AnalysisResult)

data SomeAnalysis blk
  = forall startFrom.
    SingI startFrom =>
    SomeAnalysis (Proxy startFrom) (Analysis blk startFrom)

data AnalysisEnv m blk startFrom = AnalysisEnv
  { cfg :: TopLevelConfig blk
  , startFrom :: AnalysisStartFrom m blk startFrom
  , db :: ImmutableDB IO blk
  , registry :: ResourceRegistry IO
  , limit :: Limit
  , tracer :: Tracer m (TraceEvent blk)
  , leiosDb :: LeiosDbConnection IO
  -- ^ Connection to the node's LeiosDb. For pre-Leios chains this is
  -- a connection to the empty in-memory stub and is never consulted.
  }

-- | Whether the db-analyser pass needs access to a ledger state.
data StartFrom = StartFromPoint | StartFromLedgerState

data SStartFrom startFrom where
  SStartFromPoint :: SStartFrom StartFromPoint
  SStartFromLedgerState :: SStartFrom StartFromLedgerState

type instance Sing = SStartFrom
instance SingI StartFromPoint where sing = SStartFromPoint
instance SingI StartFromLedgerState where sing = SStartFromLedgerState

data AnalysisStartFrom m blk startFrom where
  FromPoint ::
    Point blk -> AnalysisStartFrom m blk StartFromPoint
  FromLedgerState ::
    LedgerDB.LedgerDB' m blk ->
    LedgerDB.TestInternals' m blk ->
    AnalysisStartFrom m blk StartFromLedgerState

startFromPoint ::
  (IOLike.IOLike m, HasAnnTip blk) => AnalysisStartFrom m blk startFrom -> m (Point blk)
startFromPoint = \case
  FromPoint pt -> pure pt
  FromLedgerState st _ -> headerStatePoint . headerState <$> IOLike.atomically (LedgerDB.getVolatileTip st)

data TraceEvent blk
  = -- | triggered when given analysis has started
    StartedEvent AnalysisName
  | -- | triggered when analysis has ended
    DoneEvent
  | -- | triggered when block has been found, it holds:
    --   * block's number
    --   * slot number when the block was forged
    BlockSlotEvent BlockNo SlotNo (HeaderHash blk)
  | -- | triggered when block has been found, it holds:
    --   * block's number
    --   * slot number when the block was forged
    --   * the transaction output counts of that block
    CountTxOutputsEvent BlockNo SlotNo TxOutputCounts
  | -- | triggered when EBB block has been found, it holds:
    --   * its hash,
    --   * hash of previous block
    --   * flag whether the EBB is known
    EbbEvent (HeaderHash blk) (ChainHash blk) Bool
  | -- | triggered once during CountBLocks analysis,
    --   when blocks were counted
    CountedBlocksEvent Int
  | -- | triggered when header size has been measured
    --   * block's number
    --   * slot number when the block was forged
    --   * block's header size
    --   * block's size
    HeaderSizeEvent BlockNo SlotNo Word16 Word32
  | -- | triggered once during ShowBlockTxsSize analysis,
    --   holding maximum encountered header size
    MaxHeaderSizeEvent Word16
  | -- | triggered when snapshot of ledger has been stored for SlotNo
    SnapshotStoredEvent SlotNo
  | -- | triggered once during  StoreLedgerStateAt analysis,
    --   when snapshot was created in slot proceeding the
    --   requested one
    SnapshotWarningEvent SlotNo SlotNo
  | -- | triggered when applying a block with the given point failed
    LedgerErrorEvent (Point blk) (ExtValidationError blk)
  | -- | triggered for all blocks during ShowBlockTxsSize analysis,
    --   it holds:
    --   * slot number when the block was forged
    --   * the transaction counts and sizes of that block
    BlockTxSizeEvent SlotNo BlockTxSizes
  | -- | triggered for all blocks during MempoolAndForgeRepro analysis,
    --   it holds:
    --   * block number
    --   * slot number when the block was forged
    --   * what the repro measured on that block
    BlockMempoolAndForgeRepro BlockNo SlotNo MempoolAndForgeRepro

-- | How many transaction outputs a block holds, and how many the chain holds up
-- to and including that block.
data TxOutputCounts = TxOutputCounts
  { numBlockTxOutputs :: Int
  , numEbTxOutputs :: Int
  , cumulativeTxOutputs :: Int
  }

-- | How many transactions a block holds, and how many bytes they take.
data BlockTxSizes = BlockTxSizes
  { numBlockTxs :: Int
  , blockTxsSize :: SizeInBytes
  , numEbTxs :: Int
  , ebTxsSize :: SizeInBytes
  }

-- | What the MempoolAndForgeRepro analysis measured on one block.
data MempoolAndForgeRepro = MempoolAndForgeRepro
  { numTxs :: Int
  -- ^ Number of transactions in the block.
  , txsSize :: SizeInBytes
  -- ^ Total size of transactions in the block.
  , ebNumTxs :: Int
  -- ^ Number of transactions in the EB that the block certifies.
  , ebTxsByteSize :: SizeInBytes
  -- ^ Total size of transactions in the EB that the block certifies.
  , durEbRead :: IOLike.DiffTime
  -- ^ Monotonic time to read the closure of the certified EB
  , mutEbRead :: Int64
  -- ^ Total time spent in the mutator when reading the closure of the certified EB
  , gcEbRead :: Int64
  -- ^ Total time spent in gc when reading the closure of the certified EB
  , durEbTableRead :: IOLike.DiffTime
  -- ^ Monotonic time of the value read. That read gets the ledger values that
  -- the transactions of the closure consume. 'durEbApply' excludes it.
  , mutEbTableRead :: Int64
  -- ^ Total time spent in the mutator during the value read
  , gcEbTableRead :: Int64
  -- ^ Total time spent in gc during the value read
  , durEbApply :: IOLike.DiffTime
  -- ^ Monotonic time to apply the closure of the certified EB
  , mutEbApply :: Int64
  -- ^ Total time spent in the mutator when applying the closure of the certified EB
  , gcEbApply :: Int64
  -- ^ Total time spent in gc when applying the closure of the certified EB
  , durTick :: IOLike.DiffTime
  -- ^ Monotonic time to tick ledger state.
  , mutTick :: Int64
  -- ^ Total time spent in the mutator when ticking the ledger state.
  , gcTick :: Int64
  -- ^ Total time spent in gc when ticking the ledger state.
  , durSnap :: IOLike.DiffTime
  -- ^ Monotonic time to call 'Mempool.getSnapshotFor'.
  , mutSnap :: Int64
  -- ^ Total time spent in the mutator when calling 'Mempool.getSnapshotFor'.
  , gcSnap :: Int64
  -- ^ Total time spent in gc when calling 'Mempool.getSnapshotFor'.
  }

instance (HasAnalysis blk, LedgerSupportsProtocol blk) => Show (TraceEvent blk) where
  show (StartedEvent analysisName) = "Started " <> (show analysisName)
  show DoneEvent = "Done"
  show (BlockSlotEvent bn sn h) =
    intercalate "\t" $
      [ show bn
      , show sn
      , show h
      ]
  show (CountTxOutputsEvent bn sn TxOutputCounts{numBlockTxOutputs, numEbTxOutputs, cumulativeTxOutputs}) =
    intercalate "\t" $
      [ show bn
      , show sn
      , "cumulative: " <> show cumulativeTxOutputs
      , "count: " <> show numBlockTxOutputs
      , "EB count: " <> show numEbTxOutputs
      ]
  show (EbbEvent ebb previous known) =
    intercalate
      "\t"
      [ "EBB: " <> show ebb
      , "Prev: " <> show previous
      , "Known: " <> show known
      ]
  show (CountedBlocksEvent counted) = "Counted " <> show counted <> " blocks."
  show (HeaderSizeEvent bn sn hSz bSz) =
    intercalate "\t" $
      [ show bn
      , show sn
      , "header size: " <> show hSz
      , "block size: " <> show bSz
      ]
  show (MaxHeaderSizeEvent size) =
    "Maximum encountered header size = " <> show size
  show (SnapshotStoredEvent slot) =
    "Snapshot stored at " <> show slot
  show (SnapshotWarningEvent requested actual) =
    "Snapshot was created at "
      <> show actual
      <> " "
      <> "because there was no block forged at requested "
      <> show requested
  show (LedgerErrorEvent pt err) =
    "Applying block at " <> show pt <> " failed: " <> show err
  show (BlockTxSizeEvent slot BlockTxSizes{numBlockTxs, blockTxsSize, numEbTxs, ebTxsSize}) =
    intercalate
      "\t"
      [ show slot
      , "Num txs in block = " <> show numBlockTxs
      , "Total size of txs in block = " <> show blockTxsSize
      , "Num txs in EB = " <> show numEbTxs
      , "Total size of txs in EB = " <> show ebTxsSize
      ]
  show
    ( BlockMempoolAndForgeRepro
        bno
        slot
        MempoolAndForgeRepro
          { numTxs
          , txsSize
          , ebNumTxs
          , ebTxsByteSize
          , durEbRead
          , mutEbRead
          , gcEbRead
          , durEbTableRead
          , mutEbTableRead
          , gcEbTableRead
          , durEbApply
          , mutEbApply
          , gcEbApply
          , durTick
          , mutTick
          , gcTick
          , durSnap
          , mutSnap
          , gcSnap
          }
      ) =
      intercalate
        "\t"
        [ show bno
        , show slot
        , "txsCount " <> show numTxs
        , "txsSize " <> show txsSize
        , "ebNumTxs " <> show ebNumTxs
        , "ebTxsByteSize " <> show ebTxsByteSize
        , "durEbRead " <> show durEbRead
        , "mutEbRead " <> show mutEbRead
        , "gcEbRead " <> show gcEbRead
        , "durEbTableRead " <> show durEbTableRead
        , "mutEbTableRead " <> show mutEbTableRead
        , "gcEbTableRead " <> show gcEbTableRead
        , "durEbApply " <> show durEbApply
        , "mutEbApply " <> show mutEbApply
        , "gcEbApply " <> show gcEbApply
        , "durTick " <> show durTick
        , "mutTick " <> show mutTick
        , "gcTick " <> show gcTick
        , "durSnap " <> show durSnap
        , "mutSnap " <> show mutSnap
        , "gcSnap " <> show gcSnap
        ]

{-------------------------------------------------------------------------------
  Analysis: show block and slot number and hash for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
showSlotBlockNo AnalysisEnv{db, registry, startFrom, limit, tracer} =
  processAll_ db registry GetHeader startFrom limit process
    >> pure Nothing
 where
  process :: Header blk -> IO ()
  process hdr =
    traceWith tracer $
      BlockSlotEvent (blockNo hdr) (blockSlot hdr) (headerHash hdr)

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

-- | A cert-RB has an empty wire body, so its own count is 0. The txs that it
-- causes the ledger to apply are in the EB that it certifies. The EB count
-- reports those.
countTxOutputs ::
  forall blk.
  (HasAnalysis blk, ResolveLeiosBlock blk) =>
  Analysis blk StartFromPoint
countTxOutputs AnalysisEnv{db, registry, startFrom, limit, tracer, leiosDb} = do
  seed <- announcementAtPoint db =<< startFromPoint startFrom
  void $ processAll db registry GetBlock startFrom limit (0, seed) process
  pure Nothing
 where
  process ::
    -- The count so far, and the EB that the previous block announced
    (Int, Maybe (LeiosPoint, BytesSize)) ->
    blk ->
    IO (Int, Maybe (LeiosPoint, BytesSize))
  process (cumulative, prevAnnouncement) blk = do
    ebBlk <- blockWithCertifiedEbTxs leiosDb prevAnnouncement blk
    let numBlockTxOutputs = HasAnalysis.countTxOutputs blk
        numEbTxOutputs = maybe 0 HasAnalysis.countTxOutputs ebBlk
        cumulativeTxOutputs = cumulative + numBlockTxOutputs + numEbTxOutputs
    traceWith tracer $
      CountTxOutputsEvent
        (blockNo blk)
        (blockSlot blk)
        TxOutputCounts{numBlockTxOutputs, numEbTxOutputs, cumulativeTxOutputs}
    pure (cumulativeTxOutputs, headerLeiosAnnouncement (getHeader blk))

{-------------------------------------------------------------------------------
  Analysis: show the header size in bytes for all blocks
-------------------------------------------------------------------------------}

showHeaderSize :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
showHeaderSize AnalysisEnv{db, registry, startFrom, limit, tracer} = do
  maxHeaderSize <-
    processAll
      db
      registry
      ((,,) <$> GetHeader <*> GetHeaderSize <*> GetBlockSize)
      startFrom
      limit
      0
      process
  traceWith tracer $ MaxHeaderSizeEvent maxHeaderSize
  pure $ Just $ ResultMaxHeaderSize maxHeaderSize
 where
  process :: Word16 -> (Header blk, Word16, SizeInBytes) -> IO Word16
  process maxHeaderSize (hdr, headerSize, blockSize) = do
    let event =
          HeaderSizeEvent
            (blockNo hdr)
            (blockSlot hdr)
            headerSize
            (getSizeInBytes blockSize)
    traceWith tracer event
    return $ maxHeaderSize `max` headerSize

{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

-- | A cert-RB has an empty wire body, so its own tx columns are 0. The txs that
-- it causes the ledger to apply are in the EB that it certifies. The EB columns
-- report those.
showBlockTxsSize ::
  forall blk.
  (HasAnalysis blk, ResolveLeiosBlock blk) =>
  Analysis blk StartFromPoint
showBlockTxsSize AnalysisEnv{db, registry, startFrom, limit, tracer, leiosDb} = do
  seed <- announcementAtPoint db =<< startFromPoint startFrom
  void $ processAll db registry GetBlock startFrom limit seed process
  pure Nothing
 where
  process ::
    -- The EB that the previous block announced
    Maybe (LeiosPoint, BytesSize) ->
    blk ->
    IO (Maybe (LeiosPoint, BytesSize))
  process prevAnnouncement blk = do
    ebTxSizes <- certifiedEbTxSizes leiosDb prevAnnouncement blk
    traceWith tracer $
      BlockTxSizeEvent
        (blockSlot blk)
        BlockTxSizes
          { numBlockTxs = length txSizes
          , blockTxsSize = sum txSizes
          , numEbTxs = length ebTxSizes
          , ebTxsSize = sum ebTxSizes
          }
    pure $ headerLeiosAnnouncement (getHeader blk)
   where
    txSizes :: [SizeInBytes]
    txSizes = HasAnalysis.blockTxSizes blk

{-------------------------------------------------------------------------------
  Analysis: show EBBs and their predecessors
-------------------------------------------------------------------------------}

showEBBs :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
showEBBs AnalysisEnv{db, registry, startFrom, limit, tracer} = do
  processAll_ db registry GetBlock startFrom limit process
  pure Nothing
 where
  process :: blk -> IO ()
  process blk =
    case blockIsEBB blk of
      Just _epoch -> do
        let known =
              Map.lookup
                (blockHash blk)
                (HasAnalysis.knownEBBs (Proxy @blk))
                == Just (blockPrevHash blk)
            event = EbbEvent (blockHash blk) (blockPrevHash blk) known
        traceWith tracer event
      _otherwise -> return () -- Skip regular blocks

{-------------------------------------------------------------------------------
  Analysis: store a ledger at specific slot
-------------------------------------------------------------------------------}

storeLedgerStateAt ::
  forall blk.
  ( LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  , HasAnalysis blk
  ) =>
  SlotNo ->
  LedgerApplicationMode ->
  Analysis blk StartFromLedgerState
storeLedgerStateAt slotNo ledgerAppMode env = do
  void $ processAllUntil db registry GetBlock startFrom limit () process
  pure Nothing
 where
  AnalysisEnv{db, registry, startFrom, cfg, limit, tracer, leiosDb} = env
  FromLedgerState ldb internal = startFrom

  process :: () -> blk -> IO (NextStep, ())
  process _ blk =
    LedgerDB.withTipForker
      ldb
      ( \frk -> do
          result <- applyBlockToTipForker leiosDb ledgerAppMode cfg frk blk
          case result of
            Right newLedger -> do
              LedgerDB.forkerPush frk newLedger
              join $ IOLike.atomically $ LedgerDB.forkerCommit frk
              when (blockSlot blk > slotNo) $ issueWarning blk
              when ((unBlockNo $ blockNo blk) `mod` 1000 == 0) $ reportProgress blk
              LedgerDB.tryFlush ldb
              LedgerDB.garbageCollect ldb
                . fromWithOrigin 0
                . pointSlot
                . getTip
                =<< IOLike.atomically (LedgerDB.getImmutableTip ldb)
              when (blockSlot blk >= slotNo) storeLedgerState
              return (continue blk, ())
            Left err -> do
              traceWith tracer $
                LedgerErrorEvent (blockPoint blk) (LedgerDB.annLedgerErr err)
              storeLedgerState
              pure (Stop, ())
      )

  continue :: blk -> NextStep
  continue blk
    | blockSlot blk >= slotNo = Stop
    | otherwise = Continue

  issueWarning blk =
    let event = SnapshotWarningEvent slotNo (blockSlot blk)
     in traceWith tracer event
  reportProgress blk =
    let event = BlockSlotEvent (blockNo blk) (blockSlot blk) (blockHash blk)
     in traceWith tracer event

  storeLedgerState :: IO ()
  storeLedgerState =
    IOLike.atomically (pointSlot <$> LedgerDB.currentPoint ldb) >>= \case
      NotOrigin slot -> do
        LedgerDB.takeSnapshotNOW internal LedgerDB.TakeAtVolatileTip (Just "db-analyser")
        traceWith tracer $ SnapshotStoredEvent slot
      Origin -> pure ()

countBlocks ::
  forall blk.
  HasAnalysis blk =>
  Analysis blk StartFromPoint
countBlocks (AnalysisEnv{db, registry, startFrom, limit, tracer}) = do
  counted <- processAll db registry (GetPure ()) startFrom limit 0 process
  traceWith tracer $ CountedBlocksEvent counted
  pure $ Just $ ResultCountBlock counted
 where
  process :: Int -> () -> IO Int
  process count _ = pure $ count + 1

{-------------------------------------------------------------------------------
  Analysis: check for ledger state thunks every n blocks
-------------------------------------------------------------------------------}

checkNoThunksEvery ::
  forall blk.
  ( HasAnalysis blk
  , LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  , CanStowLedgerTables (LedgerState blk)
  ) =>
  Word64 ->
  Analysis blk StartFromLedgerState
checkNoThunksEvery
  nBlocks
  (AnalysisEnv{db, registry, startFrom, cfg, limit, leiosDb}) = do
    putStrLn $
      "Checking for thunks in each block where blockNo === 0 (mod " <> show nBlocks <> ")."
    void $ processAll db registry GetBlock startFrom limit () process
    pure Nothing
   where
    FromLedgerState ldb internal = startFrom

    process :: () -> blk -> IO ()
    process _ blk = do
      (oldLedger', newLedger) <- applyBlockAtTip leiosDb LedgerApply cfg ldb blk
      let newLedger' = applyDiffs oldLedger' newLedger
          bn = blockNo blk
      when (unBlockNo bn `mod` nBlocks == 0) $ do
        -- Check the new ledger state with new values stowed. This checks that
        -- the ledger has no thunks in their ledgerstate type.
        IOLike.evaluate (stowLedgerTables $ ledgerState newLedger') >>= checkNoThunks bn
        -- Check the new ledger state with diffs in the tables. This should
        -- catch any additional thunks in the diffs tables.
        IOLike.evaluate (ledgerState newLedger) >>= checkNoThunks bn
        -- Check the new ledger state with values in the ledger tables. This
        -- should catch any additional thunks in the values tables.
        IOLike.evaluate (ledgerState newLedger') >>= checkNoThunks bn

      LedgerDB.push internal newLedger
      LedgerDB.tryFlush ldb

    checkNoThunks :: NoThunksMK mk => BlockNo -> LedgerState blk mk -> IO ()
    checkNoThunks bn ls =
      noThunks ["--checkThunks"] ls >>= \case
        Nothing -> putStrLn $ show bn <> ": no thunks found."
        Just ti -> do
          putStrLn $ show bn <> ": thunks found."
          print ti

{-------------------------------------------------------------------------------
  Analysis: maintain a ledger state and issue trace markers at appropriate
  points in the epoch
-------------------------------------------------------------------------------}

traceLedgerProcessing ::
  forall blk.
  ( HasAnalysis blk
  , LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  ) =>
  Analysis blk StartFromLedgerState
traceLedgerProcessing
  (AnalysisEnv{db, registry, startFrom, cfg, limit, leiosDb}) = do
    void $ processAll db registry GetBlock startFrom limit () process
    pure Nothing
   where
    FromLedgerState ldb internal = startFrom

    process ::
      () ->
      blk ->
      IO ()
    process _ blk = do
      (oldLedger, newLedger) <- applyBlockAtTip leiosDb LedgerApply cfg ldb blk
      let newLedger' = applyDiffs oldLedger newLedger
          traces =
            ( HasAnalysis.emitTraces $
                HasAnalysis.WithLedgerState blk (ledgerState oldLedger) (ledgerState newLedger')
            )
      mapM_ Debug.traceMarkerIO traces

      LedgerDB.push internal newLedger
      LedgerDB.tryFlush ldb

{-------------------------------------------------------------------------------
  Analysis: maintain a ledger state and time the five major ledger calculations
  for each block:

  0. Forecast.
  1. Header tick.
  2. Header application.
  3. Block tick.
  4. Block application.

  We focus on these 5 operations because they are involved in:

  - Chain syncing.
  - Block forging.
  - Block validation.

-------------------------------------------------------------------------------}

benchmarkLedgerOps ::
  forall blk.
  ( LedgerSupportsProtocol blk
  , HasAnalysis blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  ) =>
  Maybe FilePath ->
  LedgerApplicationMode ->
  Analysis blk StartFromLedgerState
benchmarkLedgerOps mOutfile ledgerAppMode AnalysisEnv{db, registry, startFrom, cfg, limit, leiosDb} = do
  -- We default to CSV when the no output file is provided (and thus the results are output to stdout).
  outFormat <- F.getOutputFormat mOutfile

  withFile mOutfile $ \outFileHandle -> do
    F.writeMetadata outFileHandle outFormat ledgerAppMode
    F.writeHeader outFileHandle outFormat

    void $
      processAll
        db
        registry
        ((,) <$> GetBlock <*> GetBlockSize)
        startFrom
        limit
        ()
        (process initLedger initial outFileHandle outFormat)
    pure Nothing
 where
  ccfg = topLevelConfigProtocol cfg
  lcfg = topLevelConfigLedger cfg

  FromLedgerState initLedger initial = startFrom

  process ::
    LedgerDB.LedgerDB' IO blk ->
    LedgerDB.TestInternals' IO blk ->
    IO.Handle ->
    F.OutputFormat ->
    () ->
    (blk, SizeInBytes) ->
    IO ()
  process ledgerDB intLedgerDB outFileHandle outFormat _ (blk, sz) = do
    prevRtsStats <- GC.getRTSStats
    ( prevLedgerState
      , closureTxs
      , ebTxsBytes
      , tables
      , tEbReadMut
      , tEbReadElapsed
      , tTableReadMut
      , tTableReadElapsed
      ) <-
      LedgerDB.withTipForker ledgerDB $ \frk -> do
        st <- IOLike.atomically $ LedgerDB.forkerGetLedgerState frk
        -- Verify the certificate before the read of the EB that it names, which
        -- is where 'LedgerDB.applyBlock' verifies it. The closure below applies
        -- without validation, so the certificate is the only check that the EB
        -- txs get in this pass.
        --
        -- The check runs inside the RTS-stats window, so its cost lands in
        -- 'DP.totalTime' and 'DP.mut'. It has no column of its own.
        case ledgerAppMode of
          LedgerReapply -> pure ()
          LedgerApply -> case verifyCertRb st blk of
            Left err ->
              fail $
                "benchmark doesn't support invalid certificates: "
                  <> show rp
                  <> " "
                  <> show err
            Right () -> pure ()
        -- A cert-RB has an empty body. It applies the txs of the EB that its
        -- parent announced. Read those txs from the LeiosDb. A block that
        -- certifies no EB skips the read, so its EB figures stay 0. A 'clock'
        -- around a read of nothing reports the cost of the two RTS-stats calls.
        ((txs, txsBytes), ebReadMut, ebReadElapsed) <-
          case certifiedEbHash (parentAnnouncement st) blk of
            Nothing -> pure (([], 0), 0, 0)
            Just ebHash -> clock $ readEbClosure leiosDb ebHash
        -- Read the values that the block and those txs consume.
        (tbs, tableReadMut, tableReadElapsed) <-
          clock $ LedgerDB.forkerReadTables frk (closureKeySets txs <> getBlockKeySets blk)
        pure (st, txs, txsBytes, tbs, ebReadMut, ebReadElapsed, tableReadMut, tableReadElapsed)

    -- Apply the EB closure on the parent state, as Forker.applyBlock does.
    -- 'applyLeiosClosure' needs an unticked state, so this runs before the tick
    -- below. For a block that carries no certificate, 'applyClosure' returns the
    -- parent state and its tables unchanged.
    (ClosureApplied{caStateAfterEb, caTablesAfterEb, caClosureDiff}, tEbApply) <-
      time $ applyClosure lcfg closureTxs prevLedgerState tables

    let slot = blockSlot blk
    -- We do not use strictness annotation on the resulting tuples since
    -- 'time' takes care of forcing the evaluation of its argument's result.
    (ldgrView, tForecast) <- time $ forecast slot caStateAfterEb
    (tkHdrSt, tHdrTick) <- time $ tickTheHeaderState slot caStateAfterEb ldgrView
    (!newHeader, tHdrApp) <- time $ applyTheHeader ldgrView tkHdrSt
    (tkLdgrSt, tBlkTick) <- time $ tickTheLedgerState slot caStateAfterEb
    let !tkLdgrSt' = applyDiffs (caStateAfterEb `withLedgerTables` caTablesAfterEb) tkLdgrSt
    (!newLedger, tBlkApp) <- time $ applyTheBlock tkLdgrSt'

    currentRtsStats <- GC.getRTSStats
    let
      currentMinusPrevious :: Num a => (GC.RTSStats -> a) -> a
      currentMinusPrevious f = f currentRtsStats - f prevRtsStats
      major_gcs = currentMinusPrevious GC.major_gcs
      -- The size of the body of the EB that this block certifies. The parent's
      -- announcement carries it, and 'closureTxs' is empty for a block that
      -- certifies no EB, so it stands for the same decision that
      -- 'readCertifiedClosure' made.
      ebBytes
        | null closureTxs = 0
        | otherwise = maybe 0 snd (parentAnnouncement prevLedgerState)
      slotDataPoint =
        DP.SlotDataPoint
          { DP.slot = realPointSlot rp
          , DP.slotGap = slot `slotCount` getTipSlot caStateAfterEb
          , DP.totalTime = currentMinusPrevious GC.elapsed_ns `div` 1000
          , DP.mut = currentMinusPrevious GC.mutator_elapsed_ns `div` 1000
          , DP.gc = currentMinusPrevious GC.gc_elapsed_ns `div` 1000
          , DP.tableReadTime = tTableReadElapsed `div` 1000
          , DP.mut_tableRead = tTableReadMut `div` 1000
          , DP.ebReadTime = tEbReadElapsed `div` 1000
          , DP.mut_ebRead = tEbReadMut `div` 1000
          , DP.majGcCount = major_gcs
          , DP.minGcCount = currentMinusPrevious GC.gcs - major_gcs
          , DP.allocatedBytes = currentMinusPrevious GC.allocated_bytes
          , DP.mut_forecast = tForecast `div` 1000
          , DP.mut_headerTick = tHdrTick `div` 1000
          , DP.mut_headerApply = tHdrApp `div` 1000
          , DP.mut_blockTick = tBlkTick `div` 1000
          , -- The cert-RB and the EB it certifies are one logical block, so this
            -- covers the EB txs and the cert-RB body. The tick between the two
            -- applications has its own figure in 'DP.mut_blockTick'.
            DP.mut_blockApply = (tEbApply + tBlkApp) `div` 1000
          , DP.blockByteSize = getSizeInBytes sz
          , DP.ebByteSize = ebBytes
          , DP.ebTxsByteSize = ebTxsBytes
          , DP.ebNumTxs = fromIntegral (length closureTxs)
          , DP.blockStats = DP.BlockStats $ HasAnalysis.blockStats blk
          }

      slotCount (SlotNo i) = \case
        Slotting.Origin -> i
        Slotting.At (SlotNo j) -> i - j

    F.writeDataPoint outFileHandle outFormat slotDataPoint

    -- The EB txs apply before the tick, so their diff goes first. Without it the
    -- LedgerDB never receives the EB's change and the next blocks read stale
    -- tables.
    LedgerDB.push intLedgerDB $
      ExtLedgerState
        (maybe id prependDiffs caClosureDiff (prependDiffs tkLdgrSt newLedger))
        newHeader
    LedgerDB.tryFlush ledgerDB
   where
    rp = blockRealPoint blk

    clock act = do
      let dup x = (x, x)
      (tMutPrev, tPrev) <- bimap GC.mutator_elapsed_ns GC.elapsed_ns . dup <$> GC.getRTSStats
      !r <- act
      (tMutNow, tNow) <- bimap GC.mutator_elapsed_ns GC.elapsed_ns . dup <$> GC.getRTSStats
      pure (r, tMutNow - tMutPrev, tNow - tPrev)

    -- Compute how many nanoseconds the mutator used from the last
    -- recorded 'elapsedTime' till the end of the execution of the given
    -- action. This function forces the evaluation of its argument's
    -- result.
    time act = (\(r, tMut, _) -> (r, tMut)) <$> clock act

    forecast ::
      SlotNo ->
      ExtLedgerState blk mk ->
      IO (LedgerView (BlockProtocol blk))
    forecast slot st = do
      let forecaster = ledgerViewForecastAt lcfg (ledgerState st)
      case runExcept $ forecastFor forecaster slot of
        Left err ->
          fail $ "benchmark doesn't support headers beyond the forecast limit: " <> show rp <> " " <> show err
        Right x -> pure x

    tickTheHeaderState ::
      SlotNo ->
      ExtLedgerState blk mk ->
      LedgerView (BlockProtocol blk) ->
      IO (Ticked (HeaderState blk))
    tickTheHeaderState slot st ledgerView =
      pure $!
        tickHeaderState
          ccfg
          ledgerView
          slot
          (headerState st)

    applyTheHeader ::
      LedgerView (BlockProtocol blk) ->
      Ticked (HeaderState blk) ->
      IO (HeaderState blk)
    applyTheHeader ledgerView tickedHeaderState = case ledgerAppMode of
      LedgerApply ->
        case runExcept $ validateHeader cfg ledgerView (getHeader blk) tickedHeaderState of
          Left err -> fail $ "benchmark doesn't support invalid headers: " <> show rp <> " " <> show err
          Right x -> pure x
      LedgerReapply ->
        pure $! revalidateHeader cfg ledgerView (getHeader blk) tickedHeaderState

    tickTheLedgerState ::
      SlotNo ->
      ExtLedgerState blk EmptyMK ->
      IO (Ticked (LedgerState blk) DiffMK)
    tickTheLedgerState slot st =
      pure $ applyChainTick OmitLedgerEvents lcfg slot (ledgerState st)

    applyTheBlock ::
      TickedLedgerState blk ValuesMK ->
      IO (LedgerState blk DiffMK)
    applyTheBlock tickedLedgerSt = case ledgerAppMode of
      LedgerApply ->
        case runExcept (lrResult <$> applyBlockLedgerResult OmitLedgerEvents lcfg blk tickedLedgerSt) of
          Left err -> fail $ "benchmark doesn't support invalid blocks: " <> show rp <> " " <> show err
          Right x -> pure x
      LedgerReapply ->
        pure $! lrResult $ reapplyBlockLedgerResult OmitLedgerEvents lcfg blk tickedLedgerSt

withFile :: Maybe FilePath -> (IO.Handle -> IO r) -> IO r
withFile (Just outfile) = IO.withFile outfile IO.WriteMode
withFile Nothing = \f -> f IO.stdout

{-------------------------------------------------------------------------------
  Analysis: trace ledger state metrics
-------------------------------------------------------------------------------}

getBlockApplicationMetrics ::
  forall blk.
  ( HasAnalysis blk
  , LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  ) =>
  NumberOfBlocks -> Maybe FilePath -> Analysis blk StartFromLedgerState
getBlockApplicationMetrics (NumberOfBlocks nrBlocks) mOutFile env = do
  withFile mOutFile $ \outFileHandle -> do
    writeHeaderLine outFileHandle separator (HasAnalysis.blockApplicationMetrics @blk)
    void $
      processAll db registry GetBlock startFrom limit () (process outFileHandle)
    pure Nothing
 where
  separator = ", "

  AnalysisEnv{db, registry, startFrom, cfg, limit, leiosDb} = env
  FromLedgerState ldb intLedgerDB = startFrom

  process ::
    IO.Handle ->
    () ->
    blk ->
    IO ()
  process outFileHandle _ blk = do
    (oldLedger, nextLedgerSt) <- applyBlockAtTip leiosDb LedgerReapply cfg ldb blk
    when (unBlockNo (blockNo blk) `mod` nrBlocks == 0) $ do
      let blockApplication =
            HasAnalysis.WithLedgerState
              blk
              (ledgerState oldLedger)
              (ledgerState $ applyDiffs oldLedger nextLedgerSt)

      computeAndWriteLine
        outFileHandle
        separator
        (HasAnalysis.blockApplicationMetrics @blk)
        blockApplication

      IO.hFlush outFileHandle

    LedgerDB.push intLedgerDB nextLedgerSt
    LedgerDB.tryFlush ldb

    pure ()

{-------------------------------------------------------------------------------
  Analysis: reforge the blocks, via the mempool
-------------------------------------------------------------------------------}

data ReproMempoolForgeHowManyBlks = ReproMempoolForgeOneBlk | ReproMempoolForgeTwoBlks

-- | Mempool capacity override for 'reproMempoolForge' on a Leios chain.
--
-- 'Mempool.computeMempoolCapacity' reads this as a number of blocks, not as a
-- byte budget: it divides by the ranking-block capacity and rounds up. So the
-- value only has to exceed one EB closure, which is what two closures give.
--
-- 'LeiosDemoTypes' exported a constant for this until the node stopped
-- overriding the mempool capacity, so the tool now owns it.
leiosMempoolSize :: LedgerSupportsMempool.ByteSize32
leiosMempoolSize = maxEBClosureSize <> maxEBClosureSize

reproMempoolForge ::
  forall blk.
  ( HasAnalysis blk
  , LedgerSupportsMempool.HasTxId (LedgerSupportsMempool.GenTx blk)
  , LedgerSupportsMempool.HasTxs blk
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  , HasLeiosVoting blk
  ) =>
  Int ->
  Analysis blk StartFromLedgerState
reproMempoolForge numBlks env = do
  howManyBlocks <- case numBlks of
    1 -> pure ReproMempoolForgeOneBlk
    2 -> pure ReproMempoolForgeTwoBlks
    _ ->
      fail $
        "--repro-mempool-and-forge only supports"
          <> "1 or 2 blocks at a time, not "
          <> show numBlks

  mempool <-
    Mempool.openMempoolWithoutSyncThread
      Mempool.LedgerInterface
        { Mempool.getCurrentLedgerState = do
            st <- LedgerDB.getVolatileTip ledgerDB
            pure $
              MempoolLedgerDBView
                (ledgerState st)
                ( fmap (fmap LedgerDB.ledgerStateReadOnlyForker) $
                    LedgerDB.openReadOnlyForker ledgerDB (SpecificPoint (castPoint $ getTip st))
                )
        }
      lCfg
      -- This pass models the mempool of a forging node, so the capacity must
      -- match. A Leios node holds a whole EB closure, and a closure reaches
      -- 12 MB ('maxEBClosureSize'). One mebibyte is not enough.
      --
      -- The capacity is a ceiling, not a target. This pass adds the
      -- transactions of one or two blocks, then it flushes them after each
      -- block. So a larger capacity does not change the measured cost.
      (Mempool.MempoolCapacityBytesOverride leiosMempoolSize)
      (Nothing :: Maybe Mempool.MempoolTimeoutConfig)
      nullTracer

  -- The EB that the block at the start point announced, or 'Nothing' if that
  -- block announced no EB. The stream starts after that block, so that block is
  -- the predecessor of the first block in the fold.
  seedAnnouncement <- announcementAtPoint db =<< startFromPoint startFrom

  void $
    processAll db registry GetBlock startFrom limit Nothing $
      process howManyBlocks mempool seedAnnouncement
  pure Nothing
 where
  AnalysisEnv
    { cfg
    , startFrom = startFrom@(FromLedgerState ledgerDB intLedgerDB)
    , db
    , registry
    , limit
    , tracer
    , leiosDb
    } = env

  lCfg :: LedgerConfig blk
  lCfg = configLedger cfg

  timed :: IO a -> IO (a, IOLike.DiffTime, Int64, Int64)
  timed m = do
    before <- IOLike.getMonotonicTime
    prevRtsStats <- GC.getRTSStats
    !x <- m
    newRtsStats <- GC.getRTSStats
    after <- IOLike.getMonotonicTime
    pure
      ( x
      , after `IOLike.diffTime` before
      , (GC.mutator_elapsed_ns newRtsStats - GC.mutator_elapsed_ns prevRtsStats) `div` 1000
      , (GC.gc_elapsed_ns newRtsStats - GC.gc_elapsed_ns prevRtsStats) `div` 1000
      )

  process ::
    ReproMempoolForgeHowManyBlks ->
    Mempool.Mempool IO blk ->
    -- The EB that the block at the start point announced, if it announced one
    Maybe (LeiosPoint, BytesSize) ->
    Maybe blk ->
    blk ->
    IO (Maybe blk)
  process howManyBlocks mempool seedAnnouncement mbBlk blk' =
    (\() -> Just blk') <$> do
      -- add this block's transactions to the mempool
      do
        -- A block that certifies an EB has an empty body, so 'extractTxs'
        -- returns nothing for such a block. The forge thread of such a block
        -- held the transactions of the EB in its mempool. It took them from
        -- there at the announcing slot, and it dropped them when it applied the
        -- certificate. Add them here, so that the mempool of this pass holds
        -- what the mempool of the forge thread held.
        --
        -- 'mbBlk' is the predecessor of 'blk'', because the blocks arrive in
        -- chain order.
        let prevAnnouncement = case mbBlk of
              -- The fold never sees the predecessor of the first streamed
              -- block, so the seed gives its announcement.
              Nothing -> seedAnnouncement
              Just prevBlk -> headerLeiosAnnouncement (getHeader prevBlk)
        ebTxs <- case certifiedEbHash prevAnnouncement blk' of
          Nothing -> pure []
          Just ebHash -> fst <$> readEbClosure leiosDb ebHash
        results <-
          Mempool.addTxs mempool $ LedgerSupportsMempool.extractTxs blk' <> ebTxs
        let rejs =
              [ (LedgerSupportsMempool.txId tx, rej)
              | rej@(Mempool.MempoolTxRejected tx _) <- results
              ]
        unless (null rejs) $ do
          fail $
            unlines $
              ["Mempool rejected some of the on-chain txs: " <> show rejs]
                <> case howManyBlocks of
                  ReproMempoolForgeOneBlk -> []
                  ReproMempoolForgeTwoBlks ->
                    [ "This might be expected, see the db-analyser README."
                    , "Consider trying again with `--repro-mempool-and-forge 1`."
                    ]

      let scrutinee = case howManyBlocks of
            ReproMempoolForgeOneBlk -> Just blk'
            ReproMempoolForgeTwoBlks -> mbBlk
      case scrutinee of
        Nothing -> pure ()
        Just blk -> do
          LedgerDB.withTipForker ledgerDB $ \forker -> do
            st <- IOLike.atomically $ LedgerDB.forkerGetLedgerState forker

            -- A block that certifies an EB has an empty body. The transactions
            -- that such a block puts on the chain are the transactions of the
            -- EB, and the LeiosDb holds them. The forge thread of such a block
            -- reads that closure, so time the read. A block that certifies no
            -- EB skips the read, and its EB figures stay 0.
            --
            -- 'timed' forces its result to WHNF only, and
            -- 'resolveLeiosClosure' deserialises each transaction lazily. So
            -- this figure covers the two LeiosDb queries, and not the
            -- deserialisation.
            let mCertifiedEb = certifiedEbHash (parentAnnouncement st) blk
            ((closureTxs, ebTxsBytes), durEbRead, mutEbRead, gcEbRead) <-
              case mCertifiedEb of
                Nothing -> pure (([], 0), 0, 0, 0)
                Just ebHash -> timed $ readEbClosure leiosDb ebHash

            -- The forge thread of a certifying block reads the ledger
            -- values that the txs of the certified EB consume. Then
            -- it applies the closure of that EB to 'st' (the ledger
            -- state at the parent block) before the tick. Last it
            -- ticks the result.
            --
            -- 'resolveAndApplyLeiosClosure' makes the value read and the
            -- application. This pass measures each one, so each one has its own
            -- columns. 'applyClosure' forces every tx. Its figure holds the
            -- deserialisation that the LeiosDb read above left as thunks.
            --
            -- On a block that certifies no EB the closure is empty. The value
            -- read then asks for no keys. 'applyClosure' returns 'st' unchanged.
            (tables, durEbTableRead, mutEbTableRead, gcEbTableRead) <-
              timed $ LedgerDB.forkerReadTables forker (closureKeySets closureTxs)
            (ClosureApplied{caStateAfterEb, caClosureDiff}, durEbApply, mutEbApply, gcEbApply) <-
              timed $ applyClosure lCfg closureTxs st tables

            -- time the suspected slow parts of the forge thread that created
            -- this block
            --
            -- Primary caveat: that thread's mempool may have had more transactions in it.
            let slot = blockSlot blk
            -- The tick runs on the state after the EB, and the diff of the
            -- closure goes in front of the diff of the tick. Both diffs are
            -- then relative to the parent, which is where 'forkerReadTables'
            -- below reads its values.
            (ticked, durTick, mutTick, gcTick) <-
              timed $
                IOLike.evaluate $
                  maybe id prependDiffs caClosureDiff $
                    applyChainTick OmitLedgerEvents lCfg slot (ledgerState caStateAfterEb)
            -- The forge thread of a certifying block calls
            -- 'getSnapshotForNoCache'. See 'partitionMempool' in
            -- Ouroboros.Consensus.NodeKernel.Forge, which makes both calls:
            -- 'getSnapshotForNoCache' on the certifying branch, and
            -- 'getSnapshotFor' on the other one.
            --
            -- Applying the EB closure changes neither
            -- the tip hash nor the slot, so 'getSnapshotFor' treats its cache
            -- as valid: it reuses the cached values, and it returns the cached
            -- snapshot outright when the slot matches. Pick the call that the
            -- forge thread makes.
            let getSnapshot = case mCertifiedEb of
                  Nothing -> Mempool.getSnapshotFor
                  Just{} -> Mempool.getSnapshotForNoCache
            ((), durSnap, mutSnap, gcSnap) <- timed $ do
              snap <-
                getSnapshot mempool slot ticked $
                  fmap castLedgerTables . LedgerDB.forkerReadTables forker . castLedgerTables

              pure $ length (Mempool.snapshotTxs snap) `seq` Mempool.snapshotStateHash snap `seq` ()

            let sizes = HasAnalysis.blockTxSizes blk
            traceWith tracer $
              BlockMempoolAndForgeRepro
                (blockNo blk)
                slot
                MempoolAndForgeRepro
                  { numTxs = length sizes
                  , txsSize = sum sizes
                  , ebNumTxs = length closureTxs
                  , ebTxsByteSize = SizeInBytes ebTxsBytes
                  , durEbRead
                  , mutEbRead
                  , gcEbRead
                  , durEbTableRead
                  , mutEbTableRead
                  , gcEbTableRead
                  , durEbApply
                  , mutEbApply
                  , gcEbApply
                  , durTick
                  , mutTick
                  , gcTick
                  , durSnap
                  , mutSnap
                  , gcSnap
                  }

          -- advance the ledger state to include this block
          --
          -- 'applyBlockAtTip' applies the EB closure on a certifying block, and
          -- on that block alone.
          --
          -- TODO We could inline/reuse parts of the IsLedger ExtLedgerState
          -- instance here as an optimization that avoids repeating the
          -- 'applyChainTick' call above. We want to leave that call alone, though,
          -- since it currently matches the call in the forging thread, which is
          -- the primary intention of this Analysis. Maybe GHC's CSE is already
          -- doing this sharing optimization?
          (_, nextLedgerSt) <- applyBlockAtTip leiosDb LedgerReapply cfg ledgerDB blk
          LedgerDB.push intLedgerDB nextLedgerSt
          LedgerDB.tryFlush ledgerDB

          -- this flushes blk from the mempool, since every tx in it is now on the chain
          void $ Mempool.testSyncWithLedger mempool

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the DB
-------------------------------------------------------------------------------}

decreaseLimit :: Limit -> Maybe Limit
decreaseLimit Unlimited = Just Unlimited
decreaseLimit (Limit 0) = Nothing
decreaseLimit (Limit n) = Just . Limit $ n - 1

data NextStep = Continue | Stop

processAllUntil ::
  forall blk b startFrom st.
  (HasHeader blk, HasAnnTip blk) =>
  ImmutableDB IO blk ->
  ResourceRegistry IO ->
  BlockComponent blk b ->
  AnalysisStartFrom IO blk startFrom ->
  Limit ->
  st ->
  (st -> b -> IO (NextStep, st)) ->
  IO st
processAllUntil immutableDB registry blockComponent startFrom limit initState callback = do
  st <- startFromPoint startFrom
  itr <-
    ImmutableDB.streamAfterKnownPoint
      immutableDB
      registry
      blockComponent
      st
  go itr limit initState
 where
  go :: ImmutableDB.Iterator IO blk b -> Limit -> st -> IO st
  go itr lt !st = case decreaseLimit lt of
    Nothing -> return st
    Just decreasedLimit -> do
      itrResult <- ImmutableDB.iteratorNext itr
      case itrResult of
        ImmutableDB.IteratorExhausted -> return st
        ImmutableDB.IteratorResult b ->
          callback st b >>= \case
            (Continue, nst) -> go itr decreasedLimit nst
            (Stop, nst) -> return nst

processAll ::
  forall blk b startFrom st.
  (HasHeader blk, HasAnnTip blk) =>
  ImmutableDB IO blk ->
  ResourceRegistry IO ->
  BlockComponent blk b ->
  AnalysisStartFrom IO blk startFrom ->
  Limit ->
  st ->
  (st -> b -> IO st) ->
  IO st
processAll db rr blockComponent startFrom limit initSt cb =
  processAllUntil db rr blockComponent startFrom limit initSt callback
 where
  callback st b = (Continue,) <$> cb st b

processAll_ ::
  forall blk b startFrom.
  (HasHeader blk, HasAnnTip blk) =>
  ImmutableDB IO blk ->
  ResourceRegistry IO ->
  BlockComponent blk b ->
  AnalysisStartFrom IO blk startFrom ->
  Limit ->
  (b -> IO ()) ->
  IO ()
processAll_ db registry blockComponent startFrom limit callback =
  processAll db registry blockComponent startFrom limit () (const callback)
