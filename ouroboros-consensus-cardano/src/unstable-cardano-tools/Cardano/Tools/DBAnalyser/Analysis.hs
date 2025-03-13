{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
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

module Cardano.Tools.DBAnalyser.Analysis (
    AnalysisEnv (..)
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

import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.PoolDistr as SL
import qualified Cardano.Slotting.Slot as Slotting
import qualified Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.FileWriting as F
import qualified Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint as DP
import           Cardano.Tools.DBAnalyser.CSV (computeAndWriteLine,
                     writeHeaderLine)
import           Cardano.Tools.DBAnalyser.HasAnalysis (HasAnalysis)
import qualified Cardano.Tools.DBAnalyser.HasAnalysis as HasAnalysis
import           Cardano.Tools.DBAnalyser.Types
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad (unless, void, when)
import           Control.Monad.Except (runExcept)
import           Control.ResourceRegistry
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Data.Int (Int64)
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Singletons
import           Data.Word (Word16, Word32, Word64)
import qualified Debug.Trace as Debug
import qualified GHC.Stats as GC
import           NoThunks.Class (noThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast (forecastFor)
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..),
                     HeaderState (..), headerStatePoint, revalidateHeader,
                     tickHeaderState, validateHeader)
import           Ouroboros.Consensus.Ledger.Abstract
                     (ApplyBlock (reapplyBlockLedgerResult), LedgerCfg,
                     LedgerConfig, applyBlockLedgerResult, applyChainTick,
                     tickThenApply, tickThenApplyLedgerResult, tickThenReapply)
import           Ouroboros.Consensus.Ledger.Basics (LedgerResult (..),
                     LedgerState, getTipSlot)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
                     (LedgerSupportsMempool)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol (..))
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Protocol.Abstract (LedgerView)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB
                     (LgrDbSerialiseConstraints)
import           Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB (DiskSnapshot (..),
                     writeSnapshot)
import           Ouroboros.Consensus.Storage.Serialisation (encodeDisk)
import           Ouroboros.Consensus.Util (Flag (..), (..:))
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import           Ouroboros.Network.SizeInBytes
import           System.FS.API (SomeHasFS (..))
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
     , LgrDbSerialiseConstraints blk
     )
  => AnalysisName -> SomeAnalysis blk
runAnalysis analysisName = case go analysisName of
    SomeAnalysis p analysis -> SomeAnalysis p $ \env@AnalysisEnv{ tracer } -> do
      traceWith tracer (StartedEvent analysisName)
      result <- analysis env
      traceWith tracer DoneEvent
      pure result
  where
    go :: AnalysisName -> SomeAnalysis blk
    go ShowSlotBlockNo                                   = mkAnalysis $ showSlotBlockNo
    go CountTxOutputs                                    = mkAnalysis $ countTxOutputs
    go ShowBlockHeaderSize                               = mkAnalysis $ showHeaderSize
    go ShowBlockTxsSize                                  = mkAnalysis $ showBlockTxsSize
    go ShowEBBs                                          = mkAnalysis $ showEBBs
    go OnlyValidation                                    = mkAnalysis @StartFromPoint $ \_ -> pure Nothing
    go (StoreLedgerStateAt slotNo lgrAppMode doChecksum) = mkAnalysis $ storeLedgerStateAt slotNo lgrAppMode doChecksum
    go CountBlocks                                       = mkAnalysis $ countBlocks
    go (CheckNoThunksEvery nBks)                         = mkAnalysis $ checkNoThunksEvery nBks
    go TraceLedgerProcessing                             = mkAnalysis $ traceLedgerProcessing
    go (ReproMempoolAndForge nBks)                       = mkAnalysis $ reproMempoolForge nBks
    go (BenchmarkLedgerOps mOutfile lgrAppMode)          = mkAnalysis $ benchmarkLedgerOps mOutfile lgrAppMode
    go (GetBlockApplicationMetrics nrBlocks mOutfile)    = mkAnalysis $ getBlockApplicationMetrics nrBlocks mOutfile
    go DumpStakeDistributions                            = mkAnalysis $ dumpStakeDistributions

    mkAnalysis ::
         forall startFrom. SingI startFrom
      => Analysis blk startFrom -> SomeAnalysis blk
    mkAnalysis = SomeAnalysis (Proxy @startFrom)

type Analysis blk startFrom = AnalysisEnv IO blk startFrom -> IO (Maybe AnalysisResult)

data SomeAnalysis blk =
       forall startFrom. SingI startFrom
    => SomeAnalysis (Proxy startFrom) (Analysis blk startFrom)

data AnalysisEnv m blk startFrom = AnalysisEnv {
      cfg        :: TopLevelConfig blk
    , startFrom  :: AnalysisStartFrom blk startFrom
    , db         :: ImmutableDB IO blk
    , registry   :: ResourceRegistry IO
    , ledgerDbFS :: SomeHasFS IO
    , limit      :: Limit
    , tracer     :: Tracer m (TraceEvent blk)
    }

-- | Whether the db-analyser pass needs access to a ledger state.
data StartFrom = StartFromPoint | StartFromLedgerState

data SStartFrom startFrom where
  SStartFromPoint       :: SStartFrom StartFromPoint
  SStartFromLedgerState :: SStartFrom StartFromLedgerState

type instance Sing = SStartFrom
instance SingI StartFromPoint       where sing = SStartFromPoint
instance SingI StartFromLedgerState where sing = SStartFromLedgerState

data AnalysisStartFrom blk startFrom where
  FromPoint ::
    Point blk -> AnalysisStartFrom blk StartFromPoint
  FromLedgerState ::
    ExtLedgerState blk -> AnalysisStartFrom blk StartFromLedgerState

startFromPoint :: HasAnnTip blk => AnalysisStartFrom blk startFrom -> Point blk
startFromPoint = \case
  FromPoint pt       -> pt
  FromLedgerState st -> headerStatePoint $ headerState st

data TraceEvent blk =
    StartedEvent AnalysisName
    -- ^ triggered when given analysis has started
  | DoneEvent
    -- ^ triggered when analysis has ended
  | BlockSlotEvent BlockNo SlotNo (HeaderHash blk)
    -- ^ triggered when block has been found, it holds:
    --   * block's number
    --   * slot number when the block was forged
  | CountTxOutputsEvent BlockNo SlotNo Int Int
    -- ^ triggered when block has been found, it holds:
    --   * block's number
    --   * slot number when the block was forged
    --   * cumulative tx output
    --   * count tx output
  | EbbEvent (HeaderHash blk) (ChainHash blk) Bool
    -- ^ triggered when EBB block has been found, it holds:
    --   * its hash,
    --   * hash of previous block
    --   * flag whether the EBB is known
  | CountedBlocksEvent Int
    -- ^ triggered once during CountBLocks analysis,
    --   when blocks were counted
  | HeaderSizeEvent BlockNo SlotNo Word16 Word32
    -- ^ triggered when header size has been measured
    --   * block's number
    --   * slot number when the block was forged
    --   * block's header size
    --   * block's size
  | MaxHeaderSizeEvent Word16
    -- ^ triggered once during ShowBlockTxsSize analysis,
    --   holding maximum encountered header size
  | SnapshotStoredEvent SlotNo
    -- ^ triggered when snapshot of ledger has been stored for SlotNo
  | SnapshotWarningEvent SlotNo SlotNo
    -- ^ triggered once during  StoreLedgerStateAt analysis,
    --   when snapshot was created in slot proceeding the
    --   requested one
  | LedgerErrorEvent (Point blk) (ExtValidationError blk)
    -- ^ triggered when applying a block with the given point failed
  | BlockTxSizeEvent SlotNo Int SizeInBytes
    -- ^ triggered for all blocks during ShowBlockTxsSize analysis,
    --   it holds:
    --   * slot number when the block was forged
    --   * number of transactions in the block
    --   * total size of transactions in the block
  | BlockMempoolAndForgeRepro BlockNo SlotNo Int SizeInBytes IOLike.DiffTime Int64 Int64 IOLike.DiffTime Int64 Int64
    -- ^ triggered for all blocks during MempoolAndForgeRepro analysis,
    --   it holds:
    --   * block number
    --   * slot number when the block was forged
    --   * number of transactions in the block
    --   * total size of transactions in the block
    --   * monotonic time to tick ledger state
    --   * total time spent in the mutator when ticking the ledger state
    --   * total time spent in gc when ticking the ledger state
    --   * monotonic time to call 'Mempool.getSnapshotFor'
    --   * total time spent in the mutator when calling 'Mempool.getSnapshotFor'
    --   * total time spent in gc when calling 'Mempool.getSnapshotFor'
  | DumpStakeDistribution EpochNo (SL.PoolDistr StandardCrypto)

instance (HasAnalysis blk, LedgerSupportsProtocol blk) => Show (TraceEvent blk) where
  show (StartedEvent analysisName)        = "Started " <> (show analysisName)
  show DoneEvent                          = "Done"
  show (BlockSlotEvent bn sn h)           = intercalate "\t" $ [
      show bn
    , show sn
    , show h
    ]
  show (CountTxOutputsEvent bn sn cumulative count) = intercalate "\t" $ [
      show bn
    , show sn
    , "cumulative: " <> show cumulative
    , "count: " <> show count
    ]
  show (EbbEvent ebb previous known)      = intercalate "\t" [
      "EBB: "   <> show ebb
    , "Prev: "  <> show previous
    , "Known: " <> show known
    ]
  show (CountedBlocksEvent counted)       = "Counted " <> show counted <> " blocks."
  show (HeaderSizeEvent bn sn hSz bSz)    = intercalate "\t" $ [
      show bn
    , show sn
    , "header size: " <> show hSz
    , "block size: "  <> show bSz
    ]
  show (MaxHeaderSizeEvent size)          =
    "Maximum encountered header size = " <> show size
  show (SnapshotStoredEvent slot)         =
    "Snapshot stored at " <> show slot
  show (SnapshotWarningEvent requested actual) =
    "Snapshot was created at " <> show actual <> " " <>
    "because there was no block forged at requested " <> show requested
  show (LedgerErrorEvent pt err) =
    "Applying block at " <> show pt <> " failed: " <> show err
  show (BlockTxSizeEvent slot numBlocks txsSize) = intercalate "\t" [
      show slot
    , "Num txs in block = " <> show numBlocks
    , "Total size of txs in block = " <> show txsSize
    ]
  show (BlockMempoolAndForgeRepro bno slot txsCount txsSize durTick mutTick gcTick durSnap mutSnap gcSnap) = intercalate "\t" [
      show bno
    , show slot
    , "txsCount " <> show txsCount
    , "txsSize " <> show txsSize
    , "durTick " <> show durTick
    , "mutTick " <> show mutTick
    , "gcTick " <> show gcTick
    , "durSnap " <> show durSnap
    , "mutSnap " <> show mutSnap
    , "gcSnap " <> show gcSnap
    ]
  show (DumpStakeDistribution eno pd) =
      intercalate "\t"
    $ (\ss -> show eno : show (SL.pdTotalActiveStake pd) : show (Map.size mp) : ss)
    $ [ show (keyhash, SL.individualTotalPoolStake x, SL.individualPoolStake x)
      | (keyhash, x) <- Map.assocs mp
      ]
    where
      mp = SL.unPoolDistr pd

{-------------------------------------------------------------------------------
  Analysis: show block and slot number and hash for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
showSlotBlockNo AnalysisEnv { db, registry, startFrom, limit, tracer } =
    processAll_ db registry GetHeader startFrom limit process
        >> pure Nothing
  where
    process :: Header blk -> IO ()
    process hdr = traceWith tracer $
        BlockSlotEvent (blockNo hdr) (blockSlot hdr) (headerHash hdr)

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
countTxOutputs AnalysisEnv { db, registry, startFrom, limit, tracer } = do
    void $ processAll db registry GetBlock startFrom limit 0 process
    pure Nothing
  where
    process :: Int -> blk -> IO Int
    process cumulative blk = do
        let cumulative' = cumulative + count
            event       = CountTxOutputsEvent (blockNo blk)
                                              (blockSlot blk)
                                              cumulative'
                                              count
        traceWith tracer event
        return cumulative'
      where
        count = HasAnalysis.countTxOutputs blk

{-------------------------------------------------------------------------------
  Analysis: show the header size in bytes for all blocks
-------------------------------------------------------------------------------}

showHeaderSize :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
showHeaderSize AnalysisEnv { db, registry, startFrom, limit, tracer } = do
    maxHeaderSize <-
      processAll db registry ((,,) <$> GetHeader <*> GetHeaderSize <*> GetBlockSize) startFrom limit 0 process
    traceWith tracer $ MaxHeaderSizeEvent maxHeaderSize
    pure $ Just $ ResultMaxHeaderSize maxHeaderSize
  where
    process :: Word16 -> (Header blk, Word16, SizeInBytes) -> IO Word16
    process maxHeaderSize (hdr, headerSize, blockSize) = do
      let event = HeaderSizeEvent (blockNo hdr)
                                  (blockSlot hdr)
                                   headerSize
                                  (getSizeInBytes blockSize)
      traceWith tracer event
      return $ maxHeaderSize `max` headerSize

{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

showBlockTxsSize :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
showBlockTxsSize AnalysisEnv { db, registry, startFrom, limit, tracer } = do
    processAll_ db registry GetBlock startFrom limit process
    pure Nothing
  where
    process :: blk -> IO ()
    process blk =
      traceWith tracer $ BlockTxSizeEvent (blockSlot blk) numBlockTxs blockTxsSize
      where
        txSizes :: [SizeInBytes]
        txSizes = HasAnalysis.blockTxSizes blk

        numBlockTxs :: Int
        numBlockTxs = length txSizes

        blockTxsSize :: SizeInBytes
        blockTxsSize = sum txSizes

{-------------------------------------------------------------------------------
  Analysis: show EBBs and their predecessors
-------------------------------------------------------------------------------}

showEBBs :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
showEBBs AnalysisEnv { db, registry, startFrom, limit, tracer } = do
    processAll_ db registry GetBlock startFrom limit process
    pure Nothing
  where
    process :: blk -> IO ()
    process blk =
        case blockIsEBB blk of
          Just _epoch -> do
            let known =  Map.lookup
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
     forall blk .
     ( LgrDbSerialiseConstraints blk
     , HasAnalysis blk
     , LedgerSupportsProtocol blk
     )
  => SlotNo
  -> LedgerApplicationMode
  -> Flag "DoDiskSnapshotChecksum"
  -> Analysis blk StartFromLedgerState
storeLedgerStateAt slotNo ledgerAppMode doChecksum env = do
    void $ processAllUntil db registry GetBlock startFrom limit initLedger process
    pure Nothing
  where
    AnalysisEnv { db, registry, startFrom, cfg, limit, ledgerDbFS, tracer } = env
    FromLedgerState initLedger = startFrom

    process :: ExtLedgerState blk -> blk -> IO (NextStep, ExtLedgerState blk)
    process oldLedger blk = do
      let ledgerCfg = ExtLedgerCfg cfg
      case runExcept $ tickThenXApply ledgerCfg blk oldLedger of
        Right newLedger -> do
          when (blockSlot blk >= slotNo) $ storeLedgerState newLedger
          when (blockSlot blk > slotNo) $ issueWarning blk
          when ((unBlockNo $ blockNo blk) `mod` 1000 == 0) $ reportProgress blk
          return (continue blk, newLedger)
        Left err -> do
          traceWith tracer $ LedgerErrorEvent (blockPoint blk) err
          storeLedgerState oldLedger
          pure (Stop, oldLedger)

    tickThenXApply = case ledgerAppMode of
        LedgerReapply -> pure ..: tickThenReapply
        LedgerApply   -> tickThenApply

    continue :: blk -> NextStep
    continue blk
      | blockSlot blk >= slotNo = Stop
      | otherwise               = Continue

    issueWarning blk   = let event = SnapshotWarningEvent slotNo (blockSlot blk)
                         in traceWith tracer event
    reportProgress blk = let event = BlockSlotEvent (blockNo blk) (blockSlot blk) (blockHash blk)
                         in traceWith tracer event

    storeLedgerState :: ExtLedgerState blk -> IO ()
    storeLedgerState ledgerState = case pointSlot pt of
        NotOrigin slot -> do
          let snapshot = DiskSnapshot (unSlotNo slot) (Just "db-analyser")
          writeSnapshot ledgerDbFS doChecksum encLedger snapshot ledgerState
          traceWith tracer $ SnapshotStoredEvent slot
        Origin -> pure ()
      where
        pt = headerStatePoint $ headerState ledgerState

    encLedger :: ExtLedgerState blk -> Encoding
    encLedger =
      let ccfg = configCodec cfg
      in encodeExtLedgerState
           (encodeDisk ccfg)
           (encodeDisk ccfg)
           (encodeDisk ccfg)

countBlocks ::
     forall blk .
     ( HasAnalysis blk
     )
  => Analysis blk StartFromPoint
countBlocks (AnalysisEnv { db, registry, startFrom, limit, tracer }) = do
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
  ( HasAnalysis blk,
    LedgerSupportsProtocol blk
  ) =>
  Word64 ->
  Analysis blk StartFromLedgerState
checkNoThunksEvery
  nBlocks
  (AnalysisEnv {db, registry, startFrom, cfg, limit}) = do
    putStrLn $
      "Checking for thunks in each block where blockNo === 0 (mod " <> show nBlocks <> ")."
    void $ processAll db registry GetBlock startFrom limit initLedger process
    pure Nothing
  where
    FromLedgerState initLedger = startFrom

    process :: ExtLedgerState blk -> blk -> IO (ExtLedgerState blk)
    process oldLedger blk = do
      let ledgerCfg     = ExtLedgerCfg cfg
          appliedResult = tickThenApplyLedgerResult ledgerCfg blk oldLedger
          newLedger     = either (error . show) lrResult $ runExcept $ appliedResult
          bn            = blockNo blk
      when (unBlockNo bn `mod` nBlocks == 0 ) $ IOLike.evaluate (ledgerState newLedger) >>= checkNoThunks bn
      return newLedger

    checkNoThunks :: BlockNo -> LedgerState blk -> IO ()
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
  ( HasAnalysis blk,
    LedgerSupportsProtocol blk
  ) =>
  Analysis blk StartFromLedgerState
traceLedgerProcessing
  (AnalysisEnv {db, registry, startFrom, cfg, limit}) = do
    void $ processAll db registry GetBlock startFrom limit initLedger process
    pure Nothing
  where
    FromLedgerState initLedger = startFrom

    process
      :: ExtLedgerState blk
      -> blk
      -> IO (ExtLedgerState blk)
    process oldLedger blk = do
      let ledgerCfg     = ExtLedgerCfg cfg
          appliedResult = tickThenApplyLedgerResult ledgerCfg blk oldLedger
          newLedger     = either (error . show) lrResult $ runExcept $ appliedResult
          traces        =
            (HasAnalysis.emitTraces $
              HasAnalysis.WithLedgerState blk (ledgerState oldLedger) (ledgerState newLedger))
      mapM_ Debug.traceMarkerIO traces
      return $ newLedger

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
     ( HasAnalysis blk
     , LedgerSupportsProtocol blk
     )
  => Maybe FilePath
  -> LedgerApplicationMode
  -> Analysis blk StartFromLedgerState
benchmarkLedgerOps mOutfile ledgerAppMode AnalysisEnv {db, registry, startFrom, cfg, limit} = do
    -- We default to CSV when the no output file is provided (and thus the results are output to stdout).
    outFormat <- F.getOutputFormat mOutfile

    withFile mOutfile $ \outFileHandle -> do
      F.writeMetadata outFileHandle outFormat ledgerAppMode
      F.writeHeader   outFileHandle outFormat

      void $ processAll
        db
        registry
        ((,) <$> GetBlock <*> GetBlockSize)
        startFrom
        limit
        initLedger
        (process outFileHandle outFormat)
      pure Nothing
  where
    ccfg = topLevelConfigProtocol cfg
    lcfg = topLevelConfigLedger   cfg

    FromLedgerState initLedger = startFrom

    process ::
         IO.Handle
      -> F.OutputFormat
      -> ExtLedgerState blk
      -> (blk, SizeInBytes)
      -> IO (ExtLedgerState blk)
    process outFileHandle outFormat prevLedgerState (blk, sz)  = do
        prevRtsStats <- GC.getRTSStats
        let
          -- Compute how many nanoseconds the mutator used from the last
          -- recorded 'elapsedTime' till the end of the execution of the given
          -- action. This function forces the evaluation of its argument's
          -- result.
          time act = do
              tPrev <- GC.mutator_elapsed_ns <$> GC.getRTSStats
              !r <- act
              tNow <- GC.mutator_elapsed_ns <$> GC.getRTSStats
              pure (r, tNow - tPrev)

        let slot = blockSlot      blk
        -- We do not use strictness annotation on the resulting tuples since
        -- 'time' takes care of forcing the evaluation of its argument's result.
        (ldgrView, tForecast) <- time $ forecast            slot prevLedgerState
        (tkHdrSt,  tHdrTick)  <- time $ tickTheHeaderState  slot prevLedgerState ldgrView
        (hdrSt',   tHdrApp)   <- time $ applyTheHeader                           ldgrView tkHdrSt
        (tkLdgrSt, tBlkTick)  <- time $ tickTheLedgerState  slot prevLedgerState
        (ldgrSt',  tBlkApp)   <- time $ applyTheBlock                                     tkLdgrSt

        currentRtsStats <- GC.getRTSStats
        let
          currentMinusPrevious :: Num a => (GC.RTSStats -> a) -> a
          currentMinusPrevious f = f currentRtsStats - f prevRtsStats
          major_gcs              = currentMinusPrevious GC.major_gcs
          slotDataPoint =
            DP.SlotDataPoint
            { DP.slot            = realPointSlot rp
            , DP.slotGap         = slot `slotCount` getTipSlot prevLedgerState
            , DP.totalTime       = currentMinusPrevious GC.elapsed_ns          `div` 1000
            , DP.mut             = currentMinusPrevious GC.mutator_elapsed_ns  `div` 1000
            , DP.gc              = currentMinusPrevious GC.gc_elapsed_ns       `div` 1000
            , DP.majGcCount      = major_gcs
            , DP.minGcCount      = currentMinusPrevious GC.gcs - major_gcs
            , DP.allocatedBytes  = currentMinusPrevious GC.allocated_bytes
            , DP.mut_forecast    = tForecast `div` 1000
            , DP.mut_headerTick  = tHdrTick  `div` 1000
            , DP.mut_headerApply = tHdrApp   `div` 1000
            , DP.mut_blockTick   = tBlkTick  `div` 1000
            , DP.mut_blockApply  = tBlkApp   `div` 1000
            , DP.blockByteSize   = getSizeInBytes sz
            , DP.blockStats      = DP.BlockStats $ HasAnalysis.blockStats blk
            }

          slotCount (SlotNo i) = \case
            Slotting.Origin        -> i
            Slotting.At (SlotNo j) -> i - j

        F.writeDataPoint outFileHandle outFormat slotDataPoint

        pure $ ExtLedgerState ldgrSt' hdrSt'
      where
        rp = blockRealPoint blk

        forecast ::
             SlotNo
          -> ExtLedgerState blk
          -> IO (LedgerView (BlockProtocol blk))
        forecast slot st = do
            let forecaster = ledgerViewForecastAt lcfg (ledgerState st)
            case runExcept $ forecastFor forecaster slot of
              Left err -> fail $ "benchmark doesn't support headers beyond the forecast limit: " <> show rp <> " " <> show err
              Right x  -> pure x

        tickTheHeaderState ::
             SlotNo
          -> ExtLedgerState blk
          -> LedgerView (BlockProtocol blk)
          -> IO (Ticked (HeaderState blk))
        tickTheHeaderState slot st ledgerView =
            pure $! tickHeaderState ccfg
                                    ledgerView
                                    slot
                                    (headerState st)

        applyTheHeader ::
             LedgerView (BlockProtocol blk)
          -> Ticked (HeaderState blk)
          -> IO (HeaderState blk)
        applyTheHeader ledgerView tickedHeaderState = case ledgerAppMode of
          LedgerApply ->
            case runExcept $ validateHeader cfg ledgerView (getHeader blk) tickedHeaderState of
              Left err -> fail $ "benchmark doesn't support invalid headers: " <> show rp <> " " <> show err
              Right x -> pure x
          LedgerReapply ->
            pure $! revalidateHeader cfg ledgerView (getHeader blk) tickedHeaderState

        tickTheLedgerState ::
             SlotNo
          -> ExtLedgerState blk
          -> IO (Ticked (LedgerState blk))
        tickTheLedgerState slot st =
            pure $ applyChainTick lcfg slot (ledgerState st)

        applyTheBlock ::
             Ticked (LedgerState blk)
          -> IO (LedgerState blk)
        applyTheBlock tickedLedgerSt = case ledgerAppMode of
          LedgerApply ->
            case runExcept (lrResult <$> applyBlockLedgerResult lcfg blk tickedLedgerSt) of
              Left err -> fail $ "benchmark doesn't support invalid blocks: " <> show rp <> " " <> show err
              Right x  -> pure x
          LedgerReapply ->
            pure $! lrResult $ reapplyBlockLedgerResult lcfg blk tickedLedgerSt

withFile :: Maybe FilePath -> (IO.Handle -> IO r) -> IO r
withFile (Just outfile) = IO.withFile outfile IO.WriteMode
withFile Nothing        = \f -> f IO.stdout

{-------------------------------------------------------------------------------
  Analysis: trace ledger state metrics
-------------------------------------------------------------------------------}

getBlockApplicationMetrics ::
    forall blk .
    ( HasAnalysis blk
    , LedgerSupportsProtocol blk
    )
  => NumberOfBlocks -> Maybe FilePath -> Analysis blk StartFromLedgerState
getBlockApplicationMetrics (NumberOfBlocks nrBlocks) mOutFile env = do
    withFile mOutFile $ \outFileHandle -> do
        writeHeaderLine outFileHandle separator (HasAnalysis.blockApplicationMetrics @blk)
        void $ processAll db registry GetBlock startFrom limit initLedger (process outFileHandle)
        pure Nothing
  where
    separator = ", "

    AnalysisEnv {db, registry, startFrom, cfg, limit } = env
    FromLedgerState initLedger = startFrom

    process :: IO.Handle -> ExtLedgerState blk -> blk -> IO (ExtLedgerState blk)
    process outFileHandle currLedgerSt blk = do
      let nextLedgerSt = tickThenReapply (ExtLedgerCfg cfg) blk currLedgerSt
      when (unBlockNo (blockNo blk) `mod` nrBlocks == 0) $ do
          let blockApplication =
                HasAnalysis.WithLedgerState blk
                                            (ledgerState currLedgerSt)
                                            (ledgerState nextLedgerSt)

          computeAndWriteLine outFileHandle
                              separator
                              (HasAnalysis.blockApplicationMetrics @blk)
                              blockApplication

          IO.hFlush outFileHandle

      return nextLedgerSt

{-------------------------------------------------------------------------------
  Analysis: reforge the blocks, via the mempool
-------------------------------------------------------------------------------}

data ReproMempoolForgeHowManyBlks = ReproMempoolForgeOneBlk | ReproMempoolForgeTwoBlks

reproMempoolForge ::
  forall blk.
  ( HasAnalysis blk
  , LedgerSupportsMempool.HasTxId (LedgerSupportsMempool.GenTx blk)
  , LedgerSupportsMempool.HasTxs blk
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  ) =>
  Int ->
  Analysis blk StartFromLedgerState
reproMempoolForge numBlks env = do
    howManyBlocks <- case numBlks of
      1 -> pure ReproMempoolForgeOneBlk
      2 -> pure ReproMempoolForgeTwoBlks
      _ -> fail $ "--repro-mempool-and-forge only supports"
               <> "1 or 2 blocks at a time, not " <> show numBlks

    ref <- IOLike.newTVarIO initLedger
    mempool <- Mempool.openMempoolWithoutSyncThread
      Mempool.LedgerInterface {
        Mempool.getCurrentLedgerState = ledgerState <$> IOLike.readTVar ref
      }
      lCfg
      -- one mebibyte should generously accomodate two blocks' worth of txs
      ( Mempool.MempoolCapacityBytesOverride
      $ LedgerSupportsMempool.ByteSize32
      $ 1024*1024
      )
      nullTracer

    void $ processAll db registry GetBlock startFrom limit Nothing (process howManyBlocks ref mempool)
    pure Nothing
  where
    AnalysisEnv {
      cfg
    , startFrom = startFrom@(FromLedgerState initLedger)
    , db
    , registry
    , limit
    , tracer
    } = env

    lCfg :: LedgerConfig blk
    lCfg = configLedger cfg

    elCfg :: LedgerCfg (ExtLedgerState blk)
    elCfg = ExtLedgerCfg cfg

    timed :: IO a -> IO (a, IOLike.DiffTime, Int64, Int64)
    timed m = do
      before <- IOLike.getMonotonicTime
      prevRtsStats <- GC.getRTSStats
      !x <- m
      newRtsStats <- GC.getRTSStats
      after <- IOLike.getMonotonicTime
      pure ( x
           , after `IOLike.diffTime` before
           , (GC.mutator_elapsed_ns newRtsStats - GC.mutator_elapsed_ns prevRtsStats) `div` 1000
           , (GC.gc_elapsed_ns newRtsStats - GC.gc_elapsed_ns prevRtsStats) `div` 1000
           )

    process
      :: ReproMempoolForgeHowManyBlks
      -> IOLike.StrictTVar IO (ExtLedgerState blk)
      -> Mempool.Mempool IO blk
      -> Maybe blk
      -> blk
      -> IO (Maybe blk)
    process howManyBlocks ref mempool mbBlk blk' = (\() -> Just blk') <$> do
      -- add this block's transactions to the mempool
      do
        results <- Mempool.addTxs mempool $ LedgerSupportsMempool.extractTxs blk'
        let rejs =
              [ (LedgerSupportsMempool.txId tx, rej)
              | rej@(Mempool.MempoolTxRejected tx _) <- results
              ]
        unless (null rejs) $ do
          fail $ unlines $
               ["Mempool rejected some of the on-chain txs: " <> show rejs]
            <> case howManyBlocks of
                 ReproMempoolForgeOneBlk -> []
                 ReproMempoolForgeTwoBlks ->
                   [ "This might be expected, see the db-analyser README."
                   , "Consider trying again with `--repro-mempool-and-forge 1`."
                   ]

      let scrutinee = case howManyBlocks of
            ReproMempoolForgeOneBlk  -> Just blk'
            ReproMempoolForgeTwoBlks -> mbBlk
      case scrutinee of
        Nothing  -> pure ()
        Just blk -> do
          st <- IOLike.readTVarIO ref

          -- time the suspected slow parts of the forge thread that created
          -- this block
          --
          -- Primary caveat: that thread's mempool may have had more transactions in it.
          do
            let slot = blockSlot blk
            (ticked, durTick, mutTick, gcTick) <- timed $ IOLike.evaluate $
              applyChainTick lCfg slot (ledgerState st)
            ((), durSnap, mutSnap, gcSnap) <- timed $ IOLike.atomically $ do
              snap <- Mempool.getSnapshotFor mempool $ Mempool.ForgeInKnownSlot slot ticked

              pure $ length (Mempool.snapshotTxs snap) `seq` Mempool.snapshotLedgerState snap `seq` ()

            let sizes = HasAnalysis.blockTxSizes blk
            traceWith tracer $
              BlockMempoolAndForgeRepro
                (blockNo blk)
                slot
                (length sizes)
                (sum sizes)
                durTick
                mutTick
                gcTick
                durSnap
                mutSnap
                gcSnap

          -- advance the ledger state to include this block
          --
          -- TODO We could inline/reuse parts of the IsLedger ExtLedgerState
          -- instance here as an optimization that avoids repeating the
          -- 'applyChainTick' call above. We want to leave that call alone, though,
          -- since it currently matches the call in the forging thread, which is
          -- the primary intention of this Analysis. Maybe GHC's CSE is already
          -- doing this sharing optimization?
          IOLike.atomically $ IOLike.writeTVar ref $! tickThenReapply elCfg blk st

          -- this flushes blk from the mempool, since every tx in it is now on the chain
          void $ Mempool.syncWithLedger mempool

{-------------------------------------------------------------------------------
  Analysis: print out the stake distibution for each epoch
-------------------------------------------------------------------------------}

dumpStakeDistributions ::
  forall blk.
  ( HasAnalysis blk,
    LedgerSupportsProtocol blk
  ) =>
  Analysis blk StartFromLedgerState
dumpStakeDistributions env = do
    void $ processAll db registry GetBlock startFrom limit (initLedger, Nothing) process
    pure Nothing
  where
    AnalysisEnv {db, cfg, limit, registry, startFrom, tracer} = env

    FromLedgerState initLedger = startFrom

    process
      :: (ExtLedgerState blk, Maybe EpochNo)
      -> blk
      -> IO (ExtLedgerState blk, Maybe EpochNo)
    process (oldLedger, mbEpoch) blk = do
      let lcfg      = ExtLedgerCfg cfg
          newLedger = tickThenReapply lcfg blk oldLedger
          lst       = ledgerState newLedger

      (,) newLedger <$> case HasAnalysis.epochPoolDistr lst of
          Just (epoch, pd)
            | mbEpoch /= Just epoch ->
              Just epoch <$ traceWith tracer (DumpStakeDistribution epoch pd)

          _ -> pure mbEpoch

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the DB
-------------------------------------------------------------------------------}

decreaseLimit :: Limit -> Maybe Limit
decreaseLimit Unlimited = Just Unlimited
decreaseLimit (Limit 0) = Nothing
decreaseLimit (Limit n) = Just . Limit $ n - 1

data NextStep = Continue | Stop


processAllUntil ::
     forall blk b startFrom st. (HasHeader blk, HasAnnTip blk)
  => ImmutableDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> AnalysisStartFrom blk startFrom
  -> Limit
  -> st
  -> (st -> b -> IO (NextStep, st))
  -> IO st
processAllUntil immutableDB registry blockComponent startFrom limit initState callback = do
    itr <- ImmutableDB.streamAfterKnownPoint
      immutableDB
      registry
      blockComponent
      (startFromPoint startFrom)
    go itr limit initState
  where
    go :: ImmutableDB.Iterator IO blk b -> Limit -> st -> IO st
    go itr lt !st = case decreaseLimit lt of
      Nothing               -> return st
      Just decreasedLimit   -> do
        itrResult <- ImmutableDB.iteratorNext itr
        case itrResult of
          ImmutableDB.IteratorExhausted -> return st
          ImmutableDB.IteratorResult b  -> callback st b >>= \case
            (Continue, nst) -> go itr decreasedLimit nst
            (Stop, nst)     -> return nst

processAll ::
     forall blk b startFrom st. (HasHeader blk, HasAnnTip blk)
  => ImmutableDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> AnalysisStartFrom blk startFrom
  -> Limit
  -> st
  -> (st -> b -> IO st)
  -> IO st
processAll db rr blockComponent startFrom limit initSt cb =
  processAllUntil db rr blockComponent startFrom limit initSt callback
    where
      callback st b = (Continue, ) <$> cb st b

processAll_ ::
     forall blk b startFrom. (HasHeader blk, HasAnnTip blk)
  => ImmutableDB IO blk
  -> ResourceRegistry IO
  -> BlockComponent blk b
  -> AnalysisStartFrom blk startFrom
  -> Limit
  -> (b -> IO ())
  -> IO ()
processAll_ db registry blockComponent startFrom limit callback =
    processAll db registry blockComponent startFrom limit () (const callback)
