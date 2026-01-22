{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.Text (Text)
import Barbies
import qualified Cardano.Slotting.Slot as Slotting
import qualified Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.FileWriting as F
import qualified Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint as DP
import Cardano.Tools.DBAnalyser.CSV
  ( computeAndWriteLine
  , writeHeaderLine
  )
import Cardano.Tools.DBAnalyser.HasAnalysis (HasAnalysis)
import qualified Cardano.Tools.DBAnalyser.HasAnalysis as HasAnalysis
import Cardano.Tools.DBAnalyser.Types
import Control.Monad (unless, void, when)
import Control.Monad.Except (runExcept)
import Control.ResourceRegistry
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Singletons
import Data.Word (Word16, Word32, Word64)
import qualified Debug.Trace as Debug
import GHC.Generics hiding (to)
import qualified GHC.Stats as GC
import Lens.Micro
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
  , tickThenApply
  , tickThenApplyLedgerResult
  , tickThenReapply
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
import Ouroboros.Consensus.Util.Condense (Condense (..))
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.SizeInBytes
import qualified System.IO as IO
import Data.Monoid
import Numeric.Natural
import Cardano.Ledger.BaseTypes (ProtVer(..), getVersion64)

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
  go (DumpBlockHeader{blockFile, transactionFile}) = mkAnalysis $ dumpBlockHeader blockFile transactionFile
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
  | -- | Free form message that an analysis can use when it needs too
    Message String
  | -- | triggered when analysis has ended
    DoneEvent
  | -- | Block number, slot number, body size
    BlockHeader BlockNo SlotNo Word32
  | -- | triggered when block has been found, it holds:
    --   * block's number
    --   * slot number when the block was forged
    BlockSlotEvent BlockNo SlotNo (HeaderHash blk)
  | -- | triggered when block has been found, it holds:
    --   * block's number
    --   * slot number when the block was forged
    --   * cumulative tx output
    --   * count tx output
    CountTxOutputsEvent BlockNo SlotNo Int Int
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
    --   * number of transactions in the block
    --   * total size of transactions in the block
    BlockTxSizeEvent SlotNo Int SizeInBytes
  | -- | triggered for all blocks during MempoolAndForgeRepro analysis,
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
    BlockMempoolAndForgeRepro
      BlockNo
      SlotNo
      Int
      SizeInBytes
      IOLike.DiffTime
      Int64
      Int64
      IOLike.DiffTime
      Int64
      Int64

instance (HasAnalysis blk, LedgerSupportsProtocol blk) => Show (TraceEvent blk) where
  show (StartedEvent analysisName) = "Started " <> (show analysisName)
  show (Message msg) = "Info: " ++ msg
  show DoneEvent = "Done"
  show (BlockHeader bn sn sz) =
    intercalate ", " $
      [ show ((unBlockNo bn) :: Word64)
      , show ((unSlotNo sn) :: Word64)
      , show sz
      ]
  show (BlockSlotEvent bn sn h) =
    intercalate "\t" $
      [ show bn
      , show sn
      , show h
      ]
  show (CountTxOutputsEvent bn sn cumulative count) =
    intercalate "\t" $
      [ show bn
      , show sn
      , "cumulative: " <> show cumulative
      , "count: " <> show count
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
  show (BlockTxSizeEvent slot numBlocks txsSize) =
    intercalate
      "\t"
      [ show slot
      , "Num txs in block = " <> show numBlocks
      , "Total size of txs in block = " <> show txsSize
      ]
  show (BlockMempoolAndForgeRepro bno slot txsCount txsSize durTick mutTick gcTick durSnap mutSnap gcSnap) =
    intercalate
      "\t"
      [ show bno
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
  Analysis: dump metadata
-------------------------------------------------------------------------------}

-- | Avoids annoying `deriving` behaviour due to HeaderHash being a type family
newtype WrappedHeaderHash blk = MkWHH (HeaderHash blk)

deriving newtype instance Show (HeaderHash blk) => Show (WrappedHeaderHash blk)
deriving newtype instance Condense (HeaderHash blk) => Condense (WrappedHeaderHash blk)

data DumpQuery blk f = MkDumpQuery
  { query_header :: f (Header blk)
  , query_size :: f SizeInBytes
  , query_is_ebb :: f IsEBB
  , query_block :: f blk
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

data BlockFeatures blk f = MkBlockFeatures
  { block_num :: f BlockNo
  -- ^ The block's number
  , slot_num :: f SlotNo
  -- ^ The slot in which the block was minted
  , block_hash :: f (WrappedHeaderHash blk)
  -- ^ The block's hash
  , block_size :: f Word32
  -- ^ The blocks' size (in bytes)
  , is_ebb :: f IsEBB
  -- ^ Is the block an epoch-boundary block
  , predecessor :: f (ChainHash blk)
  -- ^ The hash of this block's predecessor
  , num_transactions :: f Int
  -- ^ Number of transaction in the block
  , era :: f Text
  -- ^ Name of the block's era. 
  , prot_major :: f Word64
  -- ^ Major protocol version
  , prot_minor :: f Natural
  -- ^ Minor protocol version
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

data TxFeatures blk f = MkTxFeatures
  { src_block :: f BlockNo
  , num_script_wits :: f Int
  , num_addr_wits:: f Int
    -- ^ Number of regular address witnesses
  , size_script_wits :: f Int
  , size_datum :: f Int
  , num_inputs :: f Int
  , num_outputs :: f Int
  , num_ref_inputs :: f Int
  , num_certs :: f Int
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

blockFeaturesNames :: BlockFeatures blk (Const String)
blockFeaturesNames =
  MkBlockFeatures
    { block_num = Const "block_id"
    , slot_num = Const "slot#"
    , block_hash = Const "block_hash"
    , block_size = Const "block_size"
    , is_ebb = Const "is_ebb?"
    , predecessor = Const "predecessor"
    , num_transactions = Const "#transactions" -- TODO: compute in duckdb?
    , era = "era_name"
    , prot_major = "major_protocol_version"
    , prot_minor = "minor_protocol_version"
    }

txFeaturesNames :: TxFeatures blk (Const String)
txFeaturesNames =
  MkTxFeatures
    { src_block = "block_id"
    , num_script_wits = "#script_wits"
    , num_addr_wits = "#addr_wits"
    , size_script_wits = "script_wits_size"
    , size_datum = "datum_size"
    , num_inputs = "#inputs"
    , num_outputs = "#outputs"
    , num_ref_inputs = "#reference_inputs"
    , num_certs = "#certs"
    }

dumpBlockHeader ::
  forall blk.
  HasAnalysis blk =>
  -- | Csv file where the block data is to be stored
  FilePath ->
  -- | Csv file where the transaction data is to be stored
  FilePath ->
  Analysis blk StartFromPoint
dumpBlockHeader blockFile txFile AnalysisEnv{db, registry, startFrom, limit, tracer} = do
  traceWith tracer $
    Message $
      "Saving block metadata to: " ++ blockFile
  traceWith tracer $
    Message $
      "Saving transaction metadata to: " ++ txFile
  withFile (Just blockFile) $ \outBlockHandle ->
    withFile (Just txFile) $ \outTxHandle -> do
      let blockHeader = csv $ Container blockFeaturesNames
      let txHeader = csv $ Container txFeaturesNames
      IO.hPutStrLn outBlockHandle blockHeader
      IO.hPutStrLn outTxHandle txHeader
      let
        component :: BlockComponent blk (DumpQuery blk Identity)
        component =
          bsequence' $
            MkDumpQuery
              { query_header = GetHeader
              , query_size = GetBlockSize
              , query_is_ebb = GetIsEBB
              , query_block = GetBlock
              }
      processAll_ db registry component startFrom limit (process outBlockHandle outTxHandle)
        >> pure Nothing
 where
  process :: IO.Handle -> IO.Handle -> DumpQuery blk Identity -> IO ()
  process bh th cmp = do
    let
      blockFeatures :: BlockFeatures blk Identity
      blockFeatures =
        MkBlockFeatures
          { block_num = blockNo <$> query_header cmp
          , slot_num = blockSlot <$> query_header cmp
          , block_hash = MkWHH . headerHash <$> query_header cmp
          , block_size = getSizeInBytes <$> query_size cmp
          , is_ebb = query_is_ebb cmp
          , predecessor = headerPrevHash <$> query_header cmp
          , num_transactions = length . toListOf HasAnalysis.txs <$> query_block cmp
          , era = HasAnalysis.eraName <$> query_block cmp
          , prot_major = getVersion64 . pvMajor . HasAnalysis.protVer <$> query_block cmp
          , prot_minor = pvMinor . HasAnalysis.protVer <$> query_block cmp
          }
    let line = csv $ Container $ bmapC @Condense (Const . condense . runIdentity) blockFeatures
    IO.hPutStrLn bh line

    let
      script_wits :: SimpleFold (HasAnalysis.TxOf blk) (HasAnalysis.ScriptType blk)
      script_wits = HasAnalysis.wits @blk . HasAnalysis.scriptWits @blk . traverse
      
      txFeatures :: HasAnalysis.TxOf blk -> TxFeatures blk Identity
      txFeatures tx =
        MkTxFeatures
          { src_block = blockNo <$> query_header cmp
          , num_script_wits = Identity $ length $ toListOf script_wits tx
          , num_addr_wits = Identity $ length $ toListOf (HasAnalysis.wits @blk . HasAnalysis.addrWits @blk . folded) tx
          , size_script_wits = Identity $ getSum $ foldMapOf (script_wits . to (HasAnalysis.scriptSize @blk)) Sum tx
          , size_datum = Identity $ tx ^. (HasAnalysis.wits @blk . to (HasAnalysis.datumSize @blk))
          , num_inputs = Identity $ HasAnalysis.numInputs @blk tx
          , num_outputs = Identity $ HasAnalysis.numOutputs @blk tx
          , num_ref_inputs = Identity $ length $ toListOf (HasAnalysis.referenceInputs @blk) tx
          , num_certs = Identity $ length $ toListOf (HasAnalysis.certs @blk) tx
          }
    let txFeaturess = toListOf (HasAnalysis.txs @blk . Lens.Micro.to txFeatures) (runIdentity $ query_block cmp)
    let txlines = map (csv . Container . bmapC @Condense (Const . condense . runIdentity)) txFeaturess
    forM_ txlines $ \txl ->
      IO.hPutStrLn th txl

  csv :: Foldable t => t String -> String
  csv = intercalate ", " . toList

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
countTxOutputs AnalysisEnv{db, registry, startFrom, limit, tracer} = do
  void $ processAll db registry GetBlock startFrom limit 0 process
  pure Nothing
 where
  process :: Int -> blk -> IO Int
  process cumulative blk = do
    let cumulative' = cumulative + count
        event =
          CountTxOutputsEvent
            (blockNo blk)
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

showBlockTxsSize :: forall blk. HasAnalysis blk => Analysis blk StartFromPoint
showBlockTxsSize AnalysisEnv{db, registry, startFrom, limit, tracer} = do
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
  , HasAnalysis blk
  ) =>
  SlotNo ->
  LedgerApplicationMode ->
  Analysis blk StartFromLedgerState
storeLedgerStateAt slotNo ledgerAppMode env = do
  void $ processAllUntil db registry GetBlock startFrom limit () process
  pure Nothing
 where
  AnalysisEnv{db, registry, startFrom, cfg, limit, tracer} = env
  FromLedgerState initLedgerDB internal = startFrom

  process :: () -> blk -> IO (NextStep, ())
  process _ blk = do
    let ledgerCfg = ExtLedgerCfg cfg
    oldLedger <- IOLike.atomically $ LedgerDB.getVolatileTip initLedgerDB
    frk <-
      LedgerDB.getForkerAtTarget initLedgerDB registry VolatileTip >>= \case
        Left{} -> error "Unreachable, volatile tip MUST be in the LedgerDB"
        Right f -> pure f
    tbs <- LedgerDB.forkerReadTables frk (getBlockKeySets blk)
    r <- case runExcept $ tickThenXApply OmitLedgerEvents ledgerCfg blk (oldLedger `withLedgerTables` tbs) of
      Right newLedger -> do
        LedgerDB.forkerPush frk newLedger
        IOLike.atomically $ LedgerDB.forkerCommit frk
        when (blockSlot blk >= slotNo) storeLedgerState
        when (blockSlot blk > slotNo) $ issueWarning blk
        when ((unBlockNo $ blockNo blk) `mod` 1000 == 0) $ reportProgress blk
        LedgerDB.tryFlush initLedgerDB
        LedgerDB.garbageCollect initLedgerDB
          . fromWithOrigin 0
          . pointSlot
          . getTip
          =<< IOLike.atomically (LedgerDB.getImmutableTip initLedgerDB)
        return (continue blk, ())
      Left err -> do
        traceWith tracer $ LedgerErrorEvent (blockPoint blk) err
        storeLedgerState
        pure (Stop, ())
    LedgerDB.forkerClose frk
    pure r

  tickThenXApply = case ledgerAppMode of
    LedgerReapply -> pure ...: tickThenReapply
    LedgerApply -> tickThenApply

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
    IOLike.atomically (pointSlot <$> LedgerDB.currentPoint initLedgerDB) >>= \case
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
  , CanStowLedgerTables (LedgerState blk)
  ) =>
  Word64 ->
  Analysis blk StartFromLedgerState
checkNoThunksEvery
  nBlocks
  (AnalysisEnv{db, registry, startFrom, cfg, limit}) = do
    putStrLn $
      "Checking for thunks in each block where blockNo === 0 (mod " <> show nBlocks <> ")."
    void $ processAll db registry GetBlock startFrom limit () process
    pure Nothing
   where
    FromLedgerState ldb internal = startFrom

    process :: () -> blk -> IO ()
    process _ blk = do
      oldLedger <- IOLike.atomically $ LedgerDB.getVolatileTip ldb
      frk <-
        LedgerDB.getForkerAtTarget ldb registry VolatileTip >>= \case
          Left{} -> error "Unreachable, volatile tip MUST be in the LedgerDB"
          Right f -> pure f
      tbs <- LedgerDB.forkerReadTables frk (getBlockKeySets blk)
      LedgerDB.forkerClose frk
      let oldLedger' = oldLedger `withLedgerTables` tbs
      let ledgerCfg = ExtLedgerCfg cfg
          appliedResult = tickThenApplyLedgerResult OmitLedgerEvents ledgerCfg blk oldLedger'
          newLedger = either (error . show) lrResult $ runExcept appliedResult
          newLedger' = applyDiffs oldLedger' newLedger
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
  ) =>
  Analysis blk StartFromLedgerState
traceLedgerProcessing
  (AnalysisEnv{db, registry, startFrom, cfg, limit}) = do
    void $ processAll db registry GetBlock startFrom limit () (process initLedger)
    pure Nothing
   where
    FromLedgerState initLedger internal = startFrom

    process ::
      LedgerDB.LedgerDB' IO blk ->
      () ->
      blk ->
      IO ()
    process ledgerDB _ blk = do
      frk <-
        LedgerDB.getForkerAtTarget ledgerDB registry VolatileTip >>= \case
          Left{} -> error "Unreachable, volatile tip MUST be in the LedgerDB"
          Right f -> pure f
      oldLedgerSt <- IOLike.atomically $ LedgerDB.forkerGetLedgerState frk
      oldLedgerTbs <- LedgerDB.forkerReadTables frk (getBlockKeySets blk)
      let oldLedger = oldLedgerSt `withLedgerTables` oldLedgerTbs
      LedgerDB.forkerClose frk

      let ledgerCfg = ExtLedgerCfg cfg
          appliedResult = tickThenApplyLedgerResult OmitLedgerEvents ledgerCfg blk oldLedger
          newLedger = either (error . show) lrResult $ runExcept appliedResult
          newLedger' = applyDiffs oldLedger newLedger
          traces =
            ( HasAnalysis.emitTraces $
                HasAnalysis.WithLedgerState blk (ledgerState oldLedger) (ledgerState newLedger')
            )
      mapM_ Debug.traceMarkerIO traces

      LedgerDB.push internal newLedger
      LedgerDB.tryFlush ledgerDB

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
  ) =>
  Maybe FilePath ->
  LedgerApplicationMode ->
  Analysis blk StartFromLedgerState
benchmarkLedgerOps mOutfile ledgerAppMode AnalysisEnv{db, registry, startFrom, cfg, limit} = do
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
    (prevLedgerState, tables) <- LedgerDB.withPrivateTipForker ledgerDB $ \frk -> do
      st <- IOLike.atomically $ LedgerDB.forkerGetLedgerState frk
      tbs <- LedgerDB.forkerReadTables frk (getBlockKeySets blk)
      pure (st, tbs)
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

    let slot = blockSlot blk
    -- We do not use strictness annotation on the resulting tuples since
    -- 'time' takes care of forcing the evaluation of its argument's result.
    (ldgrView, tForecast) <- time $ forecast slot prevLedgerState
    (tkHdrSt, tHdrTick) <- time $ tickTheHeaderState slot prevLedgerState ldgrView
    (!newHeader, tHdrApp) <- time $ applyTheHeader ldgrView tkHdrSt
    (tkLdgrSt, tBlkTick) <- time $ tickTheLedgerState slot prevLedgerState
    let !tkLdgrSt' = applyDiffs (prevLedgerState `withLedgerTables` tables) tkLdgrSt
    (!newLedger, tBlkApp) <- time $ applyTheBlock tkLdgrSt'

    currentRtsStats <- GC.getRTSStats
    let
      currentMinusPrevious :: Num a => (GC.RTSStats -> a) -> a
      currentMinusPrevious f = f currentRtsStats - f prevRtsStats
      major_gcs = currentMinusPrevious GC.major_gcs
      slotDataPoint =
        DP.SlotDataPoint
          { DP.slot = realPointSlot rp
          , DP.slotGap = slot `slotCount` getTipSlot prevLedgerState
          , DP.totalTime = currentMinusPrevious GC.elapsed_ns `div` 1000
          , DP.mut = currentMinusPrevious GC.mutator_elapsed_ns `div` 1000
          , DP.gc = currentMinusPrevious GC.gc_elapsed_ns `div` 1000
          , DP.majGcCount = major_gcs
          , DP.minGcCount = currentMinusPrevious GC.gcs - major_gcs
          , DP.allocatedBytes = currentMinusPrevious GC.allocated_bytes
          , DP.mut_forecast = tForecast `div` 1000
          , DP.mut_headerTick = tHdrTick `div` 1000
          , DP.mut_headerApply = tHdrApp `div` 1000
          , DP.mut_blockTick = tBlkTick `div` 1000
          , DP.mut_blockApply = tBlkApp `div` 1000
          , DP.blockByteSize = getSizeInBytes sz
          , DP.blockStats = DP.BlockStats $ HasAnalysis.blockStats blk
          }

      slotCount (SlotNo i) = \case
        Slotting.Origin -> i
        Slotting.At (SlotNo j) -> i - j

    F.writeDataPoint outFileHandle outFormat slotDataPoint

    LedgerDB.push intLedgerDB $ ExtLedgerState newLedger newHeader
    LedgerDB.tryFlush ledgerDB
   where
    rp = blockRealPoint blk

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
  ) =>
  NumberOfBlocks -> Maybe FilePath -> Analysis blk StartFromLedgerState
getBlockApplicationMetrics (NumberOfBlocks nrBlocks) mOutFile env = do
  withFile mOutFile $ \outFileHandle -> do
    writeHeaderLine outFileHandle separator (HasAnalysis.blockApplicationMetrics @blk)
    void $
      processAll db registry GetBlock startFrom limit () (process initLedger internal outFileHandle)
    pure Nothing
 where
  separator = ", "

  AnalysisEnv{db, registry, startFrom, cfg, limit} = env
  FromLedgerState initLedger internal = startFrom

  process ::
    LedgerDB.LedgerDB' IO blk ->
    LedgerDB.TestInternals' IO blk ->
    IO.Handle ->
    () ->
    blk ->
    IO ()
  process ledgerDB intLedgerDB outFileHandle _ blk = do
    frk <-
      LedgerDB.getForkerAtTarget ledgerDB registry VolatileTip >>= \case
        Left{} -> error "Unreachable, volatile tip MUST be in the LedgerDB"
        Right f -> pure f
    oldLedgerSt <- IOLike.atomically $ LedgerDB.forkerGetLedgerState frk
    oldLedgerTbs <- LedgerDB.forkerReadTables frk (getBlockKeySets blk)
    let oldLedger = oldLedgerSt `withLedgerTables` oldLedgerTbs
    LedgerDB.forkerClose frk

    let nextLedgerSt = tickThenReapply OmitLedgerEvents (ExtLedgerCfg cfg) blk oldLedger
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
    LedgerDB.tryFlush ledgerDB

    pure ()

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
    _ ->
      fail $
        "--repro-mempool-and-forge only supports"
          <> "1 or 2 blocks at a time, not "
          <> show numBlks

  withRegistry $ \reg -> do
    mempool <-
      Mempool.openMempoolWithoutSyncThread
        reg
        Mempool.LedgerInterface
          { Mempool.getCurrentLedgerState = \reg' -> do
              st <- LedgerDB.getVolatileTip ledgerDB
              pure $
                MempoolLedgerDBView
                  (ledgerState st)
                  ( fmap (LedgerDB.ledgerStateReadOnlyForker . LedgerDB.readOnlyForker)
                      <$> LedgerDB.getForkerAtTarget ledgerDB reg' (SpecificPoint (castPoint $ getTip st))
                  )
          }
        lCfg
        -- one mebibyte should generously accomodate two blocks' worth of txs
        ( Mempool.MempoolCapacityBytesOverride $
            LedgerSupportsMempool.ByteSize32 $
              1024 * 1024
        )
        nullTracer

    void $ processAll db registry GetBlock startFrom limit Nothing (process howManyBlocks mempool)
    pure Nothing
 where
  AnalysisEnv
    { cfg
    , startFrom = startFrom@(FromLedgerState ledgerDB intLedgerDB)
    , db
    , registry
    , limit
    , tracer
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
    Maybe blk ->
    blk ->
    IO (Maybe blk)
  process howManyBlocks mempool mbBlk blk' =
    (\() -> Just blk') <$> do
      -- add this block's transactions to the mempool
      do
        results <- Mempool.addTxs mempool $ LedgerSupportsMempool.extractTxs blk'
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
          LedgerDB.withPrivateTipForker ledgerDB $ \forker -> do
            st <- IOLike.atomically $ LedgerDB.forkerGetLedgerState forker

            -- time the suspected slow parts of the forge thread that created
            -- this block
            --
            -- Primary caveat: that thread's mempool may have had more transactions in it.
            let slot = blockSlot blk
            (ticked, durTick, mutTick, gcTick) <-
              timed $
                IOLike.evaluate $
                  applyChainTick OmitLedgerEvents lCfg slot (ledgerState st)
            ((), durSnap, mutSnap, gcSnap) <- timed $ do
              snap <-
                Mempool.getSnapshotFor mempool slot ticked $
                  fmap castLedgerTables . LedgerDB.forkerReadTables forker . castLedgerTables

              pure $ length (Mempool.snapshotTxs snap) `seq` Mempool.snapshotStateHash snap `seq` ()

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
          LedgerDB.reapplyThenPushNOW intLedgerDB blk
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
