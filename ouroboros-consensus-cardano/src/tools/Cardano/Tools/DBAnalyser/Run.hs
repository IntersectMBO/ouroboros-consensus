{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBAnalyser.Run (analyse) where

import           Cardano.Tools.DBAnalyser.Analysis
import           Cardano.Tools.DBAnalyser.HasAnalysis
import           Cardano.Tools.DBAnalyser.Types
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Tracer (Tracer (..), nullTracer)
import qualified Debug.Trace as Debug
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
                     (HasTxs)
import           Ouroboros.Consensus.Ledger.Tables.Utils
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (fromChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import           System.IO


{-------------------------------------------------------------------------------
  Analyse
-------------------------------------------------------------------------------}

analyse ::
     forall blk .
     ( Node.RunNode blk
     , Show (Header blk)
     , HasAnalysis blk
     , HasProtocolInfo blk
     , LedgerSupportsMempool.HasTxs blk
     , CanStowLedgerTables (LedgerState blk)
     )
  => DBAnalyserConfig
  -> Args blk
  -> IO (Maybe AnalysisResult)
analyse DBAnalyserConfig{analysis, confLimit, dbDir, selectDB, validation, verbose} args =
    withRegistry $ \registry -> do
      lock           <- newSVar ()
      chainDBTracer  <- mkTracer lock verbose
      analysisTracer <- mkTracer lock True
      ProtocolInfo { pInfoInitLedger = genesisLedger, pInfoConfig = cfg } <-
        mkProtocolInfo args
      let chunkInfo  = Node.nodeImmutableDbChunkInfo (configStorage cfg)
          k          = configSecurityParam cfg
          diskPolicy = LedgerDB.defaultDiskPolicy k LedgerDB.DefaultSnapshotInterval
          args' =
            Node.mkChainDbArgs
              registry InFuture.dontCheck cfg genesisLedger chunkInfo $
            ChainDB.defaultArgs (Node.stdMkChainDbHasFS dbDir) diskPolicy LedgerDB.V1.InMemoryBackingStore
          chainDbArgs = args' {
              ChainDB.cdbImmutableDbValidation = immValidationPolicy
            , ChainDB.cdbVolatileDbValidation  = volValidationPolicy
            , ChainDB.cdbTracer                = chainDBTracer
            }
          (immutableDbArgs, _, _, _) = fromChainDbArgs chainDbArgs
          _ledgerDbFS = ChainDB.cdbHasFSLgrDB chainDbArgs -- TODO

      case selectDB of
        SelectImmutableDB initializeFrom -> do
          -- TODO we need to check if the snapshot exists. If not, print an
          -- error and ask the user if she wanted to create a snapshot first and
          -- how to do it.
          eInitLedger <- runExceptT $ case initializeFrom of
            Nothing       -> do
              (ledgerDB, intLedgerDB) <- (undefined :: ExceptT Int m (LedgerDB.LedgerDB' m blk, LedgerDB.Internals' m blk)) -- TODO
              pure (forgetLedgerTables genesisLedger, ledgerDB, intLedgerDB)
            Just _snapshot -> do
              (ledgerDB, intLedgerDB) <- undefined -- TODO
              st <- lift $ atomically $ LedgerDB.getVolatileTip ledgerDB
              pure (st, ledgerDB, intLedgerDB)
              -- TODO @readSnapshot@ has type @ExceptT ReadIncrementalErr m
              -- (ExtLedgerState blk)@ but it also throws exceptions! This makes
              -- error handling more challenging than it ought to be. Maybe we
              -- can enrich the error that @readSnapthot@ return, so that it can
              -- contain the @HasFS@ errors as well.
          (initLedger, ledgerDB, intLedgerDB) <- either (error . show) pure eInitLedger
          -- This marker divides the "loading" phase of the program, where the
          -- system is principally occupied with reading snapshot data from
          -- disk, from the "processing" phase, where we are streaming blocks
          -- and running the ledger processing on them.
          Debug.traceMarkerIO "SNAPSHOT_LOADED"
          ImmutableDB.withDB (ImmutableDB.openDB immutableDbArgs runWithTempRegistry) $ \immutableDB -> do
            result <- runAnalysis analysis $ AnalysisEnv {
                cfg
              , initLedger
              , db = Left immutableDB
              , registry
              , limit = confLimit
              , tracer = analysisTracer
              , ledgerDB
              , intLedgerDB
              }
            tipPoint <- atomically $ ImmutableDB.getTipPoint immutableDB
            putStrLn $ "ImmutableDB tip: " ++ show tipPoint
            pure result
        SelectChainDB -> do
          (ledgerDB, intLedgerDB) <- undefined -- TODO
          ChainDB.withDB chainDbArgs $ \chainDB -> do
            result <- runAnalysis analysis $ AnalysisEnv {
                cfg
              , initLedger = forgetLedgerTables genesisLedger
              , db = Right chainDB
              , registry
              , limit = confLimit
              , tracer = analysisTracer
              , ledgerDB
              , intLedgerDB
              }
            tipPoint <- atomically $ ChainDB.getTipPoint chainDB
            putStrLn $ "ChainDB tip: " ++ show tipPoint
            pure result
  where
    mkTracer _    False = return nullTracer
    mkTracer lock True  = do
      startTime <- getMonotonicTime
      return $ Tracer $ \ev -> withLock $ do
        traceTime <- getMonotonicTime
        let diff = diffTime traceTime startTime
        hPutStrLn stderr $ concat ["[", show diff, "] ", show ev]
        hFlush stderr
      where
        withLock = bracket_ (takeSVar lock) (putSVar lock ())

    immValidationPolicy = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> ImmutableDB.ValidateAllChunks
      (_, Just MinimumBlockValidation) -> ImmutableDB.ValidateMostRecentChunk
      (OnlyValidation, _ )             -> ImmutableDB.ValidateAllChunks
      _                                -> ImmutableDB.ValidateMostRecentChunk

    volValidationPolicy = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> VolatileDB.ValidateAll
      (_, Just MinimumBlockValidation) -> VolatileDB.NoValidation
      (OnlyValidation, _ )             -> VolatileDB.ValidateAll
      _                                -> VolatileDB.NoValidation
