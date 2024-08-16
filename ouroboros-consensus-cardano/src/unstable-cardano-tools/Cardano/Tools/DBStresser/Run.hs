{-# LANGUAGE TypeApplications, BangPatterns, PartialTypeSignatures, TypeFamilies #-}

module Cardano.Tools.DBStresser.Run (stress) where

import           Cardano.Slotting.Slot (WithOrigin (..))
import           Cardano.Tools.DBAnalyser.HasAnalysis
import           Cardano.Tools.DBTruncater.Types
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Tracer
import           Data.Either
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node as Node hiding (cfg, registry)
import           Ouroboros.Consensus.Node.InitStorage as Node
import           Ouroboros.Consensus.Storage.ChainDB.API (getPoint)
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB hiding
                     (streamAll)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Stream as ImmutableDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (runWithTempRegistry,
                     withRegistry)
import           Prelude hiding (truncate)
import           System.IO

withDB ::
     (HasProtocolInfo b, RunNode b)
  => DBTruncaterConfig
  -> Args b
  -> FilePath
  -> ((ImmutableDB IO b, Internal IO b) -> IO a)
  -> IO a
withDB cfg args mountPoint action = do
  withRegistry $ \reg -> do
    immutableDBTracer <- mkTracer (verbose cfg)
    config <- pInfoConfig <$> mkProtocolInfo args
    let
      fs = Node.stdMkChainDbHasFS (dbDir cfg) (RelativeMountPoint mountPoint)
      chunkInfo = Node.nodeImmutableDbChunkInfo (configStorage config)
      dbargs = (ImmutableDB.defaultArgs @IO)
          { immTracer = immutableDBTracer
          , immRegistry = reg
          , immCheckIntegrity = nodeCheckIntegrity (configStorage config)
          , immCodecConfig = configCodec config
          , immChunkInfo = chunkInfo
          , immHasFS = fs
          }
    bracket (ImmutableDB.openDBInternal dbargs runWithTempRegistry) (ImmutableDB.closeDB . fst) action
  where
    mkTracer :: (Show a) => Bool -> IO (Tracer IO a)
    mkTracer False = pure mempty
    mkTracer True = do
      lock <- newMVar ()
      startTime <- getMonotonicTime
      pure $ Tracer $ \ev -> do
        bracket_ (takeMVar lock) (putMVar lock ()) $ do
          traceTime <- getMonotonicTime
          let diff = diffTime traceTime startTime
          hPutStrLn stderr $ concat ["[", show diff, "] ", show ev]
          hFlush stderr

-- forkBlockAppend ::
--      HasHeader blk
--   => ImmutableDB IO blk
--   -> [blk]
--   -> IO ()
-- forkBlockAppend client blocks =
--   let
--     f [] = pure ()
--     f (b:bs) = do
--       putStrLn $ "Appending " <> show (blockPoint b)
--       threadDelay 1
--       ImmutableDB.appendBlock client b
--       f bs
--   in void $ forkIO (f blocks)

retrieveAllBlocks :: HasHeader blk => Point blk -> ImmutableDB IO blk -> ImmutableDB IO blk -> IO ()
retrieveAllBlocks pt server client = void $ forkIO $
    fmap (fromRight undefined)
  $ runExceptT
  $ ImmutableDB.streamAll
      (ImmutableDB.streamAPI server)
      pt
      undefined
      ()
      (\b _ -> do
          -- putStrLn $ "Appending " <> show (blockPoint b)
          ImmutableDB.appendBlock client b >> threadDelay 0.001)

openIterator ::
     (IO ~ m, IOLike m, StandardHash blk)
  => ImmutableDB m blk
  -> m ()
openIterator db = do
  withRegistry $ \registry -> do
    pt <- pointToWithOriginRealPoint . ImmutableDB.tipToPoint <$> atomically (ImmutableDB.getTip db)
    !res <- case pt of
      Origin -> undefined
      At pt' -> ImmutableDB.stream
            db
            registry
            ((,) <$> getPoint <*> GetBlock)
            (StreamFromExclusive GenesisPoint)
            (StreamToInclusive pt')
    case res of
      Left e -> error $ show e
      Right{} -> pure ()
    pure ()

stress ::
     (Node.RunNode block, HasProtocolInfo block)
  => DBTruncaterConfig
  -> Args block
  -> IO ()
stress  cfg args = do
  putStrLn "GO!"
  withDB cfg args "immutableclient" $ \(clientDB, _) ->
    withDB (cfg { verbose = False, dbDir = "../mainnet/db/" }) args "immutable" $ \(serverDB, _) -> do
      pt <- ImmutableDB.tipToPoint <$> atomically (ImmutableDB.getTip clientDB)
      retrieveAllBlocks pt serverDB clientDB
      threadDelay 4
      let f = openIterator clientDB >> f
      f
