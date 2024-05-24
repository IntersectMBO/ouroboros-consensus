{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Tools.DBTruncater.Run (truncate) where

import           Cardano.Slotting.Slot (WithOrigin (..))
import           Cardano.Tools.DBAnalyser.HasAnalysis
import           Cardano.Tools.DBTruncater.Types
import           Control.Monad
import           Control.Tracer
import           Data.Functor.Identity
import           Data.Traversable (for)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node as Node
import           Ouroboros.Consensus.Node.InitStorage as Node
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB, Iterator,
                     IteratorResult (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (runWithTempRegistry,
                     withRegistry)
import           Prelude hiding (truncate)
import           System.IO

truncate ::
     forall block. (Node.RunNode block, HasProtocolInfo block)
  => DBTruncaterConfig
  -> Args block
  -> IO ()
truncate DBTruncaterConfig{ dbDir, truncateAfter, verbose } args = do
  withRegistry $ \registry -> do
    lock <- mkLock
    immutableDBTracer <- mkTracer lock verbose
    ProtocolInfo {
      pInfoConfig = config
    } <- mkProtocolInfo args
    let
      fs = Node.stdMkChainDbHasFS dbDir (RelativeMountPoint "immutable")
      chunkInfo = Node.nodeImmutableDbChunkInfo (configStorage config)
      immutableDBArgs :: Complete ImmutableDbArgs IO block
      immutableDBArgs =
        (ImmutableDB.defaultArgs @IO)
          { immTracer = immutableDBTracer
          , immRegistry = registry
          , immCheckIntegrity = nodeCheckIntegrity (configStorage config)
          , immCodecConfig = configCodec config
          , immChunkInfo = chunkInfo
          , immHasFS = fs
          }

    withDB immutableDBArgs $ \(immutableDB, internal) -> do
      mLastHdr :: Maybe (Header block) <- case truncateAfter of
        TruncateAfterSlot slotNo -> do
          mHash <- getHashForSlot internal slotNo
          for (RealPoint slotNo <$> mHash) $
            ImmutableDB.getKnownBlockComponent immutableDB GetHeader

        TruncateAfterBlock bno   -> do
          -- At the moment, we're just running a linear search with streamAll to
          -- find the correct block to truncate from, but we could in theory do this
          -- more quickly by binary searching the chunks of the ImmutableDB.
          iterator <- ImmutableDB.streamAll immutableDB registry GetHeader
          findLast ((<= bno) . blockNo) iterator

      case ImmutableDB.headerToTip <$> mLastHdr of
        Nothing ->
          putStrLn $ mconcat
            [ "Unable to find a truncate point. This is because the ImmutableDB"
            , "does not contain a block with the given slot or block number."
            ]
        Just newTip -> do
          when verbose $ do
            putStrLn $ mconcat
              [ "Truncating the ImmutableDB using the following block as the "
              , "new tip:\n"
              , "  ", show newTip
              ]
          deleteAfter internal (At newTip)

-- | Given a predicate, and an iterator, find the last item for which
-- the predicate passes.
findLast :: Monad m => (a -> Bool) -> Iterator m blk a -> m (Maybe a)
findLast p iter =
    go Nothing
  where
    go acc =
      ImmutableDB.iteratorNext iter >>= \case
        IteratorExhausted -> do
          ImmutableDB.iteratorClose iter
          pure acc
        IteratorResult a -> do
          if p a then go (Just a) else pure acc

mkLock :: MonadMVar m => m (StrictMVar m ())
mkLock = newMVar ()

mkTracer :: Show a => StrictMVar IO () -> Bool -> IO (Tracer IO a)
mkTracer _ False = pure mempty
mkTracer lock True = do
  startTime <- getMonotonicTime
  pure $ Tracer $ \ev -> do
    bracket_ (takeMVar lock) (putMVar lock ()) $ do
      traceTime <- getMonotonicTime
      let diff = diffTime traceTime startTime
      hPutStrLn stderr $ concat ["[", show diff, "] ", show ev]
      hFlush stderr

withDB ::
     (Node.RunNode block, IOLike m)
  => ImmutableDbArgs Identity m block
  -> ((ImmutableDB m block, Internal m block) -> m a)
  -> m a
withDB immutableDBArgs = bracket (ImmutableDB.openDBInternal immutableDBArgs runWithTempRegistry) (ImmutableDB.closeDB . fst)
