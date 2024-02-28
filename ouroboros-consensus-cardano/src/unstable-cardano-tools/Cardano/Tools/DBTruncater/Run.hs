{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Cardano.Tools.DBTruncater.Run (truncate) where

import           Cardano.Slotting.Slot (WithOrigin (..))
import           Cardano.Tools.DBAnalyser.HasAnalysis
import           Cardano.Tools.DBTruncater.Types
import           Control.Monad
import           Control.Tracer
import           Data.Functor.Identity
import           Ouroboros.Consensus.Block.Abstract (HasHeader, Header,
                     HeaderFields (..), getHeaderFields)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node as Node
import           Ouroboros.Consensus.Node.InitStorage as Node
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB, Iterator,
                     IteratorResult (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl
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
      immutableDBArgs :: ImmutableDbArgs Identity IO block
      immutableDBArgs =
        (ImmutableDB.defaultArgs fs)
          { immTracer = immutableDBTracer
          , immRegistry = registry
          , immCheckIntegrity = nodeCheckIntegrity (configStorage config)
          , immCodecConfig = configCodec config
          , immChunkInfo = chunkInfo
          }

    withDB immutableDBArgs $ \(immutableDB, internal) -> do

      -- At the moment, we're just running a linear search with streamAll to
      -- find the correct block to truncate from, but we could in theory do this
      -- more quickly by binary searching the chunks of the ImmutableDB.
      iterator <- ImmutableDB.streamAll immutableDB registry
        ((,) <$> GetHeader <*> GetIsEBB)

      mTruncatePoint <- findNewTip truncateAfter iterator
      case mTruncatePoint of
        Nothing ->
          putStrLn $ mconcat
            [ "Unable to find a truncate point. This is likely because the tip "
            , "of the ImmutableDB has a slot number or block number less than "
            , "the intended truncate point"
            ]
        Just (header, isEBB) -> do
          let HeaderFields slotNo blockNo hash = getHeaderFields header
              newTip = ImmutableDB.Tip slotNo isEBB blockNo hash
          when verbose $ do
            putStrLn $ mconcat
              [ "Truncating the ImmutableDB using the following block as the "
              , "new tip:\n"
              , "  ", show newTip
              ]
          deleteAfter internal (At newTip)

-- | Given a 'TruncateAfter' (either a slot number or a block number), and an
-- iterator, find the last block whose slot or block number is less than or
-- equal to the intended new chain tip.
findNewTip :: forall m blk c.
              ( HasHeader (Header blk)
              , Monad m
#if __GLASGOW_HASKELL__ >= 904
              , HasHeader blk
#endif
              )
           => TruncateAfter
           -> Iterator m blk (Header blk, c)
           -> m (Maybe (Header blk, c))
findNewTip target iter =
    go Nothing
  where
    acceptable (getHeaderFields -> HeaderFields slotNo blockNo _, _) = do
      case target of
        TruncateAfterSlot s  -> slotNo <= s
        TruncateAfterBlock b -> blockNo <= b
    go acc =
      ImmutableDB.iteratorNext iter >>= \case
        IteratorExhausted -> do
          ImmutableDB.iteratorClose iter
          pure acc
        IteratorResult item -> do
          if acceptable item then go (Just item) else pure acc

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
