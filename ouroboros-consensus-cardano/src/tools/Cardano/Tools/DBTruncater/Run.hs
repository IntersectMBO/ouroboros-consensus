{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBTruncater.Run ( truncate ) where

import Control.Monad
import Cardano.Slotting.Slot (WithOrigin(..), SlotNo)
import Cardano.Tools.DBAnalyser.HasAnalysis
import Cardano.Tools.DBTruncater.Types
import Control.Tracer
import Data.Functor.Identity
import Ouroboros.Consensus.Block.Abstract (HeaderFields(..), getBlockHeaderFields)
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Node as Node
import Ouroboros.Consensus.Node.InitStorage as Node
import Ouroboros.Consensus.Storage.Common
import Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB, Iterator, IteratorResult(..))
import Ouroboros.Consensus.Storage.ImmutableDB.Impl
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.ResourceRegistry (withRegistry, runWithTempRegistry)
import Prelude hiding (truncate)
import System.IO
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB

truncate :: forall block .
            ( Node.RunNode block, HasProtocolInfo block )
         => DBTruncaterConfig -> Args block -> IO ()
truncate DBTruncaterConfig{ dbDir, truncatePoint, verbose } args = do
  withRegistry $ \registry -> do
    lock <- mkLock
    immutableDBTracer <- mkTracer lock verbose
    ProtocolInfo {
      pInfoConfig = config
    } <- mkProtocolInfo args
    let
      TruncatePoint truncateSlotNo = truncatePoint
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
      iterator <- ImmutableDB.streamAll immutableDB registry
        ((,,) <$> GetSlot <*> GetBlock <*> GetIsEBB)

      -- Here we're specifically looking for the *first* "block" (i.e. real
      -- block or EBB) of the latest slot with number less than the truncate
      -- point.
      mTruncatePoint <- findTruncatePoint truncateSlotNo iterator
      case mTruncatePoint of
        Nothing ->
          putStrLn "Unable to find a truncate point. This is likely because the tip of the ImmutableDB has a slot number less than the intended truncate point"
        Just (slotNo, block, isEBB) -> do
          let HeaderFields _ blockNo hash = getBlockHeaderFields block
              newTip = ImmutableDB.Tip slotNo isEBB blockNo hash
          when verbose $ do
            putStrLn "Truncating the ImmutableDB using the following block as the new tip:"
            putStrLn $ "  " <> show newTip
          deleteAfter internal (At newTip)

findTruncatePoint :: Monad m => SlotNo -> Iterator m blk (SlotNo, b, c) -> m (Maybe (SlotNo, b, c))
findTruncatePoint target iter = go Nothing
  where
    go acc =
      ImmutableDB.iteratorNext iter >>= \case
        IteratorExhausted -> do
          ImmutableDB.iteratorClose iter
          pure acc
        IteratorResult new@(slotNo, _, _) -> do
          if slotNo > target
            then pure acc
            else
              case acc of
                Just lastBlock@(lastSeenSlotNo, _, _) ->
                  if slotNo > lastSeenSlotNo then go (Just new) else go (Just lastBlock)
                Nothing ->
                  go (Just new)

mkLock :: MonadSTM m => m (StrictMVar m ())
mkLock = newMVar ()

mkTracer :: (Show a) => StrictMVar IO () -> Bool -> IO (Tracer IO a)
mkTracer _ False = pure mempty
mkTracer lock True = do
  startTime <- getMonotonicTime
  pure $ Tracer $ \ev -> do
    bracket_ (takeMVar lock) (putMVar lock ()) $ do
      traceTime <- getMonotonicTime
      let diff = diffTime traceTime startTime
      hPutStrLn stderr $ concat ["[", show diff, "] ", show ev]
      hFlush stderr

withDB :: ( Node.RunNode block, IOLike m )
       => ImmutableDbArgs Identity m block
       -> ((ImmutableDB m block, Internal m block) -> m a)
       -> m a
withDB immutableDBArgs = bracket (ImmutableDB.openDBInternal immutableDBArgs runWithTempRegistry) (ImmutableDB.closeDB . fst)
