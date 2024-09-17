{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Stream (
    NextItem (..)
  , StreamAPI (..)
  , streamAPI
  , streamAPI'
  , streamAll
  ) where

import           Control.Monad.Except
import           GHC.Stack
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB hiding (streamAll)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

{-------------------------------------------------------------------------------
  Abstraction over the streaming API provided by the Chain DB
-------------------------------------------------------------------------------}

-- | Next item returned during streaming
data NextItem blk = NoMoreItems | NextItem blk

-- | Stream items from the immutable DB
--
-- When we initialize the ledger DB, we try to find a snapshot close to the
-- tip of the immutable DB, and then stream blocks from the immutable DB to its
-- tip to bring the ledger up to date with the tip of the immutable DB.
--
-- In CPS form to enable the use of 'withXYZ' style iterator init functions.
newtype StreamAPI m blk a = StreamAPI {
      -- | Start streaming after the specified block
      streamAfter :: forall b. HasCallStack
        => Point blk
        -- Reference to the block corresponding to the snapshot we found
        -- (or 'GenesisPoint' if we didn't find any)

        -> (Either (RealPoint blk) (m (NextItem a)) -> m b)
        -- Get the next item
        --
        -- Should be @Left pt@ if the snapshot we found is more recent than the
        -- tip of the immutable DB. Since we only store snapshots to disk for
        -- blocks in the immutable DB, this can only happen if the immutable DB
        -- got truncated due to disk corruption. The returned @pt@ is a
        -- 'RealPoint', not a 'Point', since it must always be possible to
        -- stream after genesis.
        -> m b
    }

-- | Stream all items
streamAll ::
     forall m blk e b a. (Monad m, HasCallStack)
  => StreamAPI m blk b
  -> Point blk             -- ^ Starting point for streaming
  -> (RealPoint blk -> e)  -- ^ Error when tip not found
  -> a                     -- ^ Starting point when tip /is/ found
  -> (b -> a -> m a)       -- ^ Update function for each item
  -> ExceptT e m a
streamAll StreamAPI{..} tip notFound e f = ExceptT $
    streamAfter tip $ \case
      Left tip' -> return $ Left (notFound tip')

      Right getNext -> do
        let go :: a -> m a
            go a = do mNext <- getNext
                      case mNext of
                        NoMoreItems -> return a
                        NextItem b  -> go =<< f b a
        Right <$> go e


streamAPI ::
     (IOLike m, HasHeader blk)
  => ImmutableDB m blk -> StreamAPI m blk blk
streamAPI = streamAPI' (return . NextItem) GetBlock

streamAPI' ::
     forall m blk a.
     (IOLike m, HasHeader blk)
  => (a -> m (NextItem a)) -- ^ Stop condition
  -> BlockComponent   blk a
  -> ImmutableDB    m blk
  -> StreamAPI      m blk a
streamAPI' shouldStop blockComponent immutableDB = StreamAPI streamAfter
  where
    streamAfter :: Point blk
                -> (Either (RealPoint blk) (m (NextItem a)) -> m b)
                -> m b
    streamAfter tip k = withRegistry $ \registry -> do
        eItr <-
          ImmutableDB.streamAfterPoint
            immutableDB
            registry
            blockComponent
            tip
        case eItr of
          -- Snapshot is too recent
          Left  err -> k $ Left  $ ImmutableDB.missingBlockPoint err
          Right itr -> k $ Right $ streamUsing itr

    streamUsing :: ImmutableDB.Iterator m blk a
                -> m (NextItem a)
    streamUsing itr = do
        itrResult <- ImmutableDB.iteratorNext itr
        case itrResult of
          ImmutableDB.IteratorExhausted -> return NoMoreItems
          ImmutableDB.IteratorResult b  -> shouldStop b
