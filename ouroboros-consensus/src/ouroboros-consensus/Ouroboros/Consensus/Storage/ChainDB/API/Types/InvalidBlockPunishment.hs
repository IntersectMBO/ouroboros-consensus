{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | How to punish the sender of a invalid block
module Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment (
    -- * opaque
    InvalidBlockPunishment
  , enact
    -- * combinators
  , Invalidity (..)
  , branch
  , mkForDiffusionPipelining
  , mkPunishThisThread
  , noPunishment
  ) where

import qualified Control.Exception as Exn
import           Control.Monad (join)
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike

-- | Is the added block itself invalid, or is its prefix invalid?
data Invalidity =
    BlockItself
  | BlockPrefix

-- | How to handle a discovered 'Invalidity'
--
-- This type is opaque because the soundness of the punishment is subtle because
-- of where it is invoked during the chain selection. As a result, arbitrary
-- monadic actions would be foot guns. Instead, this module defines a small DSL
-- for punishment that we judge to be sound.
newtype InvalidBlockPunishment m = InvalidBlockPunishment {
    enact :: Invalidity -> m ()
  }
  deriving NoThunks via
    OnlyCheckWhnfNamed "InvalidBlockPunishment" (InvalidBlockPunishment m)

-- | A noop punishment
noPunishment :: Applicative m => InvalidBlockPunishment m
noPunishment = InvalidBlockPunishment $ \_invalidity -> pure ()

-- | Create a punishment that kills this thread
mkPunishThisThread :: IOLike m => m (InvalidBlockPunishment m)
mkPunishThisThread = do
    tid <- myThreadId
    pure $ InvalidBlockPunishment $ \_invalidity ->
      throwTo tid PeerSentAnInvalidBlockException

-- | Thrown asynchronously to the client thread that added the block whose
-- processing involved an invalid block.
--
-- See 'punishThisThread'.
data PeerSentAnInvalidBlockException = PeerSentAnInvalidBlockException
  deriving (Show)

instance Exn.Exception PeerSentAnInvalidBlockException

-- | Allocate a stateful punishment that performs the given punishment if the
-- given header does not satisfy the diffusion pipelining criterion.
mkForDiffusionPipelining :: forall m blk.
     ( IOLike m
     , BlockSupportsDiffusionPipelining blk
     )
  => STM m (   BlockConfig blk
            -> Header blk
            -> InvalidBlockPunishment m
            -> InvalidBlockPunishment m
           )
mkForDiffusionPipelining = do
    var <- newTVar (initialTentativeHeaderState (Proxy @blk))
    pure $ \cfg new punish -> InvalidBlockPunishment $ \invalidity -> join $ atomically $ do
      mbSt' <- updateTentativeHeaderState cfg new <$> readTVar var
      case mbSt' of
        Just st' -> do
          writeTVar var st'
          pure $ pure ()
        Nothing  ->
          pure $ enact punish invalidity

-- | Punish according to the 'Invalidity'
branch :: (Invalidity -> InvalidBlockPunishment m) -> InvalidBlockPunishment m
branch f = InvalidBlockPunishment $ \invalidity ->
    enact (f invalidity) invalidity
