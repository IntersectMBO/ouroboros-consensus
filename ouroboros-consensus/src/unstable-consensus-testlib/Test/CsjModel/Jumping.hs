{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.CsjModel.Jumping (module Test.CsjModel.Jumping) where

import           Cardano.Slotting.Slot (WithOrigin)
import           Data.Foldable (for_)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block.Abstract (Header, Point)
import           Ouroboros.Consensus.Block.RealPoint
import           Ouroboros.Consensus.Block.SupportsProtocol
                     (BlockSupportsProtocol)
import qualified Ouroboros.Consensus.HeaderValidation as HV
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
                    (ImmutableJumpInfo (..), JumpInfo, immutableJumpInfo,
                     jTheirFragment)
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader)
import           Test.CsjModel

deriving instance NoThunks a => NoThunks (Strict.Maybe a)

-----

type F f blk = f (RealPoint blk) (JumpInfo blk)

type Inbox m blk = StrictTVar m (Strict.Maybe (F CsjReaction blk))

-----

registerClient ::
     (
       Ord pid, Show pid
     ,
       Show (Header blk)
     ,
       BlockSupportsProtocol blk, LedgerSupportsProtocol blk
     ,
       IOLike m
     )
  => STM m (WithOrigin (RealPoint blk))
     -- ^ current immutable tip
  -> CsjEnv (RealPoint blk)
  -> StrictTVar m (F (CsjState pid) blk)
  -> StrictTVar m (Map pid (Inbox m blk))
     -- ^ CSJ client inboxes
  -> ImmutableJumpInfo blk
  -> pid
  -> STM m (Jumping m blk)
registerClient getImmTip env varCsj varInboxes immJumpInfo pid = do
    inbox <- newTVar Strict.Nothing
    modifyTVar varInboxes $ Map.insert pid inbox
    stepSTM $ Connect $ immutableJumpInfo immJumpInfo
    pure $ csjModelJumping getImmTip env varCsj varInboxes pid
  where
    ImmutableJumpInfo immAnchor _immHdrSt = immJumpInfo

    imm = pointToWithOriginRealPoint $ AF.anchorToPoint immAnchor

    stepSTM = stimulateSTM env varCsj varInboxes pid imm

unregisterClient ::
     (
       Ord pid, Show pid
     ,
       Show (Header blk)
     ,
       BlockSupportsProtocol blk, HV.HasAnnTip blk
     ,
       IOLike m
     )
  => CsjEnv (RealPoint blk)
  -> StrictTVar m (F (CsjState pid) blk)
  -> StrictTVar m (Map pid (Inbox m blk))
     -- ^ CSJ client inboxes
  -> pid
  -> WithOrigin (RealPoint blk)
     -- ^ current immutable tip
  -> STM m ()
unregisterClient env varCsj varInboxes pid imm = do
    stepSTM Disconnect
    modifyTVar varInboxes $ Map.delete pid
  where
    stepSTM = stimulateSTM env varCsj varInboxes pid imm

rotateDynamo ::
    (
       Ord pid, Show pid
     ,
       Show (Header blk)
     ,
       BlockSupportsProtocol blk, HV.HasAnnTip blk
     ,
       IOLike m
     )
  => CsjEnv (RealPoint blk)
  -> StrictTVar m (F (CsjState pid) blk)
  -> StrictTVar m (Map pid (Inbox m blk))
  -> pid
  -> WithOrigin (RealPoint blk)
     -- ^ current immutable tip
  -> STM m ()
rotateDynamo env varCsj varInboxes pid imm = do
    stepSTM Starvation
  where
    stepSTM = stimulateSTM env varCsj varInboxes pid imm

csjModelJumping ::
     (
       Ord pid, Show pid
     ,
       Show (Header blk)
     ,
       BlockSupportsProtocol blk, HV.HasAnnTip blk
     ,
       IOLike m
     )
  => STM m (WithOrigin (RealPoint blk))
     -- ^ current immutable tip
  -> CsjEnv (RealPoint blk)
  -> StrictTVar m (F (CsjState pid) blk)
  -> StrictTVar m (Map pid (Inbox m blk))
     -- ^ CSJ client inboxes
  -> pid
  -> Jumping m blk
csjModelJumping getImmTip env varCsj varInboxes pid = Jumping {
    jgNextInstruction   = atomically $ do
        inboxes <- readTVar varInboxes
        let inbox = inboxes Map.! pid
        reaction <- readTVar inbox >>= \case
            Strict.Nothing       -> retry
            Strict.Just reaction -> pure reaction
        writeTVar inbox Strict.Nothing
        case reaction of
            Continue                 -> pure RunNormally
            Disengage                -> pure RunNormally
            MsgFindIntersect purp wp -> do
                stepSTM Offered
                let f = case purp of
                        ToBisect  -> JumpTo          
                        ToPromote -> JumpToGoodPoint
                pure $ JumpInstruction $ f $ wpPayload wp
            Stop p                   ->
                pure $ StopRunningNormally $ withOriginRealPointToPoint p
  ,
    jgOnAwaitReply      = step $ ChainSyncReply MsgAwaitReply
  ,
    jgOnRecvRollForward =
        step . ChainSyncReply . MsgRollForwardSTART . castRealPoint
  ,
    jgOnRollForward     = \p ji -> atomically $ do
        -- Suppress this stimulus if CSJ has already sent 'Stop'; this
        -- ChainSync client is no longer the Dynamo---it just hasn't noticed
        -- yet.
        inboxes <- readTVar varInboxes
        let inbox = inboxes Map.! pid
        readTVar inbox >>= \case
            Strict.Just Stop{} -> pure ()
            _                  ->
                stepSTM
              $ ChainSyncReply . MsgRollForwardDONE
              $ mkWithPayload p ji
  ,
    jgOnRollBackward    =
        step . ChainSyncReply . MsgRollBackward . pointToWithOriginRealPoint
  ,
    jgProcessJumpResult = \jr ->
          fmap (\() -> pure ())
        $ stepSTM . ChainSyncReply
        $ case jr of
            AcceptedJump{} -> MsgIntersectFound
            RejectedJump{} -> MsgIntersectNotFound
  ,
    jgUpdateJumpInfo    = \_ji -> pure ()
      --  "Test.CsjModel" relies on 'jgOnRollForward' instead of
      --  'jgUpdateJumpInfo'
  }
  where
    step = atomically . stepSTM

    stepSTM stimulus =  do
        imm <- getImmTip
        stimulateSTM env varCsj varInboxes pid imm stimulus

-----

stimulateSTM :: 
     (
       Ord pid, Show pid
     ,
       Show (Header blk)
     ,
       BlockSupportsProtocol blk, HV.HasAnnTip blk
     ,
       IOLike m
     )
  => CsjEnv (RealPoint blk)
  -> StrictTVar m (F (CsjState pid) blk)
  -> StrictTVar m (Map pid (Inbox m blk))
     -- ^ CSJ client inboxes
  -> pid
  -> WithOrigin (RealPoint blk)
     -- ^ current immutable tip
  -> F CsjStimulus blk
  -> STM m ()
stimulateSTM env varCsj varInboxes pid imm stimulus = do
    x <- readTVar varCsj
    case csjReactions env x pid imm stimulus of
        Nothing         -> throwSTM $ CsjModelStuck imm x pid stimulus
        Just (x', msgs) -> do
            writeTVar varCsj x'
            inboxes <- readTVar varInboxes
            for_ msgs $ \(pid', reaction) -> do
                writeTVar (inboxes Map.! pid') $ Strict.Just reaction

data CsjModelException =
    forall pid p a.
     (
       Show pid
     ,
       Show p
     ,
       Show a
     ) => CsjModelStuck
       (WithOrigin p)
       (CsjState pid p a)
       pid
       (CsjStimulus p a)

deriving instance Show CsjModelException

instance Exception CsjModelException

jumpTip ::
     (HasHeader (Header blk), Typeable blk)
  => JumpInfo blk -> Point blk
jumpTip = AF.castPoint . AF.headPoint . jTheirFragment
