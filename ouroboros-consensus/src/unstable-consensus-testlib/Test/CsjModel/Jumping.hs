{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.CsjModel.Jumping (module Test.CsjModel.Jumping) where

import           Cardano.Slotting.Slot (WithOrigin)
import           Control.Tracer (Tracer, traceWith)
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
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping hiding
                     (tracer)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
                     (ImmutableJumpInfo (..), JumpInfo (..), immutableJumpInfo,
                     jTheirFragment)
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader)
import           Test.CsjModel
import           Test.CsjModel.StateTypes (CsjState (..), FullTrim (fullTrim))

-----

type F f blk = f (RealPoint blk) (JumpInfo blk)

type Inbox m blk = StrictTVar m (Strict.Maybe (F CsjReaction blk))

-----

data CsjModelEvent pid blk =
    CsjModelEvent
        pid
        (F CsjStimulus blk)
        (WithOrigin (RealPoint blk))
        [(pid, F CsjReaction blk)]
        (F (CsjState pid) blk)
        (F (CsjState pid) blk)

deriving instance (BlockSupportsProtocol blk, HV.HasAnnTip blk, Show pid, Show (Header blk)) => Show (CsjModelEvent pid blk)

prettyCsjModelEvent ::
     (BlockSupportsProtocol blk, HV.HasAnnTip blk, Show pid, Show (Header blk))
  => CsjModelEvent pid blk
  -> String
prettyCsjModelEvent ev =
    unlines
  $ (("CsjModelEvent " ++ show pid) :)
  $ map ("    " ++)
  $ ([ show stimulus, show imm, show msgs] <>)
  $ pretty (fullTrim $ fmap discardJI x')
  where
    CsjModelEvent pid stimulus imm msgs _x x' = ev

    discardJI :: JumpInfo blk -> JI
    discardJI = const JI

    pretty z =
        ("CsjState" :)
      $ map ("    " ++)
      $ [ "disengaged = " ++ show (disengaged z)
        , "queue      = " ++ show (queue z)
        , "latestJump = " ++ show (latestJump z)
        , "dynamo     = " ++ show (dynamo z)
        ]
        <>
        (("nonDynamos =" :) $ map ("    " ++) $ map show $ Map.assocs $ nonDynamos z)

data JI = JI
  deriving (Show)

instance FullTrim JI where fullTrim JI = JI

-----

registerClient ::
     (
       Ord pid, Show pid
     ,
       Show (Header blk)
     ,
       LedgerSupportsProtocol blk
     ,
       IOLike m
     ,
       forall x. NoThunks x => NoThunks (Strict.Maybe x)
     )
  => Tracer m (CsjModelEvent pid blk)
  -> STM m (WithOrigin (RealPoint blk))
     -- ^ current immutable tip
  -> CsjEnv (RealPoint blk)
  -> StrictTVar m (F (CsjState pid) blk)
  -> StrictTVar m (Map pid (Inbox m blk))
     -- ^ CSJ client inboxes
  -> ImmutableJumpInfo blk
  -> pid
  -> STM m (Jumping m blk, STM m (m ()), m ())
registerClient tracer getImmTip env varCsj varInboxes immJumpInfo pid = do
    inbox <- newTVar Strict.Nothing
    modifyTVar varInboxes $ Map.insert pid inbox
    ev <- stepSTM $ Connect $ immutableJumpInfo immJumpInfo
    pure ( Test.CsjModel.Jumping.mkJumping tracer getImmTip env varCsj varInboxes pid
         , do
               imm' <- getImmTip
               ev' <- Test.CsjModel.Jumping.unregisterClient env varCsj varInboxes pid imm'
               pure $ traceWith tracer ev'
         , traceWith tracer ev
         )
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
  -> STM m (CsjModelEvent pid blk)
unregisterClient env varCsj varInboxes pid imm = do
    ev <- stepSTM Disconnect
    modifyTVar varInboxes $ Map.delete pid
    pure ev
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
  => Tracer m (CsjModelEvent pid blk)
  -> CsjEnv (RealPoint blk)
  -> StrictTVar m (F (CsjState pid) blk)
  -> StrictTVar m (Map pid (Inbox m blk))
  -> pid
  -> WithOrigin (RealPoint blk)
     -- ^ current immutable tip
  -> STM m (m ())
rotateDynamo tracer env varCsj varInboxes pid imm = do
    traceWith tracer <$> stepSTM Starvation
  where
    stepSTM = stimulateSTM env varCsj varInboxes pid imm

mkJumping ::
     (
       Ord pid, Show pid
     ,
       Show (Header blk)
     ,
       BlockSupportsProtocol blk, HV.HasAnnTip blk
     ,
       IOLike m
     )
  => Tracer m (CsjModelEvent pid blk)
  -> STM m (WithOrigin (RealPoint blk))
     -- ^ current immutable tip
  -> CsjEnv (RealPoint blk)
  -> StrictTVar m (F (CsjState pid) blk)
  -> StrictTVar m (Map pid (Inbox m blk))
     -- ^ CSJ client inboxes
  -> pid
  -> Jumping m blk
mkJumping tracer getImmTip env varCsj varInboxes pid = Jumping {
    jgNextInstruction   = \whetherZero -> do
        (mbEv, instr) <- loopy $ do
            inboxes <- readTVar varInboxes
            let inbox = inboxes Map.! pid
            reaction <- readTVar inbox >>= \case
                Strict.Nothing       -> retry
                Strict.Just reaction -> pure reaction
            case reaction of
                Demoting                 -> case whetherZero of
                    NoRepliesOutstanding   -> do
                        writeTVar inbox Strict.Nothing
                        ev <- stepSTM Demoted
                        pure $ Left $ traceWith tracer ev
                    SomeRepliesOutstanding -> pure $ Right (Nothing, StopRunningNormally)
                Disengage           -> pure $ Right (Nothing, RunNormally)
                MsgFindIntersect wp -> do
                    writeTVar inbox Strict.Nothing
                    ev <- stepSTM Offered
                    pure $ Right (Just ev, JumpInstruction $ JumpTo $ wpPayload wp)
                Nevermind                -> do
                    writeTVar inbox Strict.Nothing
                    pure $ Left $ pure ()
                Promoted                 -> pure $ Right (Nothing, RunNormally)
        whenJust mbEv $ \ev -> traceWith tracer ev
        pure instr
  ,
    jgOnAwaitReply      = step $ ChainSyncReply MsgAwaitReply
  ,
    jgOnRecvRollForward =
        step . ChainSyncReply . MsgRollForwardSTART . castRealPoint
  ,
    jgOnRollForward     = \p ji ->
        step
          $ ChainSyncReply . MsgRollForwardDONE
          $ mkWithPayload p ji
  ,
    jgOnRollBackward    =
        step . ChainSyncReply . MsgRollBackward . pointToWithOriginRealPoint
  ,
    jgProcessJumpResult = \jr ->
          fmap (traceWith tracer)
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
    step stimulus = do
      ev <- atomically $ stepSTM stimulus
      traceWith tracer ev

    stepSTM stimulus =  do
        imm <- getImmTip
        stimulateSTM env varCsj varInboxes pid imm stimulus

loopy :: IOLike m => STM m (Either (m ()) a) -> m a
loopy m = atomically m >>= \case
    Left io -> do io; loopy m
    Right x -> pure x

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
  -> STM m (CsjModelEvent pid blk)
stimulateSTM env varCsj varInboxes pid imm stimulus = do
    x <- readTVar varCsj
    case csjReactions env x pid imm stimulus of
        Nothing         -> throwSTM $ CsjModelStuck pid stimulus imm x
        Just (x', msgs) -> do
            writeTVar varCsj x'
            inboxes <- readTVar varInboxes
            -- Process the messages backwards because the model adds new
            -- messages by just consing them onto the front of the list.
            for_ (reverse msgs) $ \(pid', reaction) -> do
                writeTVar (inboxes Map.! pid') $ Strict.Just reaction
            pure $ CsjModelEvent pid stimulus imm msgs x x'

data CsjModelException =
    forall pid p a.
     (
       Show pid
     ,
       Show p
     ,
       Show a
     ) => CsjModelStuck
       pid
       (CsjStimulus p a)
       (WithOrigin p)
       (CsjState pid p a)

deriving instance Show CsjModelException

instance Exception CsjModelException

jumpTip ::
     (HasHeader (Header blk), Typeable blk)
  => JumpInfo blk -> Point blk
jumpTip = AF.castPoint . AF.headPoint . jTheirFragment
