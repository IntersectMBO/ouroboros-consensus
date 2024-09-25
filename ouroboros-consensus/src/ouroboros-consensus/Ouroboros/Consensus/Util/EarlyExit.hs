{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Util.EarlyExit (
    exitEarly
  , withEarlyExit
  , withEarlyExit_
    -- * Re-exports
  , lift
    -- * opaque
  , WithEarlyExit
  ) where

import           Control.Applicative
import           Control.Concurrent.Class.MonadMVar
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadEventlog
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Internal
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer
import qualified Control.Monad.Class.MonadTimer.SI as TimerSI
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Function (on)
import           Data.Proxy
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike (IOLike (..), PrimMonad (..),
                     StrictSVar, StrictTVar, castStrictSVar, castStrictTVar)
import           Ouroboros.Consensus.Util.NormalForm.StrictMVar (StrictMVar)

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

newtype WithEarlyExit m a = WithEarlyExit {
      unWithEarlyExit :: MaybeT m a
    }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadTrans
           , MonadPlus
           )

instance NoThunks (StrictTVar m a)
      => NoThunks (StrictTVar (WithEarlyExit m) a) where
  showTypeOf _ = "StrictTVar (WithEarlyExit m)"
  wNoThunks ctxt tv = do
      wNoThunks ctxt (castStrictTVar tv :: StrictTVar m a)

instance NoThunks (StrictSVar m a)
      => NoThunks (StrictSVar (WithEarlyExit m) a) where
  showTypeOf _ = "StrictSVar (WithEarlyExit m)"
  wNoThunks ctxt tv = do
      wNoThunks ctxt (castStrictSVar tv :: StrictSVar m a)

-- | Internal only
earlyExit :: m (Maybe a) -> WithEarlyExit m a
earlyExit = WithEarlyExit . MaybeT

withEarlyExit :: WithEarlyExit m a -> m (Maybe a)
withEarlyExit = runMaybeT . unWithEarlyExit

withEarlyExit_ :: Functor m => WithEarlyExit m () -> m ()
withEarlyExit_ = fmap collapse . withEarlyExit

collapse :: Maybe () -> ()
collapse Nothing   = ()
collapse (Just ()) = ()

exitEarly :: Applicative m => WithEarlyExit m a
exitEarly = earlyExit $ pure Nothing

instance (forall a'. NoThunks (m a'))
      => NoThunks (WithEarlyExit m a) where
   showTypeOf _p = "WithEarlyExit " ++ showTypeOf (Proxy @(m a))
   wNoThunks ctxt = wNoThunks ctxt . withEarlyExit

{-------------------------------------------------------------------------------
  Instances for io-classes
-------------------------------------------------------------------------------}

instance MonadSTM m => MonadSTM (WithEarlyExit m) where
  type STM (WithEarlyExit m) = WithEarlyExit (STM m)
  atomically                 = earlyExit . atomically . withEarlyExit

  type TVar    (WithEarlyExit m) = TVar    m
  type TMVar   (WithEarlyExit m) = TMVar   m
  type TQueue  (WithEarlyExit m) = TQueue  m
  type TBQueue (WithEarlyExit m) = TBQueue m
  type TArray  (WithEarlyExit m) = TArray  m
  type TSem    (WithEarlyExit m) = TSem    m
  type TChan   (WithEarlyExit m) = TChan   m

  newTVar         = lift .  newTVar
  readTVar        = lift .  readTVar
  writeTVar       = lift .: writeTVar
  retry           = lift    retry
  orElse          = (earlyExit .: orElse) `on` withEarlyExit
  newTMVar        = lift .  newTMVar
  newEmptyTMVar   = lift    newEmptyTMVar
  takeTMVar       = lift .  takeTMVar
  tryTakeTMVar    = lift .  tryTakeTMVar
  putTMVar        = lift .: putTMVar
  tryPutTMVar     = lift .: tryPutTMVar
  readTMVar       = lift .  readTMVar
  writeTMVar      = lift .: writeTMVar
  tryReadTMVar    = lift .  tryReadTMVar
  swapTMVar       = lift .: swapTMVar
  isEmptyTMVar    = lift .  isEmptyTMVar
  newTQueue       = lift    newTQueue
  readTQueue      = lift .  readTQueue
  tryReadTQueue   = lift .  tryReadTQueue
  peekTQueue      = lift .  peekTQueue
  tryPeekTQueue   = lift .  tryPeekTQueue
  flushTQueue     = lift .  flushTQueue
  writeTQueue     = lift .: writeTQueue
  isEmptyTQueue   = lift .  isEmptyTQueue
  unGetTQueue     = lift .: unGetTQueue
  newTBQueue      = lift .  newTBQueue
  readTBQueue     = lift .  readTBQueue
  tryReadTBQueue  = lift .  tryReadTBQueue
  peekTBQueue     = lift .  peekTBQueue
  tryPeekTBQueue  = lift .  tryPeekTBQueue
  flushTBQueue    = lift .  flushTBQueue
  writeTBQueue    = lift .: writeTBQueue
  lengthTBQueue   = lift .  lengthTBQueue
  isEmptyTBQueue  = lift .  isEmptyTBQueue
  isFullTBQueue   = lift .  isFullTBQueue
  unGetTBQueue    = lift .: unGetTBQueue
  newTSem         = lift .  newTSem
  waitTSem        = lift .  waitTSem
  signalTSem      = lift .  signalTSem
  signalTSemN     = lift .: signalTSemN

  newTChan          = lift    newTChan
  newBroadcastTChan = lift    newBroadcastTChan
  dupTChan          = lift .  dupTChan
  cloneTChan        = lift .  cloneTChan
  readTChan         = lift .  readTChan
  tryReadTChan      = lift .  tryReadTChan
  peekTChan         = lift .  peekTChan
  tryPeekTChan      = lift .  tryPeekTChan
  writeTChan        = lift .: writeTChan
  unGetTChan        = lift .: unGetTChan
  isEmptyTChan      = lift .  isEmptyTChan

  newTMVarIO      = lift . newTMVarIO
  newEmptyTMVarIO = lift   newEmptyTMVarIO

instance (MonadMVar m, MonadMask m, MonadEvaluate m)
      => MonadMVar (WithEarlyExit m) where
  type MVar (WithEarlyExit m) = MVar m

  newEmptyMVar          = lift    newEmptyMVar
  takeMVar              = lift .  takeMVar
  putMVar               = lift .: putMVar
  tryTakeMVar           = lift .  tryTakeMVar
  tryPutMVar            = lift .: tryPutMVar
  tryReadMVar           = lift .  tryReadMVar
  isEmptyMVar           = lift .  isEmptyMVar

  newMVar               = lift .  newMVar
  readMVar              = lift .  readMVar
  swapMVar              = lift .: swapMVar

instance MonadCatch m => MonadThrow (WithEarlyExit m) where
  throwIO = lift . throwIO
#if __GLASGOW_HASKELL__ >= 910
  -- This method is defined in the io-classes package (part of the io-sim repository) where
  -- it is guarded by the GHC version as above.
  annotateIO = annotateIO
#endif

instance MonadCatch m => MonadCatch (WithEarlyExit m) where
  catch act handler = earlyExit $
      catch (withEarlyExit act) (withEarlyExit . handler)

  generalBracket acquire release use = earlyExit $ do
      -- This is modelled on the case for ErrorT, except that we don't have
      -- to worry about reporting the right error, since we only have @Nothing@
      (mb, mc) <- generalBracket
                    (withEarlyExit acquire)
                    (\mResource exitCase ->
                        case (mResource, exitCase) of
                          (Nothing, _) ->
                            -- resource not acquired
                            return Nothing
                          (Just resource, ExitCaseSuccess (Just b)) ->
                            withEarlyExit $ release resource (ExitCaseSuccess b)
                          (Just resource, ExitCaseException e) ->
                            withEarlyExit $ release resource (ExitCaseException e)
                          (Just resource, _otherwise) ->
                            withEarlyExit $ release resource ExitCaseAbort
                    )
                    (maybe (return Nothing) (withEarlyExit . use))
      return $ (,) <$> mb <*> mc

instance MonadMask m => MonadMask (WithEarlyExit m) where
  mask f = earlyExit $
    mask $ \unmask ->
      withEarlyExit (f (earlyExit . unmask . withEarlyExit))

  uninterruptibleMask f = earlyExit $
    uninterruptibleMask $ \unmask ->
      let unmask' :: forall a. WithEarlyExit m a -> WithEarlyExit m a
          unmask' = earlyExit . unmask . withEarlyExit
      in withEarlyExit (f unmask')

instance MonadThread m => MonadThread (WithEarlyExit m) where
  type ThreadId (WithEarlyExit m) = ThreadId m

  myThreadId   = lift    myThreadId
  labelThread  = lift .: labelThread

instance (MonadMask m, MonadAsync m, MonadCatch (STM m))
      => MonadAsync (WithEarlyExit m) where
  type Async (WithEarlyExit m) = WithEarlyExit (Async m)

  async            = lift . (fmap earlyExit . async) . withEarlyExit
  asyncBound       = lift . (fmap earlyExit . async) . withEarlyExit
  asyncOn n        = lift . (fmap earlyExit . asyncOn n) . withEarlyExit
  asyncThreadId    = asyncThreadId
  cancel        a  = lift $ cancel     (withEarlyExit a)
  cancelWith    a  = lift . cancelWith (withEarlyExit a)

  waitCatchSTM a = earlyExit (commute      <$> waitCatchSTM (withEarlyExit a))
  pollSTM      a = earlyExit (fmap commute <$> pollSTM      (withEarlyExit a))

  asyncWithUnmask f = earlyExit $ fmap (Just . earlyExit) $
    asyncWithUnmask $ \unmask ->
      withEarlyExit (f (earlyExit . unmask . withEarlyExit))

  asyncOnWithUnmask n f = earlyExit $ fmap (Just . earlyExit) $
    asyncOnWithUnmask n $ \unmask ->
      withEarlyExit (f (earlyExit . unmask . withEarlyExit))

commute :: Either SomeException (Maybe a) -> Maybe (Either SomeException a)
commute (Left e)         = Just (Left e)
commute (Right Nothing)  = Nothing
commute (Right (Just a)) = Just (Right a)

instance MonadFork m => MonadFork (WithEarlyExit m) where
  forkIO           f = lift $ forkIO (collapse <$> withEarlyExit f)
  forkOn n         f = lift $ forkOn n (collapse <$> withEarlyExit f)
  forkIOWithUnmask f = lift $ forkIOWithUnmask $ \unmask ->
                         let unmask' :: forall a. WithEarlyExit m a -> WithEarlyExit m a
                             unmask' = earlyExit . unmask . withEarlyExit
                         in collapse <$> withEarlyExit (f unmask')
  forkFinally  f fin = lift $ forkFinally
                                (withEarlyExit f)
                                (withEarlyExit_ . maybe (pure ()) fin . commute)

  throwTo            = lift .: throwTo
  yield              = lift yield


instance PrimMonad m => PrimMonad (WithEarlyExit m) where
  type PrimState (WithEarlyExit m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance MonadST m => MonadST (WithEarlyExit m) where
  stToIO       = lift . stToIO
  withLiftST k = k stToIO


instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (WithEarlyExit m) where
  getMonotonicTimeNSec = lift getMonotonicTimeNSec


instance MonadMonotonicTime m => MonadMonotonicTime (WithEarlyExit m) where
  getMonotonicTime = lift getMonotonicTime

instance MonadDelay m => MonadDelay (WithEarlyExit m) where
  threadDelay = lift . threadDelay


instance TimerSI.MonadDelay m => TimerSI.MonadDelay (WithEarlyExit m) where
  threadDelay = lift . TimerSI.threadDelay

instance (MonadEvaluate m, MonadCatch m) => MonadEvaluate (WithEarlyExit m) where
  evaluate  = lift . evaluate

instance MonadEventlog m => MonadEventlog (WithEarlyExit m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

instance MonadLabelledSTM m => MonadLabelledSTM (WithEarlyExit m) where
  labelTVar      = lift .: labelTVar
  labelTMVar     = lift .: labelTMVar
  labelTQueue    = lift .: labelTQueue
  labelTBQueue   = lift .: labelTBQueue
  labelTArray    = lift .: labelTArray
  labelTSem      = lift .: labelTSem
  labelTChan     = lift .: labelTChan
  labelTVarIO    = lift .: labelTVarIO
  labelTMVarIO   = lift .: labelTMVarIO
  labelTQueueIO  = lift .: labelTQueueIO
  labelTBQueueIO = lift .: labelTBQueueIO
  labelTArrayIO  = lift .: labelTArrayIO
  labelTSemIO    = lift .: labelTSemIO
  labelTChanIO   = lift .: labelTChanIO

instance MonadSay m => MonadSay (WithEarlyExit m) where
  say = lift . say

instance (MonadInspectSTM m, Monad (InspectMonad m)) =>  MonadInspectSTM (WithEarlyExit m) where
    type InspectMonad (WithEarlyExit m) = InspectMonad m
    inspectTVar  _ = inspectTVar (Proxy @m)
    inspectTMVar _ = inspectTMVar (Proxy @m)

instance MonadTraceSTM m => MonadTraceSTM (WithEarlyExit m) where
  traceTVar    _ = lift .: traceTVar (Proxy @m)
  traceTMVar   _ = lift .: traceTMVar (Proxy @m)
  traceTQueue  _ = lift .: traceTQueue (Proxy @m)
  traceTBQueue _ = lift .: traceTBQueue (Proxy @m)
  traceTSem    _ = lift .: traceTSem (Proxy @m)

{-------------------------------------------------------------------------------
  Finally, the consensus IOLike wrapper
-------------------------------------------------------------------------------}

instance ( IOLike m
         , forall a. NoThunks (StrictTVar (WithEarlyExit m) a)
         , forall a. NoThunks (StrictSVar (WithEarlyExit m) a)
         , forall a. NoThunks (StrictMVar (WithEarlyExit m) a)
         ) => IOLike (WithEarlyExit m) where
  forgetSignKeyKES = lift . forgetSignKeyKES
