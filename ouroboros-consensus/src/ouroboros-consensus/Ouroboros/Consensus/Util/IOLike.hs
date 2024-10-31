{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Util.IOLike (
    IOLike (..)
    -- * Re-exports
    -- *** MonadThrow
  , Exception (..)
  , ExitCase (..)
  , MonadCatch (..)
  , MonadMask (..)
  , MonadThrow (..)
  , SomeException
    -- *** Variables with NoThunks invariants
  , module Ouroboros.Consensus.Util.MonadSTM.NormalForm
  , module Ouroboros.Consensus.Util.NormalForm.StrictMVar
  , module Ouroboros.Consensus.Util.NormalForm.StrictTVar
    -- *** MonadFork, TODO: Should we hide this in favour of MonadAsync?
  , MonadFork (..)
  , MonadThread (..)
  , labelThisThread
    -- *** MonadAsync
  , ExceptionInLinkedThread (..)
  , MonadAsync (..)
  , link
  , linkTo
    -- *** MonadST
  , MonadST (..)
  , PrimMonad (..)
    -- *** MonadTime
  , DiffTime
  , MonadMonotonicTime (..)
  , Time (..)
  , addTime
  , diffTime
    -- *** MonadDelay
  , MonadDelay (..)
    -- *** MonadEventlog
  , MonadEventlog (..)
    -- *** MonadEvaluate
  , MonadEvaluate (..)
    -- *** NoThunks
  , NoThunks (..)
  ) where

import           Cardano.Crypto.KES (KESAlgorithm, SignKeyKES)
import qualified Cardano.Crypto.KES as KES
import           Control.Applicative (Alternative)
import           Control.Concurrent.Class.MonadMVar (MonadInspectMVar (..))
import qualified Control.Concurrent.Class.MonadMVar.Strict as Strict
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadEventlog
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Monad.Primitive
import           Data.Functor (void)
import           Data.Proxy (Proxy (..))
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
import           Ouroboros.Consensus.Util.NormalForm.StrictMVar
import           Ouroboros.Consensus.Util.NormalForm.StrictTVar
import           Ouroboros.Consensus.Util.Orphans ()


{-------------------------------------------------------------------------------
  IOLike
-------------------------------------------------------------------------------}

class ( MonadAsync              m
      , MonadLabelledSTM        m
      , MonadTraceSTM           m
      , MonadMVar               m
      , MonadEventlog           m
      , MonadFork               m
      , MonadST                 m
      , MonadDelay              m
      , MonadThread             m
      , MonadThrow              m
      , MonadCatch              m
      , MonadMask               m
      , MonadMonotonicTime      m
      , MonadEvaluate           m
      , MonadTraceSTM           m
      , Alternative        (STM m)
      , MonadCatch         (STM m)
      , PrimMonad               m
      , MonadSay                m
      , MonadLabelledSTM        m
      , forall a. NoThunks (m a)
      , forall a. NoThunks a => NoThunks (StrictSTM.StrictTVar m a)
      , forall a. NoThunks a => NoThunks (StrictSVar m a)
      , forall a. NoThunks a => NoThunks (Strict.StrictMVar m a)
      , forall a. NoThunks a => NoThunks (StrictTVar m a)
      , forall a. NoThunks a => NoThunks (StrictMVar m a)
      ) => IOLike m where
  -- | Securely forget a KES signing key.
  --
  -- No-op for the IOSim, but 'KES.forgetSignKeyKES' for IO.
  forgetSignKeyKES :: KESAlgorithm v => SignKeyKES v -> m ()

instance IOLike IO where
  forgetSignKeyKES = KES.forgetSignKeyKES

-- | Generalization of 'link' that links an async to an arbitrary thread.
--
-- Non standard (not in 'async' library)
--
linkTo :: (MonadAsync m, MonadFork m, MonadMask m)
       => ThreadId m -> Async m a -> m ()
linkTo tid = linkToOnly tid (not . isCancel)

-- | Generalization of 'linkOnly' that links an async to an arbitrary thread.
--
-- Non standard (not in 'async' library).
--
linkToOnly :: forall m a. (MonadAsync m, MonadFork m, MonadMask m)
           => ThreadId m -> (SomeException -> Bool) -> Async m a -> m ()
linkToOnly tid shouldThrow a = do
    void $ forkRepeat ("linkToOnly " <> show linkedThreadId) $ do
      r <- waitCatch a
      case r of
        Left e | shouldThrow e -> throwTo tid (exceptionInLinkedThread e)
        _otherwise             -> return ()
  where
    linkedThreadId :: ThreadId m
    linkedThreadId = asyncThreadId a

    exceptionInLinkedThread :: SomeException -> ExceptionInLinkedThread
    exceptionInLinkedThread =
        ExceptionInLinkedThread (show linkedThreadId)

isCancel :: SomeException -> Bool
isCancel e
  | Just AsyncCancelled <- fromException e = True
  | otherwise = False

forkRepeat :: (MonadFork m, MonadMask m) => String -> m a -> m (ThreadId m)
forkRepeat label action =
  mask $ \restore ->
    let go = do r <- tryAll (restore action)
                case r of
                  Left _ -> go
                  _      -> return ()
    in forkIO (labelThisThread label >> go)

tryAll :: MonadCatch m => m a -> m (Either SomeException a)
tryAll = try


{-------------------------------------------------------------------------------
  NoThunks instance
-------------------------------------------------------------------------------}

instance NoThunks a => NoThunks (StrictSTM.StrictTVar IO a) where
  showTypeOf _ = "StrictTVar IO"
  wNoThunks ctxt tv = do
      -- We can't use @atomically $ readTVar ..@ here, as that will lead to a
      -- "Control.Concurrent.STM.atomically was nested" exception.
      a <- StrictSTM.readTVarIO tv
      noThunks ctxt a

instance NoThunks a => NoThunks (Strict.StrictMVar IO a) where
  showTypeOf _ = "StrictMVar IO"
  wNoThunks ctxt mvar = do
      aMay <- inspectMVar (Proxy :: Proxy IO) (Strict.toLazyMVar mvar)
      noThunks ctxt aMay
