{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
  , module Control.Concurrent.Class.MonadMVar.NormalForm
  , module Control.Concurrent.Class.MonadSTM.NormalForm
    -- *** MonadFork, TODO: Should we hide this in favour of MonadAsync?
  , MonadFork (..)
  , MonadThread (..)
  , labelThisThread
    -- *** MonadAsync
  , ExceptionInLinkedThread (..)
  , MonadAsync (..)
  , link
    -- *** MonadST
  , MonadST (..)
  , PrimMonad (..)
    -- *** MonadSTM
  , MonadInspectSTM (..)
  , MonadLabelledSTM
  , MonadSTM (..)
  , throwSTM
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
import           Control.Concurrent.Class.MonadMVar
import           Control.Concurrent.Class.MonadMVar.NormalForm
import           Control.Concurrent.Class.MonadSTM.NormalForm
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadEventlog
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Monad.Primitive
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  IOLike
-------------------------------------------------------------------------------}

class ( MonadAsync              m
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
      , Alternative        (STM m)
      , MonadCatch         (STM m)
      , PrimMonad               m
      , forall a. NoThunks (m a)
      , forall a. NoThunks a => NoThunks (StrictTVar m a)
      , forall a. NoThunks a => NoThunks (StrictSVar m a)
      , forall a. NoThunks a => NoThunks (StrictMVar m a)
      ) => IOLike m where
  -- | Securely forget a KES signing key.
  --
  -- No-op for the IOSim, but 'KES.forgetSignKeyKES' for IO.
  forgetSignKeyKES :: KESAlgorithm v => SignKeyKES v -> m ()

instance IOLike IO where
  forgetSignKeyKES = KES.forgetSignKeyKES
