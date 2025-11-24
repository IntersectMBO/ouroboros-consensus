{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tools.ImmDBServer.Json (module Cardano.Tools.ImmDBServer.Json) where

import           Control.Concurrent.Class.MonadMVar (newMVar, modifyMVar_, withMVar)
import           Control.Tracer
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Strict as Map
import           Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import           Cardano.Tools.ImmDBServer.Json.Say as Say
import           Cardano.Tools.ImmDBServer.Json.SendRecv as SendRecv

-----

-- | A more perspicuous isomorph of @()@.
data ToBeDetermined = TBD
  deriving (Eq, Ord)

-- | The events that the application issues.
data LogEvent =
    SayEvent (SayEvent ToBeDetermined)
  | SendRecvEvent (SendRecvEvent ToBeDetermined ToBeDetermined)

newtype CounterName = MkCounterName (SendRecvEvent ToBeDetermined ToBeDetermined)
  deriving (Eq, Ord)

mkUltimateTracer :: IO (Tracer IO LogEvent)
mkUltimateTracer = do
    -- This lock serves two roles. First, it prevents the @bytestring@
    -- library's @putStrLn@ calls from clobbering each other. Second, it
    -- ensures atomic updates of the @SendRecv@ counters.
    lock <- newMVar Map.empty
    -- We read the time /after/ acquiring the lock to ensure the invariant that
    -- the timestamps in the log file are non-descending.
    pure $ Tracer $ \case
        SayEvent ev -> withMVar lock $ \_cntrs -> do
            tm <- getCurrentTime
            BL8.putStrLn $ Aeson.encode ev { Say.at = renderTime tm }
        SendRecvEvent ev -> modifyMVar_ lock $ \cntrs -> do
            tm <- getCurrentTime
            let MkGet n cntrs' = Map.alterF updateCounter ev cntrs
            BL8.putStrLn $ Aeson.encode ev { SendRecv.at = renderTime tm, prevCount = n }
            pure cntrs'
  where
    renderTime tm = formatTime defaultTimeLocale "%FT%H:%M:%S%4QZ" tm

    updateCounter = \case
        Nothing -> MkGet 0 $ Just 1
        Just n -> MkGet n $ Just (n + 1)

-----

-- | Auxilary type for reading the map value at the same time we update it.
data Get a = MkGet !Int !a
  deriving (Functor)
