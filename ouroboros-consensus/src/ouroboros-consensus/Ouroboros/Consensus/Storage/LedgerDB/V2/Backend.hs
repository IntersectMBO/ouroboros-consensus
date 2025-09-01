{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
  ( LedgerDBBackend (..)
  , SomeBackendTrace (..)
  , SomeBackendArgs (..)
  , SomeResources (..)
  , FlavorImplSpecificTrace (..)
  , Yield
  , Sink
  , Decoders (..)
  ) where

import Codec.CBOR.Decoding
import Codec.CBOR.Read
import Control.Monad.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.ByteString (ByteString)
import Data.Proxy
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Streaming
import System.FS.API
import System.FS.CRC

data SomeBackendTrace where
  SomeBackendTrace :: Trace m backend -> SomeBackendTrace

data SomeBackendArgs m blk where
  SomeBackendArgs :: LedgerDBBackend m backend blk => Args m backend -> SomeBackendArgs m blk

data SomeResources m blk where
  SomeResources :: LedgerDBBackend m backend blk => Resources m backend -> SomeResources m blk

instance NoThunks (SomeResources m blk) where
  wNoThunks ctxt (SomeResources res) = wNoThunks ctxt res
  noThunks ctxt (SomeResources res) = noThunks ctxt res
  showTypeOf _ = "SomeResources"

class NoThunks (Resources m backend) => LedgerDBBackend m backend blk where
  data Args m backend

  data Resources m backend

  data Trace m backend

  mkResources ::
    Proxy blk ->
    Tracer m FlavorImplSpecificTrace ->
    Args m backend ->
    ResourceRegistry m ->
    SomeHasFS m ->
    m (Resources m backend)

  releaseResources :: Proxy blk -> Resources m backend -> m ()

  newHandleFromValues ::
    Tracer m FlavorImplSpecificTrace ->
    ResourceRegistry m ->
    Resources m backend ->
    ExtLedgerState blk ValuesMK ->
    m (LedgerTablesHandle m (ExtLedgerState blk))

  newHandleFromSnapshot ::
    Tracer m FlavorImplSpecificTrace ->
    ResourceRegistry m ->
    CodecConfig blk ->
    SomeHasFS m ->
    Resources m backend ->
    DiskSnapshot ->
    ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)

  snapshotManager ::
    Proxy blk ->
    Resources m backend ->
    CodecConfig blk ->
    Tracer m (TraceSnapshotEvent blk) ->
    SomeHasFS m ->
    SnapshotManager m m blk (StateRef m (ExtLedgerState blk))

  data YieldArgs m backend blk

  data SinkArgs m backend blk

  yield :: Proxy backend -> YieldArgs m backend blk -> Yield m blk

  sink :: Proxy backend -> SinkArgs m backend blk -> Sink m blk

type Yield m blk =
  ExtLedgerState blk EmptyMK ->
  ( ( Stream
        (Of (TxIn (ExtLedgerState blk), TxOut (ExtLedgerState blk)))
        (ExceptT DeserialiseFailure m)
        (Stream (Of ByteString) m (Maybe CRC)) ->
      ExceptT DeserialiseFailure m (Stream (Of ByteString) m (Maybe CRC, Maybe CRC))
    )
  ) ->
  ExceptT DeserialiseFailure m (Maybe CRC, Maybe CRC)

type Sink m blk =
  ExtLedgerState blk EmptyMK ->
  Stream
    (Of (TxIn (ExtLedgerState blk), TxOut (ExtLedgerState blk)))
    (ExceptT DeserialiseFailure m)
    (Stream (Of ByteString) m (Maybe CRC)) ->
  ExceptT DeserialiseFailure m (Stream (Of ByteString) m (Maybe CRC, Maybe CRC))

data Decoders l
  = Decoders
      (forall s. Decoder s (TxIn l))
      (forall s. Decoder s (TxOut l))

data FlavorImplSpecificTrace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose
  | BackendTrace SomeBackendTrace

deriving instance Show SomeBackendTrace => Show FlavorImplSpecificTrace
