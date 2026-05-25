{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Per-era streaming of UTxO entries between snapshot directories.
--
-- A backend exposes its snapshot-side I/O as a pair of
-- 'SnapshotYielder' / 'SnapshotSinker' values: a yielder traverses
-- the backend's own snapshot representation for a single era and
-- exposes a stream of decoded @('SL.TxIn', 'SL.TxOut' era)@ pairs;
-- a sinker takes such a stream and lays it down in a fresh snapshot
-- in the target backend's representation.
--
-- The records are deliberately era-polymorphic — the era is picked at
-- run time by whichever Cardano-level dispatch invokes them (typically
-- via SOP/'Telescope.tip' against the input ledger state's
-- 'HardForkState'). Snapshot conversion never crosses eras: input and
-- output share the same era, and so the same era variable threads
-- through both ends.
module Ouroboros.Consensus.Shelley.Ledger.SnapshotStream
  ( SnapshotYielder (..)
  , SnapshotSinker (..)
  , EntryStream
  ) where

import Cardano.Ledger.Binary.Decoding (DecShareCBOR, Interns, Share)
import Cardano.Ledger.Core (Era)
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Codec.CBOR.Read (DeserialiseFailure)
import Control.Monad.Except (ExceptT)
import Data.ByteString (ByteString)
import Data.MemPack (MemPack)
import Streaming (Of, Stream)
import System.FS.CRC (CRC)

-- | A per-era stream of UTxO entries sitting on top of a byte-level
-- stream that carries the on-disk CRC.
--
-- The outer 'Stream' yields decoded @('SL.TxIn', 'SL.TxOut' era)@
-- pairs in 'ExceptT' 'DeserialiseFailure'; the inner 'Stream' is the
-- byte residue of the underlying source (or 'Nothing'-CRC when the
-- backend has no single-file representation, e.g. LSM) so the caller
-- can keep updating a running CRC across yielder and sinker.
type EntryStream m era r =
  Stream
    (Of (SL.TxIn, SL.TxOut era))
    (ExceptT DeserialiseFailure m)
    (Stream (Of ByteString) m r)

-- | A backend's ability to yield UTxO entries out of a snapshot it
-- owns.
--
-- The yielder closes over the input 'DiskSnapshot' and whatever
-- backend-specific resources it needs (a file system, an LSM session,
-- ...). When run, it opens its own snapshot representation, decodes
-- entries for the supplied era's 'SL.NewEpochState' (used to recover
-- per-era sharing context, e.g. certificate interns), and threads the
-- decoded stream through the supplied continuation.
--
-- The final @('Maybe' 'CRC', 'Maybe' 'CRC')@ pair is @(inputCRC,
-- outputCRC)@: a backend with no single-file representation may
-- report 'Nothing' on either side.
--
-- 'releaseYielder' tears down any resources allocated when the
-- yielder was constructed.
data SnapshotYielder m = SnapshotYielder
  { runYielder ::
      forall era.
      ( Era era
      , MemPack (SL.TxOut era)
      , DecShareCBOR (SL.TxOut era)
      , Share (SL.TxOut era) ~ Interns (SL.Credential SL.Staking)
      , SL.EraCertState era
      ) =>
      SL.NewEpochState era ->
      ( EntryStream m era (Maybe CRC) ->
        ExceptT DeserialiseFailure m (Stream (Of ByteString) m (Maybe CRC, Maybe CRC))
      ) ->
      ExceptT DeserialiseFailure m (Maybe CRC, Maybe CRC)
  , releaseYielder :: m ()
  }

-- | A backend's ability to sink a stream of UTxO entries into a fresh
-- snapshot of its own.
--
-- The sinker closes over the output 'DiskSnapshot' and any
-- backend-specific resources. When run, it consumes the supplied
-- 'EntryStream' and writes the corresponding snapshot artifact;
-- 'releaseSinker' releases the closed-over resources.
data SnapshotSinker m = SnapshotSinker
  { runSinker ::
      forall era.
      ( Era era
      , MemPack (SL.TxOut era)
      ) =>
      EntryStream m era (Maybe CRC) ->
      ExceptT DeserialiseFailure m (Stream (Of ByteString) m (Maybe CRC, Maybe CRC))
  , releaseSinker :: m ()
  }
