{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 910
{-# OPTIONS_GHC -Wno-x-shelley-empty-utxo #-}
#else
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
#endif
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Per-era streaming of the Cardano UTxO tables for the snapshot converter.
--
-- A snapshot's UTxO tables live entirely in the era at the snapshot's tip (the
-- on-disk @'TxOut'@ bytes are written by the node in that era's @'MemPack'@
-- codec). So the converter streams at the concrete single era
-- @'ShelleyBlock' proto era@ rather than at the multi-era @'CardanoBlock'@:
-- 'withCardanoCurrentEra' resolves the tip era from the loaded state, and the
-- per-backend @YieldArgs@\/@SinkArgs@ are built at that era. The per-entry codec
-- mirrors the era's 'encodeValues'\/'decodeValues' exactly (so converted
-- snapshots stay readable by a node).
module Ouroboros.Consensus.Cardano.StreamingLedgerTables
  ( StreamableEra
  , withCardanoCurrentEra
  , projectCardanoValues
  , mkInMemYieldArgs
  , mkInMemSinkArgs
  ) where

import Cardano.Ledger.Binary.Decoding
  ( decShareCBOR
  , decodeMemPack
  , internsFromMap
  )
import Cardano.Ledger.Binary.Encoding
  ( encodeMemPack
  , toPlainEncoding
  )
import Cardano.Ledger.Core (eraDecoder)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.State as SL
import qualified Codec.CBOR.Encoding
import Data.SOP.BasicFunctors
import Data.SOP.Index
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork (CardanoHardForkConstraints)
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.State
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as V2
import Ouroboros.Consensus.TypeFamilyWrappers
import System.FS.API

-- | The dictionaries a single Cardano (Shelley-based) era needs to stream its
-- UTxO tables. @'SingleEraBlockSupportsLedgerHD'@ (with its @'MemPack'@
-- superclasses) and the rest follow from @'ShelleyCompatible'@ via the era
-- instances.
type StreamableEra proto era = ShelleyCompatible proto era

-- | Resolve the era at the tip of a Cardano ledger state and hand its concrete
-- single-era @'ShelleyBlock'@ — together with the era's index, its (UTxO-free)
-- ledger state, and the projection of the hard-fork values onto that era — to
-- the continuation. The first argument is the result for a Byron tip, which
-- carries no UTxO-HD tables.
withCardanoCurrentEra ::
  forall r.
  CardanoHardForkConstraints StandardCrypto =>
  LedgerState (CardanoBlock StandardCrypto) ->
  -- | Result when the tip era is Byron (no UTxO tables to stream).
  r ->
  ( forall proto era.
    StreamableEra proto era =>
    Index (CardanoEras StandardCrypto) (ShelleyBlock proto era) ->
    LedgerState (ShelleyBlock proto era) ->
    (Values (CardanoBlock StandardCrypto) -> Values (ShelleyBlock proto era)) ->
    r
  ) ->
  r
withCardanoCurrentEra (HardForkLedgerState (HardForkState idx)) onByron k =
  hcollapse $ hap np $ Telescope.tip idx
 where
  np :: NP (Current LedgerState -.-> K r) (CardanoEras StandardCrypto)
  np =
    (Fn $ \_ -> K onByron)
      :* shelleyArm (IS IZ)
      :* shelleyArm (IS (IS IZ))
      :* shelleyArm (IS (IS (IS IZ)))
      :* shelleyArm (IS (IS (IS (IS IZ))))
      :* shelleyArm (IS (IS (IS (IS (IS IZ)))))
      :* shelleyArm (IS (IS (IS (IS (IS (IS IZ))))))
      :* shelleyArm (IS (IS (IS (IS (IS (IS (IS IZ)))))))
      :* Nil

  shelleyArm ::
    StreamableEra proto era =>
    Index (CardanoEras StandardCrypto) (ShelleyBlock proto era) ->
    (Current LedgerState -.-> K r) (ShelleyBlock proto era)
  shelleyArm idx' =
    Fn $ \cur -> K (k idx' (currentState cur) (projectCardanoValues idx'))

-- | Project the current era's values out of the hard-fork @'NS'@. The values
-- read out of a snapshot's tables are tagged with the era at the tip, so the
-- requested arm is guaranteed present.
projectCardanoValues ::
  Index (CardanoEras StandardCrypto) blk ->
  Values (CardanoBlock StandardCrypto) ->
  Values blk
projectCardanoValues idx0 =
  maybe (error "projectCardanoValues: values in unexpected era") unwrapValues
    . go idx0
 where
  go :: Index xs x -> NS WrapValues xs -> Maybe (WrapValues x)
  go IZ (Z x) = Just x
  go (IS idx) (S ns) = go idx ns
  go _ _ = Nothing

-- | In-memory @YieldArgs@ at a concrete era. The per-entry decoders mirror the
-- era's @'decodeValues'@: the key is cast through @'BigEndianTxIn'@ and the
-- @'TxOut'@ is decoded with credential sharing seeded from the era state.
mkInMemYieldArgs ::
  forall proto era.
  ShelleyCompatible proto era =>
  TablesCodecVersion ->
  SomeHasFS IO ->
  DiskSnapshot ->
  LedgerState (ShelleyBlock proto era) ->
  YieldArgs IO V2.Mem LedgerState (ShelleyBlock proto era)
mkInMemYieldArgs version fs ds st =
  YieldInMemory
    fs
    ds
    version
    ( Decoders
        (eraDecoder @era (getOriginalTxIn <$> decodeMemPack))
        (eraDecoder @era (decShareCBOR certInterns))
    )
 where
  certInterns =
    internsFromMap $
      shelleyLedgerState st
        ^. SL.nesEsL
          . SL.esLStateL
          . SL.lsCertStateL
          . SL.certDStateL
          . SL.accountsL
          . SL.accountsMapL

-- | In-memory @SinkArgs@ at a concrete era. The per-entry encoders mirror the
-- era's @'encodeValues'@: the key is cast through @'BigEndianTxIn'@ so the
-- serialised entries sort the same as @'Ord' ('TxIn' blk)@.
mkInMemSinkArgs ::
  forall proto era.
  ShelleyCompatible proto era =>
  SomeHasFS IO ->
  DiskSnapshot ->
  LedgerState (ShelleyBlock proto era) ->
  SinkArgs IO V2.Mem LedgerState (ShelleyBlock proto era)
mkInMemSinkArgs fs ds _st =
  SinkInMemory
    1000
    encTxIn
    encTxOut
    fs
    ds
 where
  encTxIn :: TxIn (ShelleyBlock proto era) -> Codec.CBOR.Encoding.Encoding
  encTxIn = toPlainEncoding (Core.eraProtVerLow @era) . encodeMemPack . BigEndianTxIn

  encTxOut :: TxOut (ShelleyBlock proto era) -> Codec.CBOR.Encoding.Encoding
  encTxOut = toPlainEncoding (Core.eraProtVerLow @era) . encodeMemPack
