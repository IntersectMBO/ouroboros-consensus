{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Backends.InMemory
  ( mkInMemoryFactory
  , mkInMemoryFromSnapshot
  ) where

import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import qualified Cardano.Ledger.Conway.State as SL
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Monad.Except
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Data.MemPack
import Lens.Micro
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend (LedgerDBV2Trace (..))
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.CBOR
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import System.FS.CRC

newInMemoryTablesHandle ::
  forall m era.
  (SL.Era era, MemPack (SL.TxOut era), IOLike m) =>
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m -> SL.NewEpochState era -> TablesHandle m era
newInMemoryTablesHandle tracer shfs@(SomeHasFS hasFS) ls =
  let h =
        TablesHandle
          { -- whenever we request to read UTxOs, we provide the whole state
            stateWith = const $ pure ls
          , -- we createa a new pure handle with the second given state
            duplWithDiffs = \_ st1 ->
              encloseTimedWith (TraceLedgerTablesHandlePush >$< tracer) $
                pure $
                  newInMemoryTablesHandle tracer shfs st1
          , -- as the state already has the AVVMs, we ignore them
            stateWithUTxO = const ls
          , -- duplicating is just returning the same handle; still trace it
            -- so the per-handle lifecycle remains observable.
            duplicateHandle =
              encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $ pure h
          , -- we return the whole UTxO set in one go
            readUTxOWhole =
              encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
                pure (ls ^. slUtxoL)
          , -- We filter the UTxO set with the given predicate
            readUTxOFiltered = \predicate ->
              encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
                pure $
                  SL.UTxO $
                    Map.filter predicate (SL.unUTxO (ls ^. slUtxoL))
          , -- We access the requested (TxIn,TxOut)
            readTxOuts = \keys ->
              encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
                pure $
                  SL.UTxO $
                    Map.restrictKeys (SL.unUTxO (ls ^. slUtxoL)) keys
          , -- we don't apply AVVM diffs, the ledger already applied them
            applyDiff = \_ ->
              encloseTimedWith (TraceLedgerTablesHandlePush >$< tracer) $ pure h
          , -- closing has no effect, but trace it for lifecycle visibility
            closeHandle =
              encloseTimedWith (TraceLedgerTablesHandleClose >$< tracer) (pure ())
          , -- The statistics is the size of the UTxO map
            getStatsHandle = Statistics $ Map.size $ SL.unUTxO $ ls ^. slUtxoL
          , takeHandleSnapshot = \ds -> do
              createDirectoryIfMissing hasFS True $ snapshotToDirPath ds
              withFile hasFS (snapshotToUTxOFilePath ds) (WriteMode MustBeNew) $ \hf ->
                fmap (\(_, x) -> (Just x, UTxOHDMemSnapshot)) $
                  hPutAllCRC hasFS hf $
                    CBOR.toLazyByteString $
                      mconcat
                        [ CBOR.encodeListLen 1
                        , toPlainEncoding (SL.eraProtVerLow @era) $
                            encodeMap encodeMemPack encodeMemPack (SL.unUTxO (ls ^. slUtxoL))
                        ]
          , castHandle =
              \st ->
                encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $
                  pure $
                    newInMemoryTablesHandle tracer shfs st
          , injectValues =
              \st ->
                encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $
                  pure $
                    newInMemoryTablesHandle tracer shfs st
          }
   in h

mkInMemoryFactory ::
  forall m.
  IOLike m =>
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  MkHandle m
mkInMemoryFactory tracer shfs =
  MkHandle
    { fromNewEpochState = \st ->
        encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $
          pure (newInMemoryTablesHandle tracer shfs st)
    }

mkInMemoryFromSnapshot ::
  forall m.
  IOLike m =>
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  MkHandleFromSnapshot m
mkInMemoryFromSnapshot tracer shfs =
  MkHandleFromSnapshot
    { fromSnapshot = withExceptT BackendReadErr .: implFromSnapshot
    }
 where
  implFromSnapshot ::
    forall era.
    ( SL.Era era
    , MemPack (SL.TxOut era)
    , IOLike m
    , Share (SL.TxOut era) ~ Interns (SL.Credential SL.Staking)
    , DecShareCBOR (SL.TxOut era)
    , SL.EraCertState era
    ) =>
    DiskSnapshot -> SL.NewEpochState era -> ExceptT ReadIncrementalErr m (TablesHandle m era, Maybe CRC)
  implFromSnapshot ds ls = do
    let certInterns =
          internsFromMap $
            ls
              ^. SL.nesEsL
                . SL.esLStateL
                . SL.lsCertStateL
                . SL.certDStateL
                . SL.accountsL
                . SL.accountsMapL
    (utxo, Identity crcTables) <-
      ExceptT $
        readIncremental
          shfs
          Identity
          ( do
              l <- CBOR.decodeListLen
              case l of
                1 -> SL.eraDecoder @era (decodeMap decodeMemPack (decShareCBOR certInterns))
                _ -> fail $ "Wrong number of tables: " <> show l
          )
          (snapshotToUTxOFilePath ds)

    h <-
      lift $
        encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $
          pure $
            newInMemoryTablesHandle tracer shfs (ls & slUtxoL .~ SL.UTxO utxo)
    pure (h, Just crcTables)

snapshotToUTxOFilePath :: DiskSnapshot -> FsPath
snapshotToUTxOFilePath ds = snapshotToDirPath ds </> mkFsPath ["utxo"]
