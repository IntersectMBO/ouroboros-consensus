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
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.CBOR
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import System.FS.CRC

newInMemoryTablesHandle ::
  forall m era.
  (SL.Era era, MemPack (SL.TxOut era), MonadThrow m) =>
  SomeHasFS m -> SL.NewEpochState era -> TablesHandle m era
newInMemoryTablesHandle shfs@(SomeHasFS hasFS) ls =
  let h =
        TablesHandle
          { -- whenever we request to read UTxOs, we provide the whole state
            stateWith = const $ pure ls
          , -- we createa a new pure handle with the second given state
            duplWithDiffs = const $ pure . newInMemoryTablesHandle shfs
          , -- as the state already has the AVVMs, we ignore them
            stateWithUTxO = const ls
          , -- duplicating is just returning the same handle
            duplicateHandle = pure h
          , -- we return the whole UTxO set in one go
            readUTxOWhole = pure (ls ^. slUtxoL)
          , -- We filter the UTxO set with the given predicate
            readUTxOFiltered = \predicate -> pure $ SL.UTxO $ Map.filter predicate (SL.unUTxO (ls ^. slUtxoL))
          , -- We access the requested (TxIn,TxOut)
            readTxOuts = pure . SL.UTxO . Map.restrictKeys (SL.unUTxO (ls ^. slUtxoL))
          , -- we don't apply AVVM diffs, the ledger already applied them
            applyDiff = const $ pure h
          , -- closing has no effect
            closeHandle = pure ()
          , -- The statistics is the size of the UTxO map
            getStatsHandle = Statistics $ Map.size $ SL.unUTxO $ ls ^. slUtxoL
          , takeHandleSnapshot = \(snapshotToDirName -> snapshotName) -> do
              createDirectoryIfMissing hasFS True $ mkFsPath [snapshotName]
              withFile hasFS (mkFsPath [snapshotName, "utxo"]) (WriteMode MustBeNew) $ \hf ->
                fmap (\(_, x) -> (Just x, UTxOHDMemSnapshot)) $
                  hPutAllCRC hasFS hf $
                    CBOR.toLazyByteString $
                      mconcat
                        [ CBOR.encodeListLen 1
                        , toPlainEncoding (SL.eraProtVerLow @era) $
                            encodeMap encodeMemPack encodeMemPack (SL.unUTxO (ls ^. slUtxoL))
                        ]
          , castHandle = pure . newInMemoryTablesHandle shfs
          , injectValues = pure . newInMemoryTablesHandle shfs
          }
   in h

mkInMemoryFactory ::
  forall m.
  MonadThrow m =>
  SomeHasFS m ->
  MkHandle m
mkInMemoryFactory shfs =
  MkHandle
    { fromNewEpochState = pure . newInMemoryTablesHandle shfs
    }

mkInMemoryFromSnapshot ::
  forall m.
  MonadThrow m =>
  SomeHasFS m ->
  MkHandleFromSnapshot m
mkInMemoryFromSnapshot shfs =
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
          (snapshotToTablesPath ds)

    pure (newInMemoryTablesHandle shfs (ls & slUtxoL .~ SL.UTxO utxo), Just crcTables)

snapshotToTablesPath :: DiskSnapshot -> FsPath
snapshotToTablesPath ds = snapshotToDirPath ds </> mkFsPath ["tables"]
