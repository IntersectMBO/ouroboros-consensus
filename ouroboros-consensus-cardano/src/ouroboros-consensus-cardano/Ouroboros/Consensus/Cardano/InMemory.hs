{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Cardano.InMemory where

import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import qualified Cardano.Ledger.Conway.State as SL
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer)
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Sing (SListI)
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Data.Set (Set)
import Lens.Micro
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.Ledger
import Ouroboros.Consensus.HardFork.Combinator.State.Infra
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.CBOR
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import System.FS.CRC

data TablesHandle m era = TablesHandle
  { stateWith :: Set SL.TxIn -> m (SL.NewEpochState era)
  -- ^ Given a set of TxIns, produce a NewEpochState that has the
  -- TxOuts we could find in the backend
  , stateWithUTxO :: SL.UTxO era -> SL.NewEpochState era
  -- ^ Only used for the AVVMs, create a NewEpochState as if the
  -- given UTxOs had been read from the disk.
  , applyDiff :: Diff.Diff SL.TxIn (SL.TxOut era) -> m (TablesHandle m era)
  -- ^ Only used for AVVMs. Push a bunch of diffs to this reference
  -- without duplicating it. In the OnDisk backend
  -- this will mutate the database.
  , duplWithDiffs :: SL.NewEpochState era -> SL.NewEpochState era -> m (TablesHandle m era)
  -- ^ Given the before and after states, produce a new handle on
  -- the after state.
  --
  -- The full states are passed here so that the handle can in the
  -- InMemory case just use the second state, and in the LSM case
  -- it can compute the differences to push them to a duplicated
  -- handle.
  , duplicateHandle :: m (TablesHandle m era)
  -- ^ Create a duplicated reference to this handle
  , readUTxOWhole :: m (SL.UTxO era)
  -- ^ Read the whole UTxO set from the tables. This method inside will
  -- use pagination if accessing the disk.
  , readUTxOFiltered :: (SL.TxOut era -> Bool) -> m (SL.UTxO era)
  -- ^ Read the UTxO set filtered by a predicate on TxOuts. Will use
  -- pagination if accessing the disk.
  , readTxOuts :: Set SL.TxIn -> m (SL.UTxO era)
  -- ^ Get a particular (TxIn,TxOut) pair.
  , closeHandle :: m ()
  -- ^ Release the on-disk handle
  , getStatsHandle :: Statistics
  -- ^ Get the size of the tables for this handle
  , takeHandleSnapshot :: DiskSnapshot -> m (Maybe CRC, SnapshotBackend)
  -- ^ Take a snapshot with the given name
  , castHandle ::
      forall era'.
      (SL.Era era', MemPack (SL.TxOut era'), Eq (SL.TxOut era')) =>
      SL.NewEpochState era' -> m (TablesHandle m era')
  }

slUtxoL :: Lens' (SL.NewEpochState era) (SL.UTxO era)
slUtxoL = SL.nesEsL . SL.esLStateL . SL.lsUTxOStateL . SL.utxoL

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
          }
   in h

data MkHandle m = MkHandle
  { fromNewEpochState ::
      forall era.
      (SL.Era era, MemPack (SL.TxOut era), MonadThrow m, Eq (SL.TxOut era)) =>
      SL.NewEpochState era -> m (TablesHandle m era)
  }

data MkHandleFromSnapshot m = MkHandleFromSnapshot
  { fromSnapshot ::
      forall era.
      ( SL.Era era
      , MemPack (SL.TxOut era)
      , IOLike m
      , Share (SL.TxOut era) ~ Interns (SL.Credential SL.Staking)
      , DecShareCBOR (SL.TxOut era)
      , SL.EraCertState era
      , Eq (SL.TxOut era)
      ) =>
      DiskSnapshot -> SL.NewEpochState era -> ExceptT BackendError m (TablesHandle m era, Maybe CRC)
  }

data BackendError = BackendReadErr ReadIncrementalErr | BackendCorruptedData
  deriving Show

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

{-------------------------------------------------------------------------------
  InMemory backend constructor
-------------------------------------------------------------------------------}
