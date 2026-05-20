module Ouroboros.Consensus.Cardano.InMemory where

import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Lens.Micro
import Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff

data TablesHandle m era = TablesHandle
  { readTxOuts :: Set SL.TxIn -> m (SL.NewEpochState era)
  -- ^ Given a set of TxIns, produce a NewEpochState that has the
  -- TxOut we could find in the backend
  , duplWithDiffs :: SL.NewEpochState era -> SL.NewEpochState era -> m (TablesHandle m era)
  -- ^ Given the before and after states, produce a new handle on
  -- the after state.
  --
  -- The full states are passed here so that the handle can in the
  -- InMemory case just use the second state, and in the LSM case
  -- it can compute the differences to push them to a duplicated
  -- handle.
  , stateWith :: SL.UTxO era -> SL.NewEpochState era
  -- ^ Only used for the AVVMs, create a NewEpochState as if the
  -- given UTxOs had been read from the disk.
  , duplicateHandle :: m (TablesHandle m era)
  -- ^ Create a duplicated reference to this handle
  , readUTxOWhole :: m (SL.UTxO era)
  -- ^ Read the whole UTxO set from the tables. This method inside will
  -- use pagination if accessing the disk.
  , readUTxOFiltered :: (SL.TxOut era -> Bool) -> m (SL.UTxO era)
  -- ^ Read the UTxO set filtered by a predicate on TxOuts. Will use
  -- pagination if accessing the disk.
  , readTxOutByTxIn :: Set SL.TxIn -> m (SL.UTxO era)
  -- ^ Get a particular (TxIn,TxOut) pair.
  , applyDiff :: Diff.Diff SL.TxIn (SL.TxOut era) -> m ()
  -- ^ Only used for AVVMs. Push a bunch of diffs to this reference
  -- without duplicating it. In the OnDisk backend
  -- this will mutate the database.
  , closeHandle :: m ()
  -- ^ Release the on-disk handle
  , getStatsHandle :: Statistics
  -- ^ Get the size of the tables for this handle
  }

slUtxoL :: Lens' (SL.NewEpochState era) (SL.UTxO era)
slUtxoL = SL.nesEsL . SL.esLStateL . SL.lsUTxOStateL . SL.utxoL

newInMemoryTablesHandle :: Applicative m => SL.NewEpochState era -> TablesHandle m era
newInMemoryTablesHandle ls =
  let h =
        TablesHandle
          { -- whenever we request to read UTxOs, we provide the whole state
            readTxOuts = const $ pure ls
          , -- we createa a new pure handle with the second given state
            duplWithDiffs = const $ pure . newInMemoryTablesHandle
          , -- as the state already has the AVVMs, we ignore them
            stateWith = const ls
          , -- duplicating is just returning the same handle
            duplicateHandle = pure h
          , -- we return the whole UTxO set in one go
            readUTxOWhole = pure (ls ^. slUtxoL)
          , -- We filter the UTxO set with the given predicate
            readUTxOFiltered = \predicate -> pure $ SL.UTxO $ Map.filter predicate (SL.unUTxO (ls ^. slUtxoL))
          , -- We access the requested (TxIn,TxOut)
            readTxOutByTxIn = pure . SL.UTxO . Map.restrictKeys (SL.unUTxO (ls ^. slUtxoL))
          , -- we don't apply AVVM diffs, the ledger already applied them
            applyDiff = const $ pure ()
          , -- closing has no effect
            closeHandle = pure ()
          , -- The statistics is the size of the UTxO map
            getStatsHandle = Statistics $ Map.size $ SL.unUTxO $ ls ^. slUtxoL
          }
   in h

castHandle :: Applicative m => SL.NewEpochState era -> a -> m (TablesHandle m era)
castHandle ls _ = pure $ newInMemoryTablesHandle ls

mkHandle :: Applicative m => SL.NewEpochState era -> m (TablesHandle m era)
mkHandle = pure . newInMemoryTablesHandle
