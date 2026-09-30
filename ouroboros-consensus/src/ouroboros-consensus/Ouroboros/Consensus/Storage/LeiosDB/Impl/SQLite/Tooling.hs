{-# LANGUAGE OverloadedStrings #-}

-- | Offline maintenance of a LeiosDB partition file, for internal tooling.
--
-- These open the file directly, not through a
-- 'Ouroboros.Consensus.Storage.LeiosDB.API.LeiosDbHandle', so they
-- must not run against a file that a node has open.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Tooling
  ( truncateLeiosDbAfterSlot
  , deleteDanglingTxs
  , vacuumLeiosDb
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (void)
import Control.Monad.Class.MonadThrow (bracket)
import Data.String (fromString)
import Database.SQLite3
  ( SQLOpenFlag (..)
  , SQLVFS (..)
  , open2
  )
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives

-- | Delete the EBs announced after the given slot.
--
-- For internal tooling.
--
-- Note: this function works for both the volatile and the immutable partition
--       files. The immutable one has no 'ebsMissingTxs' table (see
--       'Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema.sql_schema_imm'),
--       so the rows there are deleted only if the table exists.
truncateLeiosDbAfterSlot :: HasCallStack => FilePath -> SlotNo -> IO ()
truncateLeiosDbAfterSlot dbPath (SlotNo slot) =
  withExistingLeiosDbFile dbPath $ \db ->
    -- One transaction, so a crash cannot leave an EB that is still announced
    -- but has no body.
    dbWithTransactionAs "BEGIN IMMEDIATE" db $ do
      hasMissingTxs <-
        (/= 0)
          <$> queryInt64
            db
            "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'ebsMissingTxs'"
      dbExec db (fromString (deletes hasMissingTxs))
 where
  deletes hasMissingTxs =
    unlines $
      ["DELETE FROM ebTxs WHERE ebHashBytes IN (" <> droppedHashes <> ");"]
        <> [ "DELETE FROM ebsMissingTxs WHERE ebHashBytes IN (" <> droppedHashes <> ");"
           | hasMissingTxs
           ]
        <> ["DELETE FROM ebs WHERE ebSlot > " <> show slot <> ";"]

  -- The EBs whose bodies the truncation drops.
  --
  -- 'ebs' holds one row per announcement, so the same EB hash can appear at
  -- several slots. 'ebTxs' and 'ebsMissingTxs' hold one copy per hash and carry
  -- no slot. So an EB announced at slot 5 and again at slot 15 keeps its body
  -- when the cut is at slot 10. That is what the EXCEPT does: take the hashes
  -- announced after the cut, then remove the ones also announced at or before
  -- it.
  droppedHashes =
    "SELECT ebHashBytes FROM ebs WHERE ebSlot > "
      <> show slot
      <> " EXCEPT SELECT ebHashBytes FROM ebs WHERE ebSlot <= "
      <> show slot

-- | Delete the transactions that no EB references.
--
-- Used in tests.
deleteDanglingTxs :: HasCallStack => FilePath -> IO ()
deleteDanglingTxs dbPath =
  withExistingLeiosDbFile dbPath $ \db ->
    dbExec db . fromString $
      "DELETE FROM txs WHERE txHashBytes NOT IN (SELECT txHashBytes FROM ebTxs)"

-- | Shrink a LeiosDb file to the space its rows need.
--
-- A delete frees pages inside the file without returning them to the
-- filesystem. This rewrites the file, so it needs free space of about the size
-- of the file. For internal tooling.
vacuumLeiosDb :: HasCallStack => FilePath -> IO ()
vacuumLeiosDb dbPath =
  withExistingLeiosDbFile dbPath $ \db ->
    dbExec db (fromString "VACUUM")

-- | Open the LeiosDb file at the given path, which must already exist.
--
-- Unrelated to 'withReader', which brackets a 'LeiosDbReader' that a
-- 'LeiosDbHandle' opens.
--
-- No 'SQLOpenCreate', unlike 'initialiseLeiosDbFiles': a wrong path must fail
-- rather than gain an empty database. No 'busy_timeout' either, so a write that
-- meets the node's own write lock gives up after the retries in 'withDie'
-- rather than block.
withExistingLeiosDbFile :: FilePath -> (DB.Database -> IO a) -> IO a
withExistingLeiosDbFile dbPath =
  bracket
    (open2 (fromString dbPath) [SQLOpenReadWrite] SQLVFSDefault)
    (void . DB.close)
