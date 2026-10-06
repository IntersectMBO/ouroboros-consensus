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
-- For internal tooling. Works for both the volatile and the immutable
-- partition files.
truncateLeiosDbAfterSlot :: HasCallStack => FilePath -> SlotNo -> IO ()
truncateLeiosDbAfterSlot dbPath (SlotNo slot) =
  withExistingLeiosDbFile dbPath $ \db ->
    -- One transaction, so a crash cannot leave an EB that is still announced
    -- but has no body.
    dbWithTransactionAs "BEGIN IMMEDIATE" db $
      dbExec db (fromString deletes)
 where
  deletes =
    unlines
      [ "DELETE FROM ebTxBytes WHERE ebHashBytes IN (" <> droppedHashes <> ");"
      , "DELETE FROM ebTxs WHERE ebHashBytes IN (" <> droppedHashes <> ");"
      , "DELETE FROM ebs WHERE ebSlot > " <> show slot <> ";"
      ]

  -- The EBs whose bodies the truncation drops.
  --
  -- 'ebs' holds one row per announcement, so the same EB hash can appear at
  -- several slots. 'ebTxs' and 'ebTxBytes' hold one copy per hash and carry
  -- no slot. So an EB announced at slot 5 and again at slot 15 keeps its body
  -- when the cut is at slot 10. That is what the EXCEPT does: take the hashes
  -- announced after the cut, then remove the ones also announced at or before
  -- it.
  droppedHashes =
    "SELECT ebHashBytes FROM ebs WHERE ebSlot > "
      <> show slot
      <> " EXCEPT SELECT ebHashBytes FROM ebs WHERE ebSlot <= "
      <> show slot

-- | Delete the tx bytes of EBs that are no longer announced.
deleteDanglingTxs :: HasCallStack => FilePath -> IO ()
deleteDanglingTxs dbPath =
  withExistingLeiosDbFile dbPath $ \db ->
    dbExec db . fromString $
      "DELETE FROM ebTxBytes WHERE ebHashBytes NOT IN (SELECT ebHashBytes FROM ebs)"

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
