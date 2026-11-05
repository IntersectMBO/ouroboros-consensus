{-# LANGUAGE OverloadedStrings #-}

-- | Offline maintenance of a LeiosDB partition file, for internal tooling.
--
-- These open the file directly, not through a
-- 'Ouroboros.Consensus.Storage.LeiosDB.API.LeiosDbHandle', so they
-- must not run against a file that a node has open.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Tooling
  ( truncateLeiosDbAfterSlot
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

-- | Delete the endorser block points after the given slot, with their bodies
-- and txs.
--
-- For internal tooling. Works for both the volatile and the immutable
-- partition files.
truncateLeiosDbAfterSlot :: HasCallStack => FilePath -> SlotNo -> IO ()
truncateLeiosDbAfterSlot dbPath (SlotNo slot) =
  withExistingLeiosDbFile dbPath $ \db ->
    -- One transaction, so a crash cannot leave a point without the body
    -- written for it.
    dbWithTransactionAs "BEGIN IMMEDIATE" db $
      dbExec db (fromString deletes)
 where
  deletes =
    unlines
      [ "DELETE FROM ebTxBytes WHERE ebSlot > " <> show slot <> ";"
      , "DELETE FROM ebTxs WHERE ebSlot > " <> show slot <> ";"
      , "DELETE FROM ebs WHERE ebSlot > " <> show slot <> ";"
      ]

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
