module Cardano.Tools.LeiosDb (LeiosDbSource (..), leiosDbPath) where

import Control.Monad (unless)
import qualified System.Directory as Directory
import System.Exit (die)
import qualified System.FilePath as FilePath

-- | Which LeiosDb the tool works on, per @--no-leios-db@.
data LeiosDbSource
  = -- | The @leios.db@ under the @--db@ directory.
    NodeLeiosDb
  | -- | No file. db-analyser uses an empty in-memory LeiosDb, and db-truncater
    -- skips every LeiosDb step.
    NoLeiosDb

-- | The path of the @leios.db@ that the node writes under its ChainDB
-- directory, or 'Nothing' for 'NoLeiosDb'.
--
-- The tool derives that path from @--db@ rather than take one of its own. An
-- operator who puts the file elsewhere can symlink it into place.
--
-- A missing file is fatal, because a tool cannot tell whether the chain holds a
-- cert-RB before it reads the chain. The caller passes 'NoLeiosDb' to say that
-- the chain holds no cert-RB.
--
-- Hence this check, rather than a check inside the SQLite backend: that backend
-- opens with 'SQLOpenCreate' and it creates the schema when it finds no file.
-- So without this check the tool would write an empty leios.db into the node's
-- directory and fail only at the first cert-RB.
leiosDbPath ::
  LeiosDbSource ->
  -- | The ChainDB directory.
  FilePath ->
  IO (Maybe FilePath)
leiosDbPath NoLeiosDb _dbDir = pure Nothing
leiosDbPath NodeLeiosDb dbDir = do
  let path = dbDir FilePath.</> "leios.db"
  exists <- Directory.doesFileExist path
  unless exists $
    die $
      "No LeiosDb at "
        <> path
        <> ". A block that carries a Leios certificate has an empty body, "
        <> "and the transactions that it puts on the chain are in the "
        <> "endorser block that it certifies, which the LeiosDb holds. "
        <> "Pass --no-leios-db if this chain holds no such block."
  pure (Just path)
