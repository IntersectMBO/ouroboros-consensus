module Cardano.Tools.LeiosDb (openLeiosDb) where

import Control.Monad (unless)
import Control.Tracer (nullTracer)
import LeiosDemoDb (LeiosDbHandle, newLeiosDBInMemory, newLeiosDBSQLite)
import qualified System.Directory as Directory
import System.Exit (die)
import qualified System.FilePath as FilePath

-- | Open the @leios.db@ that the node writes under its ChainDB directory.
--
-- The tool derives that path from @--db@ rather than take one of its own. An
-- operator who puts the file elsewhere can symlink it into place.
--
-- A missing file is fatal, because a tool cannot tell whether the chain holds a
-- cert-RB before it reads the chain. The caller passes 'True' to say that the
-- chain holds no cert-RB, and that an empty in-memory LeiosDb is enough.
--
-- Hence this check, rather than a check inside the SQLite backend: that backend
-- opens with 'SQLOpenCreate' and it creates the schema when it finds no file.
-- So without this check the tool would write an empty leios.db into the node's
-- directory and fail only at the first cert-RB.
openLeiosDb ::
  -- | Use an empty in-memory LeiosDb.
  Bool ->
  -- | The ChainDB directory.
  FilePath ->
  IO (LeiosDbHandle IO)
openLeiosDb stubbedLeiosDb dbDir
  | stubbedLeiosDb = newLeiosDBInMemory
  | otherwise = do
      let leiosDbPath = dbDir FilePath.</> "leios.db"
      exists <- Directory.doesFileExist leiosDbPath
      unless exists $
        die $
          "No LeiosDb at "
            <> leiosDbPath
            <> ". A block that carries a Leios certificate has an empty body, "
            <> "and the transactions that it puts on the chain are in the "
            <> "endorser block that it certifies, which the LeiosDb holds. "
            <> "Pass --stubbed-leios-db if this chain holds no such block."
      newLeiosDBSQLite nullTracer leiosDbPath
