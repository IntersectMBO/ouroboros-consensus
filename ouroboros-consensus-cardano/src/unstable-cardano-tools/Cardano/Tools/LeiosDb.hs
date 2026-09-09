module Cardano.Tools.LeiosDb (LeiosDbSource (..), requireLeiosDbFile) where

import Control.Monad (unless)
import qualified System.Directory as Directory
import System.Exit (die)

-- | Which LeiosDb the tool works on, per @--leios-db@ and @--no-leios-db@.
data LeiosDbSource
  = -- | The LeiosDb file, derived from @--db@ directory defaults.
    LeiosDbFiles
  | -- | No file. db-analyser uses an empty in-memory LeiosDb, and db-truncater
    -- skips every LeiosDb step.
    NoLeiosDb

-- | The path of the LeiosDb file. Dies if that file does not exist.
--
-- The path to LeiosDB's volatile/immutable partition files must exist.
-- Tools such as db-analyser, db-truncater and other infer these path from
-- the '--db' option by appending the default names.
--
-- A missing file is fatal, because a tool cannot tell whether the chain holds a
-- cert-RB before it reads the chain. The operator passes @--no-leios-db@ to say
-- that the chain holds no cert-RB.
--
-- Hence this check, rather than a check inside the SQLite backend: that backend
-- opens with 'SQLOpenCreate' and it creates the schema when it finds no file.
-- So without this check the tool would write an empty leios.db.vol/leios.db.imm into the node's
-- directory and fail only at the first cert-RB.
requireLeiosDbFile ::
  -- | Path to LeiosDB's volatile/immutable partition file
  FilePath ->
  IO ()
requireLeiosDbFile path = do
  exists <- Directory.doesFileExist path
  unless exists $
    die $
      "No LeiosDb at "
        <> path
        <> ". A block that carries a Leios certificate has an empty body, "
        <> "and the transactions that it puts on the chain are in the "
        <> "endorser block that it certifies, which the LeiosDb holds. "
        <> "Pass --leios-db if the node writes that file elsewhere, or "
        <> "--no-leios-db if this chain holds no such block."
