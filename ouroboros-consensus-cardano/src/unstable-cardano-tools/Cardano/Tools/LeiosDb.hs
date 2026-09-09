module Cardano.Tools.LeiosDb (LeiosDbSource (..), requireLeiosDbFile) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import qualified System.Directory as Directory
import System.Exit (die)
import qualified System.FilePath as FilePath

-- | Which LeiosDb the tool works on, per @--leios-db@ and @--no-leios-db@.
data LeiosDbSource
  = -- | The LeiosDb file. 'Nothing' is the @--db@ directory default, see
    -- 'requireLeiosDbFile'.
    LeiosDbFile (Maybe FilePath)
  | -- | No file. db-analyser uses an empty in-memory LeiosDb, and db-truncater
    -- skips every LeiosDb step.
    NoLeiosDb

-- | The path of the LeiosDb file. Dies if that file does not exist.
--
-- Without @--leios-db@ the tool reads @leios.db@ under the @--db@ directory,
-- which is where a node with the default @LeiosDbConfig@ writes it. That
-- default holds only when the node keeps all its databases under one path. A
-- node that splits the immutable path from the volatile one writes the file
-- under the volatile path, and a node can also name another file altogether.
-- @--leios-db@ is for both cases.
--
-- A missing file is fatal, because a tool cannot tell whether the chain holds a
-- cert-RB before it reads the chain. The operator passes @--no-leios-db@ to say
-- that the chain holds no cert-RB.
--
-- Hence this check, rather than a check inside the SQLite backend: that backend
-- opens with 'SQLOpenCreate' and it creates the schema when it finds no file.
-- So without this check the tool would write an empty leios.db into the node's
-- directory and fail only at the first cert-RB.
requireLeiosDbFile ::
  -- | The ChainDB directory, that is @--db@.
  FilePath ->
  -- | The @--leios-db@ path, if the operator gave one.
  Maybe FilePath ->
  IO FilePath
requireLeiosDbFile dbDir mPath = do
  let path = fromMaybe (dbDir FilePath.</> "leios.db") mPath
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
  pure path
