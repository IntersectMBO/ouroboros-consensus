{-# LANGUAGE StandaloneKindSignatures #-}

module Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbArgs (..)
  , LeiosBackendArgs (..)
  , defaultArgs
  , sqliteArgs
  , openDB
  , module X
  ) where

import Control.ResourceRegistry (ResourceRegistry)
import Control.Tracer
import Data.Kind
import Ouroboros.Consensus.Storage.LeiosDB.API
import Ouroboros.Consensus.Storage.LeiosDB.API as X
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite as X
import Ouroboros.Consensus.Storage.LeiosDB.Trace
import Ouroboros.Consensus.Storage.LeiosDB.Trace as X
import Ouroboros.Consensus.Util.Args
import System.FilePath
import System.FilePath ((</>))

type LeiosDbArgs :: (Type -> Type) -> (Type -> Type) -> Type
data LeiosDbArgs f m = LeiosDbArgs
  { leiosDbTracer :: Tracer m TraceLeiosDb
  , leiosBackendArgs :: HKD f (LeiosBackendArgs m)
  }

-- | How to open the LeiosDB. The registry is the ChainDB's, which owns whatever
-- the backend runs behind the scenes.
newtype LeiosBackendArgs m
  = LeiosBackendArgs (ResourceRegistry m -> Tracer m TraceLeiosDb -> m (LeiosDB m))

defaultArgs :: Monad m => LeiosDbArgs Defaults m
defaultArgs = LeiosDbArgs nullTracer NoDefault

-- | The SQLite backend, its volatile and immutable files beside the VolatileDB
-- and the ImmutableDB, in the given directories.
sqliteArgs ::
  -- | Directory of the volatile file
  FilePath ->
  -- | Directory of the immutable file
  FilePath ->
  LeiosBackendArgs IO
sqliteArgs volDir immDir = LeiosBackendArgs $ \registry trcr ->
  newLeiosDBSQLite registry trcr (volDir </> "leios.vol.db") (immDir </> "leios.imm.db")

openDB :: ResourceRegistry m -> Complete LeiosDbArgs m -> m (LeiosDB m)
openDB registry (LeiosDbArgs trcr (LeiosBackendArgs mk)) = mk registry trcr
