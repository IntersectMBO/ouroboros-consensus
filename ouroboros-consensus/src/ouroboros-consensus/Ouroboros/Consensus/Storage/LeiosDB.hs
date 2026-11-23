{-# LANGUAGE StandaloneKindSignatures #-}

module Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbArgs (..)
  , LeiosBackendArgs (..)
  , defaultArgs
  , sqliteArgs
  , openDB
  , module X
  ) where

import Control.Tracer
import Data.Kind
import Ouroboros.Consensus.Storage.LeiosDB.API
import Ouroboros.Consensus.Storage.LeiosDB.API as X
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite as X
import Ouroboros.Consensus.Storage.LeiosDB.Trace
import Ouroboros.Consensus.Storage.LeiosDB.Trace as X
import Ouroboros.Consensus.Util.Args

type LeiosDbArgs :: (Type -> Type) -> (Type -> Type) -> Type
data LeiosDbArgs f m = LeiosDbArgs
  { leiosDbTracer :: Tracer m TraceLeiosDb
  , leiosBackendArgs :: HKD f (LeiosBackendArgs m)
  }

newtype LeiosBackendArgs m = LeiosBackendArgs (Tracer m TraceLeiosDb -> m (LeiosDB m))

defaultArgs :: Monad m => LeiosDbArgs Defaults m
defaultArgs = LeiosDbArgs nullTracer NoDefault

sqliteArgs :: FilePath -> FilePath -> LeiosBackendArgs IO
sqliteArgs f1 f2 = LeiosBackendArgs $ \trcr -> newLeiosDBSQLite undefined trcr f1 f2

openDB :: Complete LeiosDbArgs m -> m (LeiosDB m)
openDB (LeiosDbArgs trcr (LeiosBackendArgs mk)) = mk trcr
