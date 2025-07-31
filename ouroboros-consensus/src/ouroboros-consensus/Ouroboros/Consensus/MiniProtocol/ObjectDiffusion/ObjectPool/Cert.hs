{-# LANGUAGE NamedFieldPuns #-}
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.Cert
  ( getCertPoolReaderFromCertDB
  , getCertPoolWriterFromCertDB
  , getCertPoolReaderFromChainDB
  , getCertPoolWriterFromChainDB
  ) where
import Ouroboros.Consensus.Storage.PerasCertDB.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.Block
import Data.Int (Int64)
import Ouroboros.Consensus.Storage.ChainDB.API

newtype CertTicketNo = TicketNo Int64 deriving (Eq, Ord, Show)


getCertPoolReaderFromCertDB :: PerasCertDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) CertTicketNo m
getCertPoolReaderFromCertDB = undefined
-- TODO: amesgen with fingertrees

getCertPoolWriterFromCertDB :: (StandardHash blk, Monad m) => PerasCertDB m blk -> ObjectPoolWriter PerasRoundNo (PerasCert blk) m
getCertPoolWriterFromCertDB PerasCertDB{addCert} =
  ObjectPoolWriter
    { wrGetObjectId = perasCertRound
    , objectPoolAddObjects =
        mapM_ addCert
    }

getCertPoolReaderFromChainDB :: ChainDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) CertTicketNo m
getCertPoolReaderFromChainDB = undefined

getCertPoolWriterFromChainDB :: (StandardHash blk, Monad m) => ChainDB m blk -> ObjectPoolWriter PerasRoundNo (PerasCert blk) m
getCertPoolWriterFromChainDB ChainDB{addPerasCert} =
  ObjectPoolWriter
    { wrGetObjectId = perasCertRound
    , objectPoolAddObjects =
        mapM_ addPerasCert
    }
