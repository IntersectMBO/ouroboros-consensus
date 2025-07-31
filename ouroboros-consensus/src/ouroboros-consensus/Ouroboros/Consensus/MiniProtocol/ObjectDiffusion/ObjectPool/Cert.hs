{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.Cert
  ( getCertPoolReaderFromCertDB
  , getCertPoolWriterFromCertDB
  , getCertPoolReaderFromChainDB
  , getCertPoolWriterFromChainDB
  ) where

import Data.Functor ((<&>))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.PerasCertDB.API
  ( PerasCertDB
  , PerasCertSnapshot
  , PerasCertTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasCertDB.API as PerasCertDB
import Ouroboros.Consensus.Util.IOLike

mkCertPoolReader ::
  (IOLike m, StandardHash blk) =>
  STM m (PerasCertSnapshot blk) ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
mkCertPoolReader getCertSnapshot =
  ObjectPoolReader
    { rdrGetObjectId = perasCertRound
    , objectPoolGetSnapshot =
        getCertSnapshot <&> \snap ->
          ObjectPoolSnapshot
            { objectPoolObjectsAfter = \ticketNo ->
                [ (cert, tno, sz)
                | (cert, tno) <- PerasCertDB.getCertsAfter snap ticketNo
                , let sz = 0 -- TODO
                ]
            , objectPoolHasObject = PerasCertDB.containsCert snap
            }
    , objectPoolZeroIndex = PerasCertDB.zeroPerasCertTicketNo
    }

getCertPoolReaderFromCertDB ::
  (IOLike m, StandardHash blk) =>
  PerasCertDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
getCertPoolReaderFromCertDB perasCertDB =
  mkCertPoolReader (PerasCertDB.getCertSnapshot perasCertDB)

getCertPoolWriterFromCertDB ::
  (StandardHash blk, Monad m) =>
  PerasCertDB m blk -> ObjectPoolWriter PerasRoundNo (PerasCert blk) m
getCertPoolWriterFromCertDB perasCertDB =
  ObjectPoolWriter
    { wrGetObjectId = perasCertRound
    , objectPoolAddObjects =
        mapM_ $ PerasCertDB.addCert perasCertDB
    }

getCertPoolReaderFromChainDB ::
  (IOLike m, StandardHash blk) =>
  ChainDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
getCertPoolReaderFromChainDB chainDB =
  mkCertPoolReader (ChainDB.getPerasCertSnapshot chainDB)

getCertPoolWriterFromChainDB ::
  (StandardHash blk, Monad m) =>
  ChainDB m blk -> ObjectPoolWriter PerasRoundNo (PerasCert blk) m
getCertPoolWriterFromChainDB chainDB =
  ObjectPoolWriter
    { wrGetObjectId = perasCertRound
    , objectPoolAddObjects =
        mapM_ $ ChainDB.addPerasCert chainDB
    }
