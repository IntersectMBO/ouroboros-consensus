module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasCert
  ( makePerasCertPoolReaderFromCertDB
  , makePerasCertPoolWriterFromCertDB
  , makePerasCertPoolReaderFromChainDB
  , makePerasCertPoolWriterFromChainDB
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

makePerasCertPoolReader ::
  (IOLike m, StandardHash blk) =>
  STM m (PerasCertSnapshot blk) ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReader getCertSnapshot =
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
    , objectPoolZeroTicketNo = PerasCertDB.zeroPerasCertTicketNo
    }

makePerasCertPoolReaderFromCertDB ::
  (IOLike m, StandardHash blk) =>
  PerasCertDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromCertDB perasCertDB =
  makePerasCertPoolReader (PerasCertDB.getCertSnapshot perasCertDB)

makePerasCertPoolWriterFromCertDB ::
  (StandardHash blk, Monad m) =>
  PerasCertDB m blk -> ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromCertDB perasCertDB =
  ObjectPoolWriter
    { wrGetObjectId = perasCertRound
    , objectPoolAddObjects =
        mapM_ $ PerasCertDB.addCert perasCertDB
    }

makePerasCertPoolReaderFromChainDB ::
  (IOLike m, StandardHash blk) =>
  ChainDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromChainDB chainDB =
  makePerasCertPoolReader (ChainDB.getPerasCertSnapshot chainDB)

makePerasCertPoolWriterFromChainDB ::
  (StandardHash blk, Monad m) =>
  ChainDB m blk -> ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromChainDB chainDB =
  ObjectPoolWriter
    { wrGetObjectId = perasCertRound
    , objectPoolAddObjects =
        mapM_ $ ChainDB.addPerasCertAsync chainDB
    }
