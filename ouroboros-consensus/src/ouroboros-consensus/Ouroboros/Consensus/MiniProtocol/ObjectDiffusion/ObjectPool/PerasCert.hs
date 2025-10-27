{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Instantiate 'ObjectPoolReader' and 'ObjectPoolWriter' using Peras
-- certificates from the 'PerasCertDB' (or the 'ChainDB' which is wrapping the
-- 'PerasCertDB').
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasCert
  ( makePerasCertPoolReaderFromCertDB
  , makePerasCertPoolWriterFromCertDB
  , makePerasCertPoolReaderFromChainDB
  , makePerasCertPoolWriterFromChainDB
  ) where

import Control.Monad ((>=>))
import qualified Data.Map as Map
import GHC.Exception (throw)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , WithArrivalTime (..)
  , addArrivalTime
  )
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

makePerasCertPoolReaderFromSnapshot ::
  IOLike m =>
  STM m (PerasCertSnapshot blk) ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromSnapshot getCertSnapshot =
  ObjectPoolReader
    { oprObjectId = getPerasCertRound
    , oprZeroTicketNo = PerasCertDB.zeroPerasCertTicketNo
    , oprObjectsAfter = \lastKnown limit -> do
        certSnapshot <- getCertSnapshot
        pure $
          take (fromIntegral limit) $
            [ (ticketNo, getPerasCertRound cert, pure (vpcCert (forgetArrivalTime cert)))
            | (ticketNo, cert) <-
                Map.toAscList $
                  PerasCertDB.getCertsAfter certSnapshot lastKnown
            ]
    }

makePerasCertPoolReaderFromCertDB ::
  IOLike m =>
  PerasCertDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromCertDB perasCertDB =
  makePerasCertPoolReaderFromSnapshot (PerasCertDB.getCertSnapshot perasCertDB)

makePerasCertPoolWriterFromCertDB ::
  (StandardHash blk, IOLike m) =>
  SystemTime m ->
  PerasCertDB m blk ->
  ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromCertDB systemTime perasCertDB =
  ObjectPoolWriter
    { opwObjectId = getPerasCertRound
    , opwAddObjects = addPerasCerts systemTime (PerasCertDB.addCert perasCertDB)
    , opwHasObject = do
        certSnapshot <- PerasCertDB.getCertSnapshot perasCertDB
        pure $ PerasCertDB.containsCert certSnapshot
    }

makePerasCertPoolReaderFromChainDB ::
  IOLike m =>
  ChainDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromChainDB chainDB =
  makePerasCertPoolReaderFromSnapshot (ChainDB.getPerasCertSnapshot chainDB)

makePerasCertPoolWriterFromChainDB ::
  (StandardHash blk, IOLike m) =>
  SystemTime m ->
  ChainDB m blk ->
  ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromChainDB systemTime chainDB =
  ObjectPoolWriter
    { opwObjectId = getPerasCertRound
    , opwAddObjects = addPerasCerts systemTime (ChainDB.addPerasCertAsync chainDB)
    , opwHasObject = do
        certSnapshot <- ChainDB.getPerasCertSnapshot chainDB
        pure $ PerasCertDB.containsCert certSnapshot
    }

data PerasCertInboundException
  = forall blk. PerasCertValidationError (PerasValidationErr blk)

deriving instance Show PerasCertInboundException

instance Exception PerasCertInboundException

-- | Validate a list of 'PerasCert's, throwing a 'PerasCertInboundException' if
-- any of them are invalid.
validatePerasCerts ::
  (StandardHash blk, MonadThrow m) =>
  [PerasCert blk] ->
  m [ValidatedPerasCert blk]
validatePerasCerts certs = do
  let perasCfg = makePerasCfg Nothing
  -- TODO replace the mocked-up Nothing with a real
  -- 'BlockConfig' when all the plumbing is in place
  -- see https://github.com/tweag/cardano-peras/issues/73
  -- see https://github.com/tweag/cardano-peras/issues/120
  case traverse (validatePerasCert perasCfg) certs of
    Left validationErr -> throw (PerasCertValidationError validationErr)
    Right validatedCerts -> return validatedCerts

-- | Add a list of 'PerasCert's into an object pool.
--
-- NOTE: we first validate the certificates, throwing an exception if any of
-- them are invalid. We then wrap them with their arrival time, and finally add
-- them to the pool using the provided adder function.
--
-- The order of the first two operations (i.e., validation and timestamping) are
-- rather arbitrary, and the abstract Peras protocol just assumes it can happen
-- "within" a slot.
addPerasCerts ::
  (StandardHash blk, MonadThrow m) =>
  SystemTime m ->
  (WithArrivalTime (ValidatedPerasCert blk) -> m a) ->
  [PerasCert blk] ->
  m ()
addPerasCerts systemTime adder = do
  validatePerasCerts
    >=> mapM (addArrivalTime systemTime)
    >=> mapM_ adder
