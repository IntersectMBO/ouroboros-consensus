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

import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Exception (throw)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , WithArrivalTime (..)
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

-- | TODO: replace by `Data.Map.take` as soon as we move to GHC 9.8
takeAscMap :: Int -> Map k v -> Map k v
takeAscMap n = Map.fromDistinctAscList . take n . Map.toAscList

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
        let certsAfterLastKnown =
              PerasCertDB.getCertsAfter certSnapshot lastKnown
        let loadCertsAfterLastKnown =
              pure $
                fmap
                  (vpcCert . forgetArrivalTime)
                  (takeAscMap (fromIntegral limit) certsAfterLastKnown)
        pure $
          if Map.null certsAfterLastKnown
            then Nothing
            else Just loadCertsAfterLastKnown
    }

makePerasCertPoolReaderFromCertDB ::
  IOLike m =>
  PerasCertDB m blk ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
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
    , opwAddObjects = \certs -> do
        addPerasCerts systemTime (PerasCertDB.addCert perasCertDB) certs
    , opwHasObject = do
        certSnapshot <- PerasCertDB.getCertSnapshot perasCertDB
        pure $ PerasCertDB.containsCert certSnapshot
    }

makePerasCertPoolReaderFromChainDB ::
  IOLike m =>
  ChainDB m blk ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
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
    , opwAddObjects = \certs -> do
        addPerasCerts systemTime (ChainDB.addPerasCertAsync chainDB) certs
    , opwHasObject = do
        certSnapshot <- ChainDB.getPerasCertSnapshot chainDB
        pure $ PerasCertDB.containsCert certSnapshot
    }

data PerasCertInboundException
  = forall blk. PerasCertValidationError [PerasValidationErr blk]

deriving instance Show PerasCertInboundException

instance Exception PerasCertInboundException

-- | Add a batch of Peras certs to a pool after validating them.
addPerasCerts ::
  (StandardHash blk, MonadSTM m) =>
  SystemTime m ->
  (WithArrivalTime (ValidatedPerasCert blk) -> m a) ->
  [PerasCert blk] ->
  m ()
addPerasCerts systemTime addCert certs = do
  now <- systemTimeCurrent systemTime
  case validatePerasCerts certs of
    -- All certs are valid => add them to the pool
    ([], validatedCerts) ->
      mapM_
        (addCert . WithArrivalTime now)
        validatedCerts
    -- Some certs are invalid => reject the whole batch
    (errs, _) ->
      throw (PerasCertValidationError errs)

-- | Validate a batch of Peras certs.
validatePerasCerts ::
  StandardHash blk =>
  [PerasCert blk] ->
  ([PerasValidationErr blk], [ValidatedPerasCert blk])
validatePerasCerts certs = do
  let perasParams = mkPerasParams
  -- TODO pass down 'BlockConfig' when all the plumbing is in place
  -- see https://github.com/tweag/cardano-peras/issues/73
  -- see https://github.com/tweag/cardano-peras/issues/120
  partitionEithers (validatePerasCert perasParams <$> certs)
