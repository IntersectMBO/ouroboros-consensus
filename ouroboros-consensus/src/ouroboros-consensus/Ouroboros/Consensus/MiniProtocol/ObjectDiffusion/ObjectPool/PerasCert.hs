{-# LANGUAGE FlexibleContexts #-}
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

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Exception (throw)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , WithArrivalTime (..)
  , addArrivalTime
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
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
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
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
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  PerasCertDB m blk ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromCertDB perasCertDB =
  makePerasCertPoolReaderFromSnapshot (PerasCertDB.getCertSnapshot perasCertDB)

makePerasCertPoolWriterFromCertDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  PerasCfg blk ->
  SystemTime m ->
  PerasCertDB m blk ->
  ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromCertDB perasCfg systemTime perasCertDB =
  ObjectPoolWriter
    { opwObjectId = getPerasCertRound
    , opwAddObjects =
        addPerasCerts
          perasCfg
          systemTime
          (PerasCertDB.addCert perasCertDB)
    , opwHasObject = do
        certSnapshot <- PerasCertDB.getCertSnapshot perasCertDB
        pure $ PerasCertDB.containsCert certSnapshot
    }

makePerasCertPoolReaderFromChainDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  ChainDB m blk ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromChainDB chainDB =
  makePerasCertPoolReaderFromSnapshot (ChainDB.getPerasCertSnapshot chainDB)

makePerasCertPoolWriterFromChainDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  PerasCfg blk ->
  SystemTime m ->
  ChainDB m blk ->
  ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromChainDB perasCfg systemTime chainDB =
  ObjectPoolWriter
    { opwObjectId = getPerasCertRound
    , opwAddObjects =
        addPerasCerts
          perasCfg
          systemTime
          (ChainDB.addPerasCertAsync chainDB)
    , opwHasObject = do
        certSnapshot <- ChainDB.getPerasCertSnapshot chainDB
        pure $ PerasCertDB.containsCert certSnapshot
    }

data PerasCertInboundException where
  PerasCertValidationError ::
    BlockSupportsPeras blk =>
    PerasValidationErr blk ->
    PerasCertInboundException

deriving instance Show PerasCertInboundException

instance Exception PerasCertInboundException

-- | Validate a list of 'PerasCert's, throwing a 'PerasCertInboundException' if
-- any of them are invalid.
validatePerasCerts ::
  ( BlockSupportsPeras blk
  , MonadThrow m
  ) =>
  PerasCfg blk ->
  [PerasCert blk] ->
  m [ValidatedPerasCert blk]
validatePerasCerts perasCfg certs = do
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
  ( BlockSupportsPeras blk
  , MonadThrow m
  ) =>
  PerasCfg blk ->
  SystemTime m ->
  (WithArrivalTime (ValidatedPerasCert blk) -> m a) ->
  [PerasCert blk] ->
  m ()
addPerasCerts perasCfg systemTime adder certs = do
  validatePerasCerts perasCfg certs
    >>= mapM (addArrivalTime systemTime)
    >>= mapM_ adder
