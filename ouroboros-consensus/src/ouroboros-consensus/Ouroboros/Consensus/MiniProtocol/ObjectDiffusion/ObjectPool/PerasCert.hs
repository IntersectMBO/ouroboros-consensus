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
  (IOLike m, StandardHash blk) =>
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
              pure (getPerasCert <$> takeAscMap (fromIntegral limit) certsAfterLastKnown)
        pure $
          if Map.null certsAfterLastKnown
            then Nothing
            else Just loadCertsAfterLastKnown
    }

makePerasCertPoolReaderFromCertDB ::
  (IOLike m, StandardHash blk) =>
  PerasCertDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromCertDB perasCertDB =
  makePerasCertPoolReaderFromSnapshot (PerasCertDB.getCertSnapshot perasCertDB)

makePerasCertPoolWriterFromCertDB ::
  (StandardHash blk, IOLike m) =>
  PerasCertDB m blk -> ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromCertDB perasCertDB =
  ObjectPoolWriter
    { opwObjectId = getPerasCertRound
    , opwAddObjects = \certs -> do
        validatePerasCerts certs
          >>= mapM_ (PerasCertDB.addCert perasCertDB)
    , opwHasObject = do
        certSnapshot <- PerasCertDB.getCertSnapshot perasCertDB
        pure $ PerasCertDB.containsCert certSnapshot
    }

makePerasCertPoolReaderFromChainDB ::
  (IOLike m, StandardHash blk) =>
  ChainDB m blk -> ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromChainDB chainDB =
  makePerasCertPoolReaderFromSnapshot (ChainDB.getPerasCertSnapshot chainDB)

makePerasCertPoolWriterFromChainDB ::
  (StandardHash blk, IOLike m) =>
  ChainDB m blk -> ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromChainDB chainDB =
  ObjectPoolWriter
    { opwObjectId = getPerasCertRound
    , opwAddObjects = \certs -> do
        validatePerasCerts certs
          >>= mapM_ (ChainDB.addPerasCertAsync chainDB)
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
  let perasParams = mkPerasParams
  -- TODO pass down 'BlockConfig' when all the plumbing is in place
  -- see https://github.com/tweag/cardano-peras/issues/73
  -- see https://github.com/tweag/cardano-peras/issues/120
  case traverse (validatePerasCert perasParams) certs of
    Left validationErr -> throw (PerasCertValidationError validationErr)
    Right validatedCerts -> return validatedCerts
