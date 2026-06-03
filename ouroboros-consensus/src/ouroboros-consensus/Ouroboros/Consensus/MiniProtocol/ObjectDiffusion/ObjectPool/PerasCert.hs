{-# LANGUAGE FlexibleContexts #-}

-- | Instantiate 'ObjectPoolReader' and 'ObjectPoolWriter' using Peras
-- certificates from the 'PerasCertDB' (or the 'ChainDB' which is wrapping the
-- 'PerasCertDB').
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasCert
  ( makePerasCertPoolReaderFromCertDB
  , makePerasCertPoolWriterFromCertDB
  , makePerasCertPoolReaderFromChainDB
  , makePerasCertPoolWriterFromChainDB
  ) where

import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (PerasCert)
  , IsPerasCert (getPerasCertRound)
  , PerasRoundNo
  , ValidatedPerasCert (vpcCert)
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
  ( ObjectPoolReader (..)
  , ObjectPoolWriter (..)
  )
import Ouroboros.Consensus.Peras.Context
  ( LedgerStateHeaderStateSupportsPerasVoting
  , PerasEpochContextResolverHandle
  , verifyPerasCertInContext
  )
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB, getPerasEpochContextResolverHandle)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.PerasCertDB.API
  ( PerasCertDB
  , PerasCertTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasCertDB.API as PerasCertDB
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadSTM (STM, atomically)
  )

-- | TODO: replace by `Data.Map.take` as soon as we move to GHC 9.8
takeAscMap :: Int -> Map k v -> Map k v
takeAscMap n = Map.fromDistinctAscList . take n . Map.toAscList

-------------------------------------------------------------------------------
-- Readers
-------------------------------------------------------------------------------

-- | Internal helper: create a pool reader from a @getCertsAfter@ function.
makePerasCertPoolReader ::
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  ( PerasCertTicketNo ->
    STM m (Map PerasCertTicketNo (m (WithArrivalTime (ValidatedPerasCert blk))))
  ) ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReader getCertsAfterSTM =
  ObjectPoolReader
    { oprObjectId = getPerasCertRound
    , oprZeroTicketNo = PerasCertDB.zeroPerasCertTicketNo
    , oprObjectsAfter = \lastKnown limit -> do
        certsAfterLastKnownNoLimit <- getCertsAfterSTM lastKnown
        if Map.null certsAfterLastKnownNoLimit
          then pure Nothing
          else pure . Just $ do
            let certsAfterLastKnown = takeAscMap (fromIntegral limit) certsAfterLastKnownNoLimit
            traverse
              (\loadCertAction -> (vpcCert . forgetArrivalTime) <$> loadCertAction)
              certsAfterLastKnown
    }

makePerasCertPoolReaderFromCertDB ::
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDB m blk ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromCertDB perasCertDB =
  makePerasCertPoolReader
    (PerasCertDB.getCertsAfter perasCertDB)

makePerasCertPoolReaderFromChainDB ::
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  ChainDB m blk ->
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m
makePerasCertPoolReaderFromChainDB chainDB =
  makePerasCertPoolReader
    (ChainDB.getPerasCertsAfter chainDB)

-------------------------------------------------------------------------------
-- Writers
-------------------------------------------------------------------------------

-- | Create a pool writer directly from a 'PerasCertDB'. This is mostly meant
-- for tests against the 'PerasCertDB' in isolation; for actual production use,
-- see 'makePerasCertPoolWriterFromChainDB' which creates a pool writer from the
-- 'ChainDB' with proper handling of chain selection side-effects.
makePerasCertPoolWriterFromCertDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  , LedgerStateHeaderStateSupportsPerasVoting blk
  ) =>
  SystemTime m ->
  PerasCertDB m blk ->
  PerasEpochContextResolverHandle m blk ->
  ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromCertDB systemTime perasCertDB resolverHandle =
  ObjectPoolWriter
    { opwObjectId = getPerasCertRound
    , opwAddObjects = \certs -> do
        now <- systemTimeCurrent systemTime
        atomically $ do
          alreadyInDb <- PerasCertDB.getCertIds perasCertDB
          let certsNotAlreadyInDb = filter ((`Set.notMember` alreadyInDb) . getPerasCertRound) certs
          validatedCerts <- traverse (verifyPerasCertInContext resolverHandle) certsNotAlreadyInDb
          -- Some certs are invalid => reject the whole batch
          -- We could combine the two 'traverse' operations into one in which case
          -- any validated cert would be immediately added no matter what is the
          -- validity of the other certs in the batch.
          traverse_ (PerasCertDB.addCert perasCertDB . WithArrivalTime now) validatedCerts
    , opwHasObject = do
        certIds <- PerasCertDB.getCertIds perasCertDB
        pure $ \roundNo -> Set.member roundNo certIds
    }

-- | Create a pool writer from the 'ChainDB'. This properly handles any needed
-- chain selection side-effects.
makePerasCertPoolWriterFromChainDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  , LedgerStateHeaderStateSupportsPerasVoting blk
  ) =>
  SystemTime m ->
  ChainDB m blk ->
  ObjectPoolWriter PerasRoundNo (PerasCert blk) m
makePerasCertPoolWriterFromChainDB systemTime chainDB =
  let resolverHandle = getPerasEpochContextResolverHandle chainDB
   in ObjectPoolWriter
        { opwObjectId = getPerasCertRound
        , opwAddObjects = \certs -> do
            now <- systemTimeCurrent systemTime
            validatedCerts <- atomically $ do
              alreadyInDb <- ChainDB.getPerasCertIds chainDB
              let certsNotAlreadyInDb = filter ((`Set.notMember` alreadyInDb) . getPerasCertRound) certs
              traverse (verifyPerasCertInContext resolverHandle) certsNotAlreadyInDb
            -- Some certs are invalid => reject the whole batch
            -- We could combine the two 'traverse' operations into one in which case
            -- any validated cert would be immediately added no matter what is the
            -- validity of the other certs in the batch.
            traverse_ (ChainDB.addPerasCertAsync chainDB . WithArrivalTime now) validatedCerts
        , opwHasObject = do
            certIds <- ChainDB.getPerasCertIds chainDB
            pure $ \roundNo -> Set.member roundNo certIds
        }
