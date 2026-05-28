{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Mocked Peras certificates without crypto.
module Ouroboros.Consensus.Peras.Cert.Mock
  ( MockPerasCert (..)
  , forgeMockPerasCert
  , validateMockPerasCert
  ) where

import Cardano.Binary (decodeListLenOf, encodeListLen)
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import Data.Data (Proxy (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
  ( ConvertRawHash
  , HeaderHash
  , Point
  , StandardHash
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , BoostedBlock
  , IsPerasCert (..)
  , PerasParams (..)
  , PerasRoundNo
  , PerasVoteCollection (pvcTarget, pvcVotes)
  , PerasVoteCollectionWithQuorum (..)
  , PerasVoteTarget (..)
  , ValidatedPerasCert (..), PerasSeatIndex, ValidatedPerasVote (vpvVote), IsPerasVote (getPerasVoteSeatIndex)
  )
import Ouroboros.Consensus.Node.Serialisation (SerialiseNodeToNode (..))
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Network.Util (ShowProxy (..))
import Data.Containers.NonEmpty (NE)
import Data.Set (Set)
import qualified Data.Set.NonEmpty as NESet
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime(forgetArrivalTime))
import Ouroboros.Consensus.Peras.Vote.Mock (MockPerasVote)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.List.NonEmpty as NonEmpty

-- | Mocked Peras certificates without crypto.
--
-- NOTE: this is parameterized around the concrete block type being certified.
data MockPerasCert blk
  = MockPerasCert
  { mockCertRound :: PerasRoundNo
  , mockCertBlock :: Point blk
  , mockCertVoters :: NE (Set PerasSeatIndex)
  }

deriving instance StandardHash blk => Show (MockPerasCert blk)
deriving instance StandardHash blk => Eq (MockPerasCert blk)
deriving instance StandardHash blk => Ord (MockPerasCert blk)
deriving instance StandardHash blk => NoThunks (MockPerasCert blk)
deriving instance StandardHash blk => NFData (MockPerasCert blk)
deriving instance Generic (MockPerasCert blk)

type instance BoostedBlock (MockPerasCert blk) = Point blk
instance IsPerasCert (MockPerasCert blk) blk where
  getPerasCertRound = mockCertRound
  getPerasCertBlock = mockCertBlock

instance ShowProxy blk => ShowProxy (MockPerasCert blk) where
  showProxy _ = "MockPerasCert(" <> showProxy (Proxy @blk) <> ")"

instance
  Serialise (HeaderHash blk) =>
  Serialise (MockPerasCert blk)
  where
  encode
    MockPerasCert
      { mockCertRound
      , mockCertBlock
      , mockCertVoters
      } =
      encodeListLen 3
        <> encode mockCertRound
        <> encode mockCertBlock
        <> encode (NonEmpty.toList . NESet.toList $ mockCertVoters)
  decode = do
    decodeListLenOf 3
    mockCertRound <- decode
    mockCertBlock <- decode
    mockCertVoters <- decodeNonEmptySet
    pure $
      MockPerasCert
        { mockCertRound
        , mockCertBlock
        , mockCertVoters
        }
   where decodeNonEmptySet = do
            xs <- decode
            case NonEmpty.nonEmpty xs of
              Nothing -> fail "Expected a non-empty set of PerasSeatIndex"
              Just neSet -> pure $ NESet.fromList neSet

instance
  ConvertRawHash blk =>
  SerialiseNodeToNode blk (MockPerasCert blk)
  where
  encodeNodeToNode
    ccfg
    version
    MockPerasCert
      { mockCertRound
      , mockCertBlock
      , mockCertVoters
      } =
      encodeListLen 3
        <> encodeNodeToNode ccfg version mockCertRound
        <> encodeNodeToNode ccfg version mockCertBlock
        <> encode (NonEmpty.toList . NESet.toList $ mockCertVoters)
  decodeNodeToNode ccfg version = do
    decodeListLenOf 3
    mockCertRound <- decodeNodeToNode ccfg version
    mockCertBlock <- decodeNodeToNode ccfg version
    mockCertVoters <- decodeNodeToNodeNonEmptySet ccfg version
    pure
      MockPerasCert
        { mockCertRound
        , mockCertBlock
        , mockCertVoters
        }
   where decodeNodeToNodeNonEmptySet _ccfg _version = do
            xs <- decode
            case NonEmpty.nonEmpty xs of
              Nothing -> fail "Expected a non-empty set of PerasSeatIndex"
              Just neSet -> pure $ NESet.fromList neSet

-- | Helper to write 'BlockSupportsPeras.forgePerasCert'.
forgeMockPerasCert ::
  forall blk.
  (PerasCert blk ~ MockPerasCert blk, PerasVote blk ~ MockPerasVote blk)=>
  PerasParams ->
  PerasVoteCollectionWithQuorum blk ->
  Either (PerasError blk) (ValidatedPerasCert blk)
forgeMockPerasCert params votes = do
  let target = pvcTarget . forgetQuorum $ votes
  let voters = NESet.fromList . fmap getPerasVoteSeatIndex . NEMap.elems . pvcVotes . forgetQuorum $ votes
  Right
    ValidatedPerasCert
      { vpcCert =
          MockPerasCert
            { mockCertRound = pvtRoundNo target
            , mockCertBlock = pvtBlock target
            , mockCertVoters = voters
            }
      , vpcCertBoost = perasWeight params
      }

-- | Helper to write 'BlockSupportsPeras.verifyPerasCert'.
--
-- WARNING: we do not perform any validation whatsoever for mocked certificates.
validateMockPerasCert ::
  forall blk.
  PerasParams ->
  PerasCert blk ->
  Either (PerasError blk) (ValidatedPerasCert blk)
validateMockPerasCert params cert =
  Right
    ValidatedPerasCert
      { vpcCert = cert
      , vpcCertBoost = perasWeight params
      }
