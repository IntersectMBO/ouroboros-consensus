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
  , validateMockPerasCert
  , forgeMockPerasCert
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
  , PerasVoteTarget (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVotesWithQuorum (..)
  )
import Ouroboros.Consensus.Node.Serialisation (SerialiseNodeToNode (..))
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Network.Util (ShowProxy (..))

-- | Mocked Peras certificates without crypto.
--
-- NOTE: this is parameterized around the concrete block type being certified.
data MockPerasCert blk
  = MockPerasCert
  { mockCertRound :: PerasRoundNo
  , mockCertBlock :: Point blk
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
      } =
      encodeListLen 2
        <> encode mockCertRound
        <> encode mockCertBlock
  decode = do
    decodeListLenOf 2
    mockCertRound <- decode
    mockCertBlock <- decode
    pure $
      MockPerasCert
        { mockCertRound
        , mockCertBlock
        }

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
      } =
      encodeListLen 2
        <> encodeNodeToNode ccfg version mockCertRound
        <> encodeNodeToNode ccfg version mockCertBlock
  decodeNodeToNode ccfg version = do
    decodeListLenOf 2
    mockCertRound <- decodeNodeToNode ccfg version
    mockCertBlock <- decodeNodeToNode ccfg version
    pure
      MockPerasCert
        { mockCertRound
        , mockCertBlock
        }

-- | Helper to write 'BlockSupportsPeras.validatePerasCert'.
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

-- | Helper to write 'BlockSupportsPeras.forgePerasCert'.
forgeMockPerasCert ::
  forall blk.
  PerasCert blk ~ MockPerasCert blk =>
  PerasParams ->
  ValidatedPerasVotesWithQuorum blk ->
  Either (PerasError blk) (ValidatedPerasCert blk)
forgeMockPerasCert params votes =
  return $
    ValidatedPerasCert
      { vpcCert =
          MockPerasCert
            { mockCertRound = pvtRoundNo (vpvqTarget votes)
            , mockCertBlock = pvtBlock (vpvqTarget votes)
            }
      , vpcCertBoost = perasWeight params
      }
