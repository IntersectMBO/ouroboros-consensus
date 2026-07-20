{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Mocked Peras certificates without crypto.
module Ouroboros.Consensus.Peras.Cert.Mock
  ( MockPerasCert (..)
  ) where

import Cardano.Binary (decodeListLenOf, encodeListLen)
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import Data.Containers.NonEmpty (NE)
import Data.Data (Proxy (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set.NonEmpty as NESet
import Data.Set.NonEmpty.Internal (NESet (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
  ( ConvertRawHash
  , HeaderHash
  , Point
  , StandardHash
  )
import Ouroboros.Consensus.Node.Serialisation (SerialiseNodeToNode (..))
import Ouroboros.Consensus.Peras.Cert.Class (IsPerasCert (..))
import Ouroboros.Consensus.Peras.Types
  ( BoostedBlock
  , PerasRoundNo
  , PerasSeatIndex
  )
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Network.Util (ShowProxy (..))

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
    pure
      MockPerasCert
        { mockCertRound
        , mockCertBlock
        , mockCertVoters
        }
   where
    decodeNonEmptySet = do
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
   where
    decodeNodeToNodeNonEmptySet _ccfg _version = do
      xs <- decode
      case NonEmpty.nonEmpty xs of
        Nothing -> fail "Expected a non-empty set of PerasSeatIndex"
        Just neSet -> pure $ NESet.fromList neSet

-- * Orphan instances

-- NOTE: we need this to be able to derive a couple of other classes for
-- 'NESet PerasSeatIndex'.
deriving instance Generic (NESet a)
