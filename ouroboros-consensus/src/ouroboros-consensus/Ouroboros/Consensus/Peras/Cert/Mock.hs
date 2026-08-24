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

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLenOf
  , encodeListLen
  )
import Control.DeepSeq (NFData)
import Data.Containers.NonEmpty (NE)
import Data.Data (Proxy (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set.NonEmpty as NESet
import Data.Set.NonEmpty.Internal (NESet (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
  ( Point
  , StandardHash
  )
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
  ( Typeable blk
  , FromCBOR (Point blk)
  ) =>
  FromCBOR (MockPerasCert blk)
  where
  fromCBOR = do
    decodeListLenOf 3
    mockCertRound <- fromCBOR
    mockCertBlock <- fromCBOR
    mockCertVoters <- decodeNonEmptySet
    pure
      MockPerasCert
        { mockCertRound
        , mockCertBlock
        , mockCertVoters
        }
   where
    decodeNonEmptySet = do
      xs <- fromCBOR
      case NonEmpty.nonEmpty xs of
        Nothing -> fail "Expected a non-empty set of PerasSeatIndex"
        Just neSet -> pure (NESet.fromList neSet)

instance
  ( Typeable blk
  , ToCBOR (Point blk)
  ) =>
  ToCBOR (MockPerasCert blk)
  where
  toCBOR
    MockPerasCert
      { mockCertRound
      , mockCertBlock
      , mockCertVoters
      } =
      encodeListLen 3
        <> toCBOR mockCertRound
        <> toCBOR mockCertBlock
        <> toCBOR (NonEmpty.toList (NESet.toList mockCertVoters))

-- * Orphan instances

-- NOTE: we need this to be able to derive a couple of other classes for
-- 'NESet PerasSeatIndex'.
deriving instance Generic (NESet a)
