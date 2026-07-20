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

-- | Mocked Peras votes without crypto.
module Ouroboros.Consensus.Peras.Vote.Mock
  ( MockPerasVote (..)
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
import Ouroboros.Consensus.Node.Serialisation (SerialiseNodeToNode (..))
import Ouroboros.Consensus.Peras.Types
  ( BoostedBlock
  , PerasRoundNo
  , PerasSeatIndex (..)
  )
import Ouroboros.Consensus.Peras.Vote.Class (IsPerasVote (..))
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Network.Util (ShowProxy (..))

-- | Mocked Peras votes without crypto.
--
-- NOTE: this is parameterized around the concrete block type being voted for.
data MockPerasVote blk
  = MockPerasVote
  { mockVoteRound :: PerasRoundNo
  , mockVoteBlock :: Point blk
  , mockVoteSeatIndex :: PerasSeatIndex
  }

deriving instance StandardHash blk => Show (MockPerasVote blk)
deriving instance StandardHash blk => Eq (MockPerasVote blk)
deriving instance StandardHash blk => Ord (MockPerasVote blk)
deriving instance StandardHash blk => NoThunks (MockPerasVote blk)
deriving instance StandardHash blk => NFData (MockPerasVote blk)
deriving instance Generic (MockPerasVote blk)

type instance BoostedBlock (MockPerasVote blk) = Point blk

instance IsPerasVote (MockPerasVote blk) blk where
  getPerasVoteRound = mockVoteRound
  getPerasVoteBlock = mockVoteBlock
  getPerasVoteSeatIndex = mockVoteSeatIndex

instance ShowProxy blk => ShowProxy (MockPerasVote blk) where
  showProxy _ = "MockPerasVote(" <> showProxy (Proxy @blk) <> ")"

instance
  Serialise (HeaderHash blk) =>
  Serialise (MockPerasVote blk)
  where
  encode
    MockPerasVote
      { mockVoteRound
      , mockVoteBlock
      , mockVoteSeatIndex
      } =
      encodeListLen 3
        <> encode mockVoteRound
        <> encode mockVoteBlock
        <> encode mockVoteSeatIndex
  decode = do
    decodeListLenOf 3
    mockVoteRound <- decode
    mockVoteBlock <- decode
    mockVoteSeatIndex <- decode
    pure
      MockPerasVote
        { mockVoteRound
        , mockVoteBlock
        , mockVoteSeatIndex
        }

instance
  ConvertRawHash blk =>
  SerialiseNodeToNode blk (MockPerasVote blk)
  where
  encodeNodeToNode
    ccfg
    version
    MockPerasVote
      { mockVoteRound
      , mockVoteBlock
      , mockVoteSeatIndex
      } =
      encodeListLen 3
        <> encodeNodeToNode ccfg version mockVoteRound
        <> encodeNodeToNode ccfg version mockVoteBlock
        <> encodeNodeToNode ccfg version mockVoteSeatIndex
  decodeNodeToNode ccfg version = do
    decodeListLenOf 3
    mockVoteRound <- decodeNodeToNode ccfg version
    mockVoteBlock <- decodeNodeToNode ccfg version
    mockVoteSeatIndex <- decodeNodeToNode ccfg version
    pure
      MockPerasVote
        { mockVoteRound
        , mockVoteBlock
        , mockVoteSeatIndex
        }
