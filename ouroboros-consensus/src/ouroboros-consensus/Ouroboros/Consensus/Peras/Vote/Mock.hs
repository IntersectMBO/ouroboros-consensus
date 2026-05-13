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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Mocked Peras votes without crypto.
module Ouroboros.Consensus.Peras.Vote.Mock
  ( MockPerasVote (..)
  , validateMockPerasVote
  ) where

import Cardano.Binary (decodeListLenOf, encodeListLen)
import Codec.Serialise (Serialise (..))
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
  , IsPerasVote (..)
  , PerasVoteStake
  , ValidatedPerasVote (..)
  )
import Ouroboros.Consensus.Node.Serialisation (SerialiseNodeToNode (..))
import Ouroboros.Consensus.Peras.Params (PerasParams)
import Ouroboros.Consensus.Peras.Types
  ( PerasRoundNo
  , PerasVoteStakeDistr
  , PerasVoterId (..)
  )
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Network.Util (ShowProxy (..))

-- | Mocked Peras votes without crypto.
--
-- NOTE: this is parameterized around the concrete block type being voted for.
data MockPerasVote blk
  = MockPerasVote
  { mockVoteRound :: PerasRoundNo
  , mockVoteBlock :: Point blk
  , mockVoteVoterId :: PerasVoterId
  , mockVoteStake :: PerasVoteStake
  -- ^ This field is unique to the mocked vote, and allows us to bypass the
  -- need for a 'PerasVoteStakeDistr' when creating validated votes in tests.
  }

deriving instance StandardHash blk => Show (MockPerasVote blk)
deriving instance StandardHash blk => Eq (MockPerasVote blk)
deriving instance StandardHash blk => Ord (MockPerasVote blk)
deriving instance StandardHash blk => NoThunks (MockPerasVote blk)
deriving instance Generic (MockPerasVote blk)

instance IsPerasVote (MockPerasVote blk) blk where
  getPerasVoteRound = mockVoteRound
  getPerasVoteBlock = mockVoteBlock
  getPerasVoteVoterId = mockVoteVoterId

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
      , mockVoteVoterId
      } =
      encodeListLen 3
        <> encode mockVoteRound
        <> encode mockVoteBlock
        <> encode mockVoteVoterId
  decode = do
    decodeListLenOf 3
    mockVoteRound <- decode
    mockVoteBlock <- decode
    mockVoteVoterId <- decode
    pure $
      MockPerasVote
        { mockVoteRound
        , mockVoteBlock
        , mockVoteVoterId
        , mockVoteStake = 0
        -- NOTE: stakes are never sent over the wire, but computed locally from
        -- the stake distribution. We might need to change this in the future if
        -- we ever need roundtrip tests using mocked votes, but for now this is
        -- sufficient for our needs.
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
      , mockVoteVoterId
      } =
      encodeListLen 3
        <> encodeNodeToNode ccfg version mockVoteRound
        <> encodeNodeToNode ccfg version mockVoteBlock
        <> encodeNodeToNode ccfg version mockVoteVoterId
  decodeNodeToNode ccfg version = do
    decodeListLenOf 3
    mockVoteRound <- decodeNodeToNode ccfg version
    mockVoteBlock <- decodeNodeToNode ccfg version
    mockVoteVoterId <- decodeNodeToNode ccfg version
    pure
      MockPerasVote
        { mockVoteRound
        , mockVoteBlock
        , mockVoteVoterId
        , mockVoteStake = 0
        -- NOTE: stakes are never sent over the wire, but computed locally from
        -- the stake distribution. We might need to change this in the future if
        -- we ever need roundtrip tests using mocked votes, but for now this is
        -- sufficient for our needs.
        }

-- | Helper to write 'BlockSupportsPeras.validatePerasVote'.
--
-- WARNING: we do not perform any validation whatsoever for mocked votes.
validateMockPerasVote ::
  forall blk.
  PerasVote blk ~ MockPerasVote blk =>
  PerasParams ->
  PerasVoteStakeDistr ->
  PerasVote blk ->
  Either (PerasError blk) (ValidatedPerasVote blk)
validateMockPerasVote _params _stakeDistr vote =
  Right
    ValidatedPerasVote
      { vpvVote = vote
      , vpvVoteStake = mockVoteStake vote
      }
