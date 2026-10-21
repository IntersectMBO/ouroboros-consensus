{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Tracing.Peras () where

import Cardano.Logging (LogFormatting (..))
import Data.Aeson (Value (..), (.=))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Void (absurd)
import Ouroboros.Consensus.Block.SupportsPeras (PerasVotingCommitteeError)
import Ouroboros.Consensus.Committee.WFA (SeatIndex (..))
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import Ouroboros.Consensus.Peras.Types (PerasCertSize (..), VoteWeight (..))
import Ouroboros.Consensus.Peras.Void (VoidPerasError (..))
import Ouroboros.Consensus.Peras.Voting.Adapter (PerasConversionError (..))
import Ouroboros.Consensus.Tracing.Committee ()

--
-- instances for PerasError
--

instance LogFormatting (VoidPerasError blk) where
  forMachine _dtal = absurd . unVoidPerasError

instance
  LogFormatting (PerasVotingCommitteeError blk) =>
  LogFormatting (V1.PerasError blk)
  where
  forMachine dtal (V1.PerasVotingWFAError e) =
    mconcat
      [ "kind" .= String "PerasVotingWFAError"
      , forMachine dtal e
      ]
  forMachine dtal (V1.PerasVotingCommitteeError e) =
    mconcat
      [ "kind" .= String "PerasVotingCommitteeError"
      , forMachine dtal e
      ]
  forMachine dtal (V1.PerasVotingConversionError e) =
    mconcat
      [ "kind" .= String "PerasVotingConversionError"
      , forMachine dtal e
      ]
  forMachine _dtal (V1.PerasQuorumNotReachedError e) =
    mconcat
      [ "kind" .= String "PerasQuorumNotReachedError"
      , "voteWeight" .= unVoteWeight e
      ]
  forMachine _dtal (V1.PerasCertTooLargeError limit actual) =
    mconcat
      [ "kind" .= String "PerasCertTooLargeError"
      , "limit" .= unPerasCertSize limit
      , "actual" .= unPerasCertSize actual
      ]

instance LogFormatting PerasConversionError where
  forMachine _dtal (EveryoneVotesButFoundNonPersistentVoterInVote seatIndex) =
    mconcat
      [ "kind" .= String "EveryoneVotesButFoundNonPersistentVoterInVote"
      , "seatIndex" .= unSeatIndex seatIndex
      ]
  forMachine _dtal (EveryoneVotesButFoundNonPersistentVotersInCert seatIndices) =
    mconcat
      [ "kind" .= String "EveryoneVotesButFoundNonPersistentVotersInCert"
      , "seatIndices" .= fmap unSeatIndex (NonEmpty.toList seatIndices)
      ]
  forMachine _dtal (SeatIndexOverflowError word64) =
    mconcat
      [ "kind" .= String "SeatIndexOverflowError"
      , "seatIndex" .= word64
      ]
  forMachine _dtal (CryptoError msg) =
    mconcat
      [ "kind" .= String "CryptoError"
      , "message" .= msg
      ]
