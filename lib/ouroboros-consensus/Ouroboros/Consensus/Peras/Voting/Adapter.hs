{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}

-- | Conversion between concrete Peras types and abstract committee types
module Ouroboros.Consensus.Peras.Voting.Adapter
  ( PerasVoteCompatibleWithVotingCommittee (..)
  , PerasCertCompatibleWithVotingCommittee (..)
  , PerasConversionError (..)
  , fromPerasSeatIndex
  , toPerasSeatIndex
  ) where

import Data.Containers.NonEmpty (HasNonEmpty (..))
import GHC.Generics (Generic)
import GHC.Word (Word16, Word64)
import NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.WFA (SeatIndex (..))
import Ouroboros.Consensus.Peras.Types (PerasSeatIndex (..))

-- | Conversion between (concrete) Peras votes and (abstract) committee votes.
--
-- NOTE: the functional dependency @vote -> crypto@ explicitly ties each
-- concrete Peras vote type to a specific crypto scheme.
class
  PerasVoteCompatibleWithVotingCommittee vote crypto committee
    | vote -> crypto
  where
  toPerasVote ::
    Committee.Vote crypto committee ->
    Either PerasConversionError vote
  fromPerasVote ::
    vote ->
    Either PerasConversionError (Committee.Vote crypto committee)

-- | Conversion between (concrete) Peras certificates and (abstract) committee
-- certificates.
--
-- NOTE: the functional dependency @cert -> crypto@ explicitly ties each
-- concrete Peras certificate type to a specific crypto scheme.
class
  PerasCertCompatibleWithVotingCommittee cert crypto committee
    | cert -> crypto
  where
  toPerasCert ::
    Committee.Cert crypto committee ->
    Either PerasConversionError cert
  fromPerasCert ::
    cert ->
    Either PerasConversionError (Committee.Cert crypto committee)

-- | Errors that can occur when converting between Peras and committee types
data PerasConversionError
  = EveryoneVotesButFoundNonPersistentVoterInVote SeatIndex
  | EveryoneVotesButFoundNonPersistentVotersInCert (NE [SeatIndex])
  | SeatIndexOverflowError Word64
  | CryptoError String
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | Convert a Peras seat index to a committee seat index.
fromPerasSeatIndex ::
  PerasSeatIndex ->
  SeatIndex
fromPerasSeatIndex (PerasSeatIndex seatIndex) =
  SeatIndex (fromIntegral @Word16 @Word64 seatIndex)

-- | Convert a committee seat index to a Peras seat index
--
-- NOTE: this can fail if the seat index in the committee vote or certificate
-- overflows the smaller 'Word16' type used by Peras votes and certificates.
-- In practice, this should never happen unless there is a bug in the voting
-- committee logic.
toPerasSeatIndex ::
  SeatIndex ->
  Either PerasConversionError PerasSeatIndex
toPerasSeatIndex (SeatIndex seatIndex)
  | seatIndex <= fromIntegral @Word16 @Word64 maxBound =
      Right (PerasSeatIndex (fromIntegral @Word64 @Word16 seatIndex))
  | otherwise =
      Left (SeatIndexOverflowError seatIndex)
