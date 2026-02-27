{-# LANGUAGE DeriveGeneric #-}

module Test.Ouroboros.Storage.PerasVoteDB.Model
  ( PerasVoteDbModelError (..)
  , Model (..)
  , initModel
  , openDB
  , closeDB
  , addVote
  , getVoteIds
  , getVotesAfter
  , getForgedCertForRound
  , garbageCollect
  ) where

import Control.Exception (assert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block.Abstract (StandardHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasVoteBlock (..)
  , HasPerasVoteRound (..)
  , PerasCert (..)
  , PerasCfg
  , PerasParams (..)
  , PerasRoundNo
  , PerasVoteId (..)
  , PerasVoteStake (..)
  , PerasVoteTarget (..)
  , PerasVoterId
  , ValidatedPerasCert (..)
  , ValidatedPerasVote
  , getPerasCertBoostedBlock
  , getPerasVoteStake
  , getPerasVoteVoterId
  , stakeAboveThreshold
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  )
import Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( AddPerasVoteResult (..)
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  )

data VoteEntry blk = VoteEntry
  { veTicketNo :: PerasVoteTicketNo
  -- ^ The ticket number assigned to this vote
  , veVoter :: PerasVoterId
  -- ^ The voter ID
  , veVote :: WithArrivalTime (ValidatedPerasVote blk)
  -- ^ The vote itself
  }
  deriving (Show, Eq, Ord, Generic)

data PerasVoteDbModelError = MultipleWinnersInRound PerasRoundNo
  deriving (Show, Generic)

data Model blk = Model
  { open :: Bool
  -- ^ Is the database open?
  , params :: PerasParams
  -- ^ Configuration parameters
  , lastTicketNo :: PerasVoteTicketNo
  -- ^ The last issued ticket number
  , votes :: Map (PerasVoteTarget blk) (Set (VoteEntry blk))
  -- ^ Collection of votes indexed by target (round number, boosted block)
  , certs :: Map PerasRoundNo (ValidatedPerasCert blk)
  -- ^ Forged certificates indexed by round number
  }
  deriving (Show, Generic)

initModel :: PerasCfg blk -> Model blk
initModel cfg =
  Model
    { open = False
    , params = cfg
    , lastTicketNo = zeroPerasVoteTicketNo
    , votes = Map.empty
    , certs = Map.empty
    }

-- | Check whether a given voter has already voted in a given round
--
-- NOTE: while this is an innefficient traversal, it allows the model to be as
-- trivial as possible. The actual PerasVoteDB implementation uses a separate
-- collection to track this information efficienty, at the cost of added
-- complexity.
hasVote ::
  PerasVoteId blk ->
  Model blk ->
  Bool
hasVote voteId model =
  Set.member voteId voteIds
 where
  voteIds =
    Set.unions $
      [ Set.map
          ( \ve ->
              PerasVoteId
                { pviRoundNo = pvtRoundNo voteTarget
                , pviVoterId = veVoter ve
                }
          )
          votesForTarget
      | (voteTarget, votesForTarget) <- Map.toList (votes model)
      ]

openDB ::
  Model blk ->
  Model blk
openDB model =
  model
    { open = True
    }

closeDB ::
  Model blk ->
  Model blk
closeDB model =
  model
    { open = False
    , lastTicketNo = zeroPerasVoteTicketNo
    , votes = Map.empty
    , certs = Map.empty
    }

addVote ::
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  Model blk ->
  ( Either PerasVoteDbModelError (AddPerasVoteResult blk)
  , Model blk
  )
addVote vote model
  -- The ID of a vote is a pair (voterId, roundNo). So checking if the voter has
  -- already voted in this round means checking if the pair (voterId, roundNo)
  -- is already present in the model i.e. if the vote is already in the model.
  -- In which case, we can ignore it.
  --
  -- NOTE: this is under the assumption that a voter doesn't cast two different
  -- votes for the same round (that is, with the same ID but different body).
  | voterAlreadyVotedInRound =
      ( Right $
          PerasVoteAlreadyInDB
      , model
      )
  -- A quorum was reached, but there is another cert already boosting a different
  -- block in this round => integrity violation (shouldn't happen in practice)
  | reachedQuorum
  , Just existingCert <- certAtRound
  , getPerasCertBoostedBlock freshCert /= getPerasCertBoostedBlock existingCert =
      ( Left $
          MultipleWinnersInRound roundNo
      , model
      )
  -- A quorum was reached for the first time (when there is no existing
  -- certificate for the given round) => causing a new cert to be generated
  | reachedQuorum
  , Nothing <- certAtRound =
      -- Also ensure that we didn't already have a quorum before adding this
      -- vote in a more direct way: the stake represented by the existing votes
      -- must be below the threshold.
      assert (not hadQuorum) $
        ( Right $
            AddedPerasVoteAndGeneratedNewCert freshCert
        , model
            { votes =
                Map.insert voteTarget extendedVotes (votes model)
            , certs =
                Map.insert roundNo freshCert (certs model)
            , lastTicketNo =
                nextTicketNo
            }
        )
  -- Otherwise, just add the vote without generating a new cert
  | otherwise =
      ( Right $
          AddedPerasVoteButDidntGenerateNewCert
      , model
          { votes =
              Map.insert voteTarget extendedVotes (votes model)
          , lastTicketNo =
              nextTicketNo
          }
      )
 where
  -- Extract relevant information from the vote
  roundNo =
    getPerasVoteRound vote
  votedBlock =
    getPerasVoteBlock vote
  voter =
    getPerasVoteVoterId vote
  -- Compute the next ticket number associated to this vote.
  -- NOTE: This is a 64-bit counter, so there's no practical risk of overflow.
  nextTicketNo =
    succ (lastTicketNo model)
  -- Prepare various data structures needed to update the model
  voteId =
    PerasVoteId{pviRoundNo = roundNo, pviVoterId = voter}
  voteTarget =
    PerasVoteTarget{pvtRoundNo = roundNo, pvtBlock = votedBlock}
  voteEntry =
    VoteEntry{veTicketNo = nextTicketNo, veVoter = voter, veVote = vote}
  -- Has this voter already voted in this round?
  voterAlreadyVotedInRound =
    hasVote voteId model
  -- The existing votes for this round and block
  existingVotes =
    Map.findWithDefault Set.empty voteTarget (votes model)
  -- The extended set of votes including the new one
  extendedVotes =
    Set.insert voteEntry existingVotes
  -- Get the total stake of a set of votes
  getTotalStake =
    PerasVoteStake
      . sum
      . fmap
        ( unPerasVoteStake
            . getPerasVoteStake
            . forgetArrivalTime
            . veVote
        )
      . Set.toList
  -- Total stake represented by the existing votes
  existingVotesStake =
    getTotalStake existingVotes
  -- Total stake represented by the extended set of votes
  extendedVotesStake =
    getTotalStake extendedVotes
  -- Did we already have a quorum before adding this new vote?
  hadQuorum =
    stakeAboveThreshold (params model) existingVotesStake
  -- Did we reach the quorum threshold with this new vote?
  reachedQuorum =
    stakeAboveThreshold (params model) extendedVotesStake
  -- The existing certificate (if any) for this round
  certAtRound =
    Map.lookup roundNo (certs model)
  -- The fresh certificate that would be generated if a quorum is reached
  freshCert =
    ValidatedPerasCert
      { vpcCert =
          PerasCert
            { pcCertRound = getPerasVoteRound vote
            , pcCertBoostedBlock = getPerasVoteBlock vote
            }
      , vpcCertBoost = perasWeight (params model)
      }

getVoteIds ::
  Model blk ->
  Set (PerasVoteId blk)
getVoteIds model =
  Set.unions $
    [ Set.map
        ( \ve ->
            PerasVoteId
              { pviRoundNo = pvtRoundNo voteTarget
              , pviVoterId = veVoter ve
              }
        )
        votesForTarget
    | (voteTarget, votesForTarget) <- Map.toList (votes model)
    ]

getVotesAfter ::
  PerasVoteTicketNo ->
  Model blk ->
  Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk))
getVotesAfter ticketNo model =
  Map.fromList
    [ (veTicketNo ve, veVote ve)
    | votesForTarget <- Map.elems (votes model)
    , ve <- Set.toList votesForTarget
    , veTicketNo ve > ticketNo
    ]

getForgedCertForRound ::
  PerasRoundNo ->
  Model blk ->
  Maybe (ValidatedPerasCert blk)
getForgedCertForRound roundNo model =
  Map.lookup roundNo (certs model)

garbageCollect ::
  PerasRoundNo ->
  Model blk ->
  Model blk
garbageCollect roundNo model =
  model
    { votes =
        Map.filterWithKey
          (\voteTarget _ -> pvtRoundNo voteTarget >= roundNo)
          (votes model)
    , certs =
        Map.filterWithKey
          (\r _ -> r >= roundNo)
          (certs model)
    }
