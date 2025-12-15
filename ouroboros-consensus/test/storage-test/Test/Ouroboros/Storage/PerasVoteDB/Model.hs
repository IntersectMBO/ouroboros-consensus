{-# LANGUAGE DeriveGeneric #-}

module Test.Ouroboros.Storage.PerasVoteDB.Model
  ( PerasVoteDbModelError (..)
  , Model (..)
  , initModel
  , openDB
  , closeDB
  , addVote
  , getVoteSnapshot
  , getForgedCertForRound
  , garbageCollect
  ) where

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
  , PerasVoteSnapshot (..)
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
  -- This voter has already voted in this round => ignore the vote
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
  -- Compute the next ticket number associated to this vote
  nextTicketNo =
    succ (lastTicketNo model)
  -- Prepare various data structures needed to update the model
  voteId =
    PerasVoteId{pviRoundNo = roundNo, pviVoterId = voter}
  voteTarget =
    PerasVoteTarget{pvtRoundNo = roundNo, pvtBlock = votedBlock}
  voteEntry =
    VoteEntry{veTicketNo = nextTicketNo, veVoter = voter, veVote = vote}
  -- Has this  voter already voted in this round?
  voterAlreadyVotedInRound =
    hasVote voteId model
  -- The existing votes for this round and block
  existingVotes =
    Map.findWithDefault Set.empty voteTarget (votes model)
  -- The extended set of votes including the new one
  extendedVotes =
    Set.insert voteEntry existingVotes
  -- Total stake represented by the extended set of votes
  extendedVotesStake =
    PerasVoteStake $
      sum $
        unPerasVoteStake
          . getPerasVoteStake
          . forgetArrivalTime
          . veVote
          <$> Set.toList extendedVotes
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

getVoteSnapshot ::
  Model blk ->
  PerasVoteSnapshot blk
getVoteSnapshot model =
  PerasVoteSnapshot
    { containsVote = \voteId ->
        hasVote
          voteId
          model
    , getVotesAfter = \ticketNo ->
        Map.fromList
          [ (tn, vote)
          | (_, vs) <- Map.toList (votes model)
          , VoteEntry{veTicketNo = tn, veVote = vote} <- Set.toList vs
          , tn > ticketNo
          ]
    }

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
