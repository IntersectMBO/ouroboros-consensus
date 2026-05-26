{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.TreeDiff (ToExpr (..), defaultExprViaShow)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block (SlotNo, WithOrigin (..), pointSlot)
import Ouroboros.Consensus.Block.Abstract (StandardHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasCert (..)
  , IsPerasVote (..)
  , PerasParams
  , PerasRoundNo
  , PerasSeatIndex
  , PerasVoteId (..)
  , PerasVoteTarget (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , VoteWeight (..)
  , perasWeight
  , weightAboveThreshold
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( AddPerasVoteResult (..)
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  )

data VoteEntry blk = VoteEntry
  { veTicketNo :: PerasVoteTicketNo
  -- ^ The ticket number assigned to this vote
  , veVoter :: PerasSeatIndex
  -- ^ The seat index of the voter
  , veVote :: WithArrivalTime (ValidatedPerasVote blk)
  -- ^ The vote itself
  }

deriving instance Show (PerasVote blk) => Show (VoteEntry blk)
deriving instance Eq (PerasVote blk) => Eq (VoteEntry blk)
deriving instance Ord (PerasVote blk) => Ord (VoteEntry blk)
deriving instance Generic (VoteEntry blk)

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

-- deriving (Show, Generic)

deriving instance
  ( StandardHash blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  ) =>
  Show (Model blk)
deriving instance Generic (Model blk)

instance
  ( StandardHash blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  ) =>
  ToExpr (Model blk)
  where
  toExpr = defaultExprViaShow

initModel :: PerasParams -> Model blk
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
                , pviSeatIndex = veVoter ve
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
  ( StandardHash blk
  , Ord (PerasVote blk)
  , PerasCert blk ~ MockPerasCert blk
  , IsPerasVote (PerasVote blk) blk
  , IsPerasCert (PerasCert blk) blk
  ) =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  Model blk ->
  ( Either PerasVoteDbModelError (AddPerasVoteResult blk)
  , Model blk
  )
addVote vote model
  -- The ID of a vote is a pair (seatIndex, roundNo). So checking if the voter has
  -- already voted in this round means checking if the pair (seatIndex, roundNo)
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
  , getPerasCertPoint freshCert /= getPerasCertPoint existingCert =
      ( Left $
          MultipleWinnersInRound roundNo
      , model
      )
  -- A quorum was reached for the first time (when there is no existing
  -- certificate for the given round) => causing a new cert to be generated
  | reachedQuorum
  , Nothing <- certAtRound =
      -- Also ensure that we didn't already have a quorum before adding this
      -- vote in a more direct way: the weight represented by the existing votes
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
    getPerasVotePoint vote
  voter =
    getPerasVoteSeatIndex vote
  -- Compute the next ticket number associated to this vote.
  -- NOTE: This is a 64-bit counter, so there's no practical risk of overflow.
  nextTicketNo =
    succ (lastTicketNo model)
  -- Prepare various data structures needed to update the model
  voteId =
    PerasVoteId{pviRoundNo = roundNo, pviSeatIndex = voter}
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
  -- Get the total weight of a set of votes
  getTotalWeight =
    VoteWeight
      . sum
      . fmap
        ( unVoteWeight
            . vpvVoteWeight
            . forgetArrivalTime
            . veVote
        )
      . Set.toList
  -- Total weight represented by the existing votes
  existingVotesWeight =
    getTotalWeight existingVotes
  -- Total weight represented by the extended set of votes
  extendedVotesWeight =
    getTotalWeight extendedVotes
  -- Did we already have a quorum before adding this new vote?
  hadQuorum =
    weightAboveThreshold (params model) existingVotesWeight
  -- Did we reach the quorum threshold with this new vote?
  reachedQuorum =
    weightAboveThreshold (params model) extendedVotesWeight
  -- The existing certificate (if any) for this round
  certAtRound =
    Map.lookup roundNo (certs model)
  -- The fresh certificate that would be generated if a quorum is reached
  freshCert =
    ValidatedPerasCert
      { vpcCert =
          MockPerasCert
            { mockCertRound = roundNo
            , mockCertBlock = votedBlock
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
              , pviSeatIndex = veVoter ve
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
  SlotNo ->
  Model blk ->
  Model blk
garbageCollect slotNo model =
  model
    { votes =
        Map.filterWithKey
          (\voteTarget _ -> not (Set.member (pvtRoundNo voteTarget) roundsToDelete))
          (votes model)
    , certs =
        Map.filterWithKey
          (\roundNo _ -> not (Set.member roundNo roundsToDelete))
          (certs model)
    }
 where
  -- A round is deleted when ALL of its vote targets point to blocks strictly
  -- older than the GC threshold, i.e. when even its youngest target is old.
  roundsToDelete =
    Map.keysSet $
      Map.filter (< NotOrigin slotNo) youngestSlotByRound

  -- The youngest target slot across all vote targets for a given round.
  youngestSlotByRound =
    Map.fromListWith
      max
      [ (pvtRoundNo vt, pointSlot (pvtBlock vt))
      | vt <- Map.keys (votes model)
      ]
