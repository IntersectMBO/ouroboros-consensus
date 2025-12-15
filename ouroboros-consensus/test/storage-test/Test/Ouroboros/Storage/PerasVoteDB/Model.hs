{-# LANGUAGE DeriveGeneric #-}

module Test.Ouroboros.Storage.PerasVoteDB.Model
  ( Model (..)
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
import Ouroboros.Consensus.Block.Abstract (Point, StandardHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasVoteBlock (..)
  , HasPerasVoteRound (..)
  , PerasCert (..)
  , PerasCfg
  , PerasParams (..)
  , PerasQuorumStakeThreshold (unPerasQuorumStakeThreshold)
  , PerasRoundNo
  , PerasVoteStake (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote
  , getPerasVoteStake
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  )
import Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( AddPerasVoteResult (..)
  , PerasVoteSnapshot
  )

data Model blk = Model
  { votes ::
      Map
        (PerasRoundNo, Point blk)
        (Set (WithArrivalTime (ValidatedPerasVote blk)))
  , certs ::
      Map
        PerasRoundNo
        (ValidatedPerasCert blk)
  , params :: PerasParams
  , open :: Bool
  }
  deriving (Show, Generic)

initModel :: PerasCfg blk -> Model blk
initModel cfg =
  Model
    { open = False
    , votes = Map.empty
    , certs = Map.empty
    , params = cfg
    }

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
    , votes = Map.empty
    , certs = Map.empty
    }

addVote ::
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  Model blk ->
  ( Maybe (AddPerasVoteResult blk)
  , Model blk
  )
addVote vote model = maybeResultAndNewModel
 where
  roundNo =
    getPerasVoteRound vote
  votedBlock =
    getPerasVoteBlock vote
  existingVotes =
    Map.findWithDefault Set.empty (roundNo, votedBlock) (votes model)
  extendedVotes =
    Set.insert vote existingVotes
  quorumStakeThreshold =
    unPerasQuorumStakeThreshold . perasQuorumStakeThreshold . params
  getVoteStake =
    unPerasVoteStake . getPerasVoteStake . forgetArrivalTime
  reachedQuorum =
    sum (getVoteStake <$> Set.toList extendedVotes)
      >= quorumStakeThreshold model
  result
    | Set.member vote existingVotes =
        PerasVoteAlreadyInDB
    | reachedQuorum =
        let cert =
              ValidatedPerasCert
                { vpcCert =
                    PerasCert
                      { pcCertRound = getPerasVoteRound vote
                      , pcCertBoostedBlock = getPerasVoteBlock vote
                      }
                , vpcCertBoost = perasWeight (params model)
                }
         in AddedPerasVoteAndGeneratedNewCert cert
    | otherwise =
        AddedPerasVoteButDidntGenerateNewCert
  maybeResultAndNewModel =
    case result of
      PerasVoteAlreadyInDB ->
        ( Just result
        , model
        )
      AddedPerasVoteAndGeneratedNewCert cert
        | Just cert' <- Map.lookup roundNo (certs model)
        , cert' /= cert ->
            ( Nothing -- 'EquivocatingCertError'
            , model
            )
        | otherwise ->
            ( Just result
            , model
                { certs =
                    Map.insert roundNo cert (certs model)
                , votes =
                    Map.insert (roundNo, votedBlock) extendedVotes (votes model)
                }
            )
      AddedPerasVoteButDidntGenerateNewCert ->
        ( Just result
        , model
            { votes =
                Map.insert (roundNo, votedBlock) extendedVotes (votes model)
            }
        )

getVoteSnapshot ::
  Model blk ->
  PerasVoteSnapshot blk
getVoteSnapshot _model =
  error "Not implemented"

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
garbageCollect _roundNo _model =
  error "Not implemented"
