{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Ouroboros.Storage.PerasCertDB.Model
  ( Model (..)
  , initModel
  , openDB
  , addCert
  , getWeightSnapshot
  , getLatestCertSeen
  , garbageCollect
  , hasRoundNo
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff (ToExpr (..), defaultExprViaShow)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Peras.Weight
  ( PerasWeightSnapshot
  , mkPerasWeightSnapshot
  )

data Model blk = Model
  { certs :: Set (WithArrivalTime (ValidatedPerasCert blk))
  , latestCertSeen :: Maybe (WithArrivalTime (ValidatedPerasCert blk))
  , open :: Bool
  }
  deriving Generic

deriving instance StandardHash blk => Show (Model blk)

instance StandardHash blk => ToExpr (Model blk) where
  toExpr = defaultExprViaShow

initModel :: Model blk
initModel = Model{open = False, certs = Set.empty, latestCertSeen = Nothing}

openDB :: Model blk -> Model blk
openDB model = model{open = True}

addCert ::
  StandardHash blk =>
  Model blk -> WithArrivalTime (ValidatedPerasCert blk) -> Model blk
addCert model@Model{certs, latestCertSeen} cert
  | certs `hasRoundNo` cert = model
  | otherwise = model{certs = certs', latestCertSeen = latestCertSeen'}
 where
  certs' = Set.insert cert certs
  latestCertSeen' = case latestCertSeen of
    Nothing -> Just cert
    Just prev
      | getPerasCertRound cert > getPerasCertRound prev -> Just cert
      | otherwise -> Just prev

hasRoundNo ::
  Set (WithArrivalTime (ValidatedPerasCert blk)) ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  Bool
hasRoundNo certs cert =
  (getPerasCertRound cert) `Set.member` (Set.map getPerasCertRound certs)

getWeightSnapshot ::
  StandardHash blk =>
  Model blk -> PerasWeightSnapshot blk
getWeightSnapshot Model{certs} =
  mkPerasWeightSnapshot
    [ (getPerasCertBoostedBlock cert, getPerasCertBoost cert)
    | cert <- Set.toList certs
    ]

getLatestCertSeen ::
  Model blk -> Maybe (WithArrivalTime (ValidatedPerasCert blk))
getLatestCertSeen Model{latestCertSeen} =
  latestCertSeen

garbageCollect :: SlotNo -> Model blk -> Model blk
garbageCollect slotNo model@Model{certs} =
  model{certs = Set.filter keepCert certs}
 where
  keepCert cert = pointSlot (getPerasCertBoostedBlock cert) >= NotOrigin slotNo
