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
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
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

deriving instance Show (PerasCert blk) => Show (Model blk)

instance Show (PerasCert blk) => ToExpr (Model blk) where
  toExpr = defaultExprViaShow

initModel :: Model blk
initModel = Model{open = False, certs = Set.empty, latestCertSeen = Nothing}

openDB :: Model blk -> Model blk
openDB model = model{open = True}

addCert ::
  ( Ord (PerasCert blk)
  , IsPerasCert (PerasCert blk) blk
  ) =>
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
  IsPerasCert (PerasCert blk) blk =>
  Set (WithArrivalTime (ValidatedPerasCert blk)) ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  Bool
hasRoundNo certs cert =
  (getPerasCertRound cert) `Set.member` (Set.map getPerasCertRound certs)

getWeightSnapshot ::
  ( IsPerasCert (PerasCert blk) blk
  , StandardHash blk
  ) =>
  Model blk -> PerasWeightSnapshot blk
getWeightSnapshot Model{certs} =
  mkPerasWeightSnapshot
    [ (getPerasCertPoint cert, vpcCertBoost (forgetArrivalTime cert))
    | cert <- Set.toList certs
    ]

getLatestCertSeen ::
  Model blk -> Maybe (WithArrivalTime (ValidatedPerasCert blk))
getLatestCertSeen Model{latestCertSeen} =
  latestCertSeen

garbageCollect ::
  IsPerasCert (PerasCert blk) blk =>
  SlotNo -> Model blk -> Model blk
garbageCollect slotNo model@Model{certs} =
  model{certs = Set.filter keepCert certs}
 where
  keepCert cert = pointSlot (getPerasCertPoint cert) >= NotOrigin slotNo
