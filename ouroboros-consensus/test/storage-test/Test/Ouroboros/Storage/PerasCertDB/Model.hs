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
  , settleCertsUpTo
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
import Ouroboros.Consensus.Storage.PerasCertDB.API
  ( AddPerasCertResult (..)
  , WithBoostedBlockStatus (..)
  , forgetBoostedBlockStatus
  )

data Model blk = Model
  { certs :: Set (WithArrivalTime (ValidatedPerasCert blk))
  , latestCertSeen :: Maybe (WithBoostedBlockStatus (WithArrivalTime (ValidatedPerasCert blk)))
  , unsettled :: Set PerasRoundNo
  -- ^ The rounds of the certificates in 'certs' that have not been settled.
  , open :: Bool
  }
  deriving Generic

deriving instance Show (PerasCert blk) => Show (Model blk)

instance Show (PerasCert blk) => ToExpr (Model blk) where
  toExpr = defaultExprViaShow

initModel :: Model blk
initModel =
  Model
    { open = False
    , certs = Set.empty
    , latestCertSeen = Nothing
    , unsettled = Set.empty
    }

openDB :: Model blk -> Model blk
openDB model = model{open = True}

addCert ::
  ( Ord (PerasCert blk)
  , IsPerasCert (PerasCert blk) blk
  ) =>
  Model blk -> WithArrivalTime (ValidatedPerasCert blk) -> (AddPerasCertResult, Model blk)
addCert model@Model{certs, latestCertSeen, unsettled} cert
  | certs `hasRoundNo` cert = (PerasCertAlreadyInDB, model)
  | otherwise =
      ( AddedPerasCertToDB
      , model
          { certs = certs'
          , latestCertSeen = latestCertSeen'
          , unsettled = Set.insert (getPerasCertRound cert) unsettled
          }
      )
 where
  certs' = Set.insert cert certs
  latestCertSeen' =
    case latestCertSeen of
      Nothing ->
        Just (CertBoostingBlockInVolatileDB cert)
      Just prev
        | getPerasCertRound cert
            > getPerasCertRound (forgetBoostedBlockStatus prev) ->
            Just (CertBoostingBlockInVolatileDB cert)
        | otherwise ->
            Just prev

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
  Model blk ->
  Maybe (WithBoostedBlockStatus (WithArrivalTime (ValidatedPerasCert blk)))
getLatestCertSeen Model{latestCertSeen} =
  latestCertSeen

-- | Settle the unsettled certificates boosting slots up to the given one, and
-- return their rounds.
settleCertsUpTo ::
  IsPerasCert (PerasCert blk) blk =>
  WithOrigin SlotNo -> Model blk -> (Set PerasRoundNo, Model blk)
settleCertsUpTo slot model@Model{certs, unsettled} =
  (settled, model{unsettled = unsettled Set.\\ settled})
 where
  settled =
    Set.fromList
      [ getPerasCertRound cert
      | cert <- Set.toList certs
      , pointSlot (getPerasCertPoint cert) <= slot
      , getPerasCertRound cert `Set.member` unsettled
      ]

-- | Remove the settled certificates boosting slots older than the given one.
garbageCollect ::
  IsPerasCert (PerasCert blk) blk =>
  SlotNo -> Model blk -> Model blk
garbageCollect slotNo model@Model{certs, latestCertSeen, unsettled} =
  model
    { certs = Set.filter keepCert certs
    , latestCertSeen = updateIfBoostingGarbageCollectedBlock <$> latestCertSeen
    }
 where
  keepCert cert =
    pointSlot (getPerasCertPoint cert) >= NotOrigin slotNo
      || getPerasCertRound cert `Set.member` unsettled

  updateIfBoostingGarbageCollectedBlock cert
    | pointSlot (getPerasCertPoint (forgetBoostedBlockStatus cert))
        < NotOrigin slotNo =
        CertBoostingBlockNoLongerInVolatileDB (forgetBoostedBlockStatus cert)
    | otherwise =
        cert
