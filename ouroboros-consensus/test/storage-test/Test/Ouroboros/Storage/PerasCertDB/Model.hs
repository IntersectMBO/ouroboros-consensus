{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Ouroboros.Storage.PerasCertDB.Model
  ( Model (..)
  , initModel
  , openDB
  , closeDB
  , addCert
  , getWeightSnapshot
  , garbageCollect
  , hasRoundNo
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Peras.Weight
  ( PerasWeightSnapshot
  , mkPerasWeightSnapshot
  )

data Model blk = Model
  { certs :: Set (ValidatedPerasCert blk)
  , open :: Bool
  }
  deriving Generic

deriving instance StandardHash blk => Show (Model blk)

initModel :: Model blk
initModel = Model{open = False, certs = Set.empty}

openDB :: Model blk -> Model blk
openDB model = model{open = True}

closeDB :: Model blk -> Model blk
closeDB _ = Model{open = False, certs = Set.empty}

addCert ::
  StandardHash blk =>
  Model blk -> ValidatedPerasCert blk -> Model blk
addCert model@Model{certs} cert
  | certs `hasRoundNo` cert = model
  | otherwise = model{certs = Set.insert cert certs}

hasRoundNo ::
  StandardHash blk =>
  Set (ValidatedPerasCert blk) ->
  ValidatedPerasCert blk ->
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

garbageCollect :: StandardHash blk => SlotNo -> Model blk -> Model blk
garbageCollect slot model@Model{certs} =
  model{certs = Set.filter keepCert certs}
 where
  keepCert cert = pointSlot (getPerasCertBoostedBlock cert) >= NotOrigin slot
