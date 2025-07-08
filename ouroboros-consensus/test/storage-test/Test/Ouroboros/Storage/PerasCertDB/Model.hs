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
  ) where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block (PerasCert, StandardHash, boostPerCert, perasCertBoostedBlock)
import Ouroboros.Consensus.Storage.PerasCertDB.API (PerasWeightSnapshot (..))

data Model blk = Model
  { certs :: Set (PerasCert blk)
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
  Model blk -> PerasCert blk -> Model blk
addCert model@Model{certs} cert =
  model{certs = Set.insert cert certs}

getWeightSnapshot ::
  StandardHash blk =>
  Model blk -> PerasWeightSnapshot blk
getWeightSnapshot Model{certs} = snap
 where
  snap =
    PerasWeightSnapshot
      { getPerasWeightSnapshot =
          Set.fold
            (\cert acc -> Map.insertWith (<>) (perasCertBoostedBlock cert) boostPerCert acc)
            Map.empty
            certs
      }
