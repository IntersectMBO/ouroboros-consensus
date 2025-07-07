{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Ouroboros.Storage.PerasCertDB.Model
  ( PerasCertDBModel,
    initPerasCertDBModel,
    openDBModel,
    closeDBModel,
    addCertModel,
    getWeightSnapshotModel
  ) where
import Ouroboros.Consensus.Block (PerasCert, boostPerCert, perasCertBoostedBlock, StandardHash)
import Data.Set (Set)
import GHC.Generics (Generic)
import Data.Proxy (Proxy)
import qualified Data.Set as Set
import Ouroboros.Consensus.Storage.PerasCertDB.API
import qualified Data.Map as Map
import Data.Vector.Internal.Check (HasCallStack)
import Ouroboros.Consensus.Storage.PerasCertDB.Impl (PerasCertDbError(..))
import Ouroboros.Consensus.Util.CallStack (prettyCallStack)

data PerasCertDBModel blk = PerasCertDBModel
    {
        open :: Bool,
        certs :: Set (PerasCert blk)
    } deriving Generic

deriving instance (StandardHash blk) => Show (PerasCertDBModel blk)

initPerasCertDBModel :: Proxy blk -> PerasCertDBModel blk
initPerasCertDBModel _ = PerasCertDBModel
    { open = False
    , certs = Set.empty
    }

openDBModel :: PerasCertDBModel blk -> PerasCertDBModel blk
openDBModel model = model { open = True }

closeDBModel :: PerasCertDBModel blk -> PerasCertDBModel blk
closeDBModel model = model { open = False }

addCertModel :: (HasCallStack, StandardHash blk) => PerasCertDBModel blk -> PerasCert blk -> Either PerasCertDbError (PerasCertDBModel blk)
addCertModel model cert =
    if open model
    then Right model { certs = Set.insert cert (certs model) }
    else Left (ClosedDBError prettyCallStack)

getWeightSnapshotModel :: (HasCallStack, StandardHash blk) => PerasCertDBModel blk -> Either PerasCertDbError (PerasWeightSnapshot blk)
getWeightSnapshotModel model =
    if open model
    then
        Right $ PerasWeightSnapshot {
            getPerasWeightSnapshot = Set.fold
                (\cert acc -> Map.insertWith (<>) (perasCertBoostedBlock cert) boostPerCert acc)
                Map.empty
                (certs model)
        }
    else Left (ClosedDBError prettyCallStack)
