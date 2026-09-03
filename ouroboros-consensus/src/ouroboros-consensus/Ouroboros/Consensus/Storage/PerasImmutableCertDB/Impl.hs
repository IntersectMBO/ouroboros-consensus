module Ouroboros.Consensus.Storage.PerasImmutableCertDB.Impl (createDB) where

import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Storage.PerasImmutableCertDB.API
import Ouroboros.Consensus.Util.IOLike (IOLike)

createDB :: IOLike m => m (PerasImmutableCertDB m blk)
createDB =
  return $
    PerasHistCertDB
      { addCert = undefined
      , getPointCerts = undefined
      }

implAddCert :: ValidatedPerasCert blk -> m AddPerasImmutableCertResult
implAddCert = undefined

implGetCerts :: Point block -> m [ValidatedPerasCert blk]
implGetCerts = undefined
