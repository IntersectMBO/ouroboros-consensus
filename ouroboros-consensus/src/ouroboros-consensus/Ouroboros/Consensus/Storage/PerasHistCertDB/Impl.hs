module Ouroboros.Consensus.Storage.PerasHistCertDB.Impl (createDB) where

import Ouroboros.Consensus.Storage.PerasHistCertDB.API
import Ouroboros.Consensus.Util.IOLike (IOLike)

createDB :: IOLike m => m (PerasHistCertDB m blk)
createDB =
  return $
    PerasHistCertDB
      { appendCert = undefined
      }
