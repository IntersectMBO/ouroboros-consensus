{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.PerasCertDB.API
  ( PerasCertDB (..)
  , AddPerasCertResult (..)

    -- * 'PerasCertSnapshot'
  , PerasCertSnapshot (..)
  , PerasCertTicketNo
  , zeroPerasCertTicketNo
  ) where

import Data.Map (Map)
import Data.Word (Word64)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Peras.Weight
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (WithFingerprint (..))

data PerasCertDB m blk = PerasCertDB
  { addCert :: WithArrivalTime (ValidatedPerasCert blk) -> m AddPerasCertResult
  -- ^ Add a Peras certificate to the database. The result indicates whether
  -- the certificate was actually added, or if it was already present.
  , getWeightSnapshot :: STM m (WithFingerprint (PerasWeightSnapshot blk))
  -- ^ Return the Peras weights in order compare the current selection against
  -- potential candidate chains, namely the weights for blocks not older than
  -- the current immutable tip. It might contain weights for even older blocks
  -- if they have not yet been garbage-collected.
  --
  -- The 'Fingerprint' is updated every time a new certificate is added, but it
  -- stays the same when certificates are garbage-collected.
  , getCertSnapshot :: STM m (PerasCertSnapshot blk)
  , garbageCollect :: SlotNo -> m ()
  -- ^ Garbage-collect state older than the given slot number.
  , closeDB :: m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDB" (PerasCertDB m blk)

data AddPerasCertResult = AddedPerasCertToDB | PerasCertAlreadyInDB
  deriving stock (Show, Eq)

data PerasCertSnapshot blk = PerasCertSnapshot
  { containsCert :: PerasRoundNo -> Bool
  -- ^ Do we have the certificate for this round?
  , getCertsAfter ::
      PerasCertTicketNo ->
      Map PerasCertTicketNo (WithArrivalTime (ValidatedPerasCert blk))
  -- ^ Get certificates after the given ticket number (excluded).
  -- The result is a map of ticket numbers to validated certificates.
  }

-- | A sequence number, incremented every time we receive a new certificate.
--
-- Note that we will /usually/ receive certificates monotonically by round
-- number, so round numbers could /almost/ fulfill the role of ticket numbers.
-- However, in certain edge cases (while catching up, or during cooldowns), this
-- might not be true, such as during syncing or during cooldown periods.
-- Therefore, for robustness, we choose to maintain dedicated ticket numbers
-- separately.
newtype PerasCertTicketNo = PerasCertTicketNo Word64
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, NoThunks)

zeroPerasCertTicketNo :: PerasCertTicketNo
zeroPerasCertTicketNo = PerasCertTicketNo 0
