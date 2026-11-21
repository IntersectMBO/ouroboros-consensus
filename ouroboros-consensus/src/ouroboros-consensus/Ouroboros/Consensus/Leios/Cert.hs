{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- | The claim a block's Leios certificate makes, and whether the certificate
-- establishes it.
module Ouroboros.Consensus.Leios.Cert
  ( LeiosClaim (..)
  , LeiosCertRejection (..)
  , verifyLeiosClaim
  ) where

import Cardano.Crypto.Leios (LeiosCert, VerificationError, verifyLeiosCert)
import Cardano.Ledger.BaseTypes (unboundRational)
import Control.Monad.Except (Except, withExcept)
import Control.Monad.Trans.Except (except)
import Data.Functor (void)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Ouroboros.Consensus.Block (SlotNo)
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement, LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types (RankingBlockHash)

-- | What a block containing a certificate claims: that the endorser block its
-- predecessor announced is certified, by the committee in the ledger view at
-- the block's slot.
data LeiosClaim = LeiosClaim
  { leiosClaimSlot :: !SlotNo
  -- ^ The predecessor's slot
  , leiosClaimAnnouncement :: !LeiosAnnouncement
  -- ^ What the predecessor announced
  , leiosClaimLedgerView :: !LeiosLedgerView
  -- ^ The Leios part of the ledger view at the block's slot
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | Why a block's Leios certificate is invalid
data LeiosCertRejection
  = -- | The block's predecessor announced nothing certifiable.
    LeiosCertWithoutAnnouncement
  | LeiosCertInvalid !VerificationError
  deriving stock (Eq, Show)
  deriving NoThunks via InspectHeapNamed "LeiosCertRejection" LeiosCertRejection

-- | Verify that the certificate establishes the claim, given the
-- predecessor's hash
--
-- The certificate must be signed by a quorum of the committee that was seated
-- when the predecessor announced the endorser block. The claim's view, however,
-- is the Leios part of the ledger view at the certifying block's slot, not at
-- the predecessor's slot. It nonetheless seats the right committee, because the
-- committee is fixed for a whole epoch and because a header that certifies an
-- announcement from an earlier epoch is invalid: so the certifying block and
-- its predecessor are in the same epoch, and therefore have the same committee.
verifyLeiosClaim ::
  LeiosCert ->
  LeiosClaim ->
  RankingBlockHash ->
  Except LeiosCertRejection ()
verifyLeiosClaim cert claim predecessor =
  void $
    withExcept LeiosCertInvalid $
      except $
        verifyLeiosCert
          (llvCommittee llv)
          (unboundRational (llvQuorumStakeThreshold llv))
          predecessor
          cert
 where
  llv = leiosClaimLedgerView claim
