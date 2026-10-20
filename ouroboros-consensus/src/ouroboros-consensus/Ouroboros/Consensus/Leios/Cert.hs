{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The claim a block's Leios certificate makes, and whether the certificate
-- establishes it.
module Ouroboros.Consensus.Leios.Cert
  ( LeiosClaim (..)
  , LeiosCertRejection (..)
  , leiosClaim
  , verifyLeiosClaim
  ) where

import Cardano.Crypto.Leios (LeiosCert, VerificationError, verifyLeiosCert)
import Cardano.Ledger.BaseTypes (unboundRational)
import Control.Monad.Except (Except, throwError, withExcept)
import Control.Monad.Trans.Except (except)
import Data.Functor (void)
import Data.Maybe.Strict (StrictMaybe (..))
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types (RbHash (..))

-- | What a block containing a certificate claims: that the endorser block its
-- predecessor announced is certified. The predecessor's hash identifies the
-- claim.
data LeiosClaim = LeiosClaim
  { leiosClaimSlot :: !SlotNo
  -- ^ The predecessor's slot
  , leiosClaimRbHash :: !RbHash
  -- ^ The predecessor's hash
  }
  deriving stock (Eq, Show)

-- | Why a block's Leios certificate is invalid
data LeiosCertRejection
  = -- | The block's predecessor is genesis, which announces nothing.
    LeiosCertAtGenesis
  | -- | The ledger view at the block's slot has no Leios committee.
    LeiosCertWithoutCommittee
  | LeiosCertInvalid !VerificationError
  deriving stock (Eq, Show)
  deriving NoThunks via InspectHeapNamed "LeiosCertRejection" LeiosCertRejection

-- | The claim of a block containing a certificate, given the block's
-- predecessor's hash and slot.
leiosClaim ::
  forall blk.
  ConvertRawHash blk =>
  ChainHash blk ->
  WithOrigin SlotNo ->
  Except LeiosCertRejection LeiosClaim
leiosClaim prevHash predecessorSlot = case (prevHash, predecessorSlot) of
  (BlockHash h, NotOrigin slot) ->
    pure $ LeiosClaim slot (MkRbHash (toRawHash (Proxy @blk) h))
  (GenesisHash, Origin) -> throwError LeiosCertAtGenesis
  -- The envelope check rules out these two, since a block's predecessor is
  -- genesis exactly when its slot is 'Origin'. Rejecting them as at genesis is
  -- harmless even so.
  (GenesisHash, NotOrigin _) -> throwError LeiosCertAtGenesis
  (BlockHash _, Origin) -> throwError LeiosCertAtGenesis

-- | Verify that the certificate establishes the claim
--
-- The certificate must be signed by a quorum of the committee that was seated
-- when the predecessor announced the endorser block. The given view, however,
-- is the Leios part of the ledger view at the certifying block's slot, not at
-- the predecessor's slot. It nonetheless seats the right committee, because the
-- committee is fixed for a whole epoch and because a header that certifies an
-- announcement from an earlier epoch is invalid: so the certifying block and
-- its predecessor are in the same epoch, and therefore have the same committee.
--
-- If the view is 'SNothing', then the certifying block's era has no Leios, and
-- so the certificate is invalid.
verifyLeiosClaim ::
  StrictMaybe LeiosLedgerView ->
  LeiosCert ->
  LeiosClaim ->
  Except LeiosCertRejection ()
verifyLeiosClaim mbView cert claim = case mbView of
  SNothing -> throwError LeiosCertWithoutCommittee
  SJust llv ->
    void $
      withExcept LeiosCertInvalid $
        except $
          verifyLeiosCert
            (llvCommittee llv)
            (unboundRational (llvQuorumStakeThreshold llv))
            (leiosClaimRbHash claim)
            cert
