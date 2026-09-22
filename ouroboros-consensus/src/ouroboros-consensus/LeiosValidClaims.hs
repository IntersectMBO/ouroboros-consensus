{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | The set of 'RbHash'es we have seen a valid Leios certificate for.
--
-- A CertRB claims that the endorser block announced by its predecessor is
-- certified; the claim is identified by the announcing block's 'RbHash', which
-- is the CertRB's prev-hash. Verifying a certificate is somewhat expensive, and
-- several CertRBs can make the same claim, so a claim is verified at most once.
--
-- Only /positive/ verdicts are recorded. A negative verdict is not cached:
-- there are unboundedly many invalid certificates that could make a given
-- claim, and another certificate making the same claim may well be valid.
module LeiosValidClaims
  ( ValidClaims
  , emptyValidClaims
  , insertValidClaim
  , memberValidClaim
  , pruneValidClaims
  , sizeValidClaims

    -- * Deciding a CertRB's claim
  , Announcing (..)
  , ClaimVerdict (..)
  , decideClaim
  ) where

import Cardano.Slotting.Slot (SlotNo)
import qualified Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict as Strict (Map)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import GHC.Generics (Generic)
import LeiosDemoTypes
  ( LeiosCert
  , LeiosCommittee
  , LeiosForecastRejection
    ( LeiosForecastAfterGenesis
    , LeiosForecastInvalidCertificate
    , LeiosForecastMissingCommittee
    )
  , RbHash
  , Weight
  , verifyLeiosCert
  )
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block (RealPoint (..), StandardHash)

-- | Claims known to be certified, indexed both by claim and by the slot of the
-- announcing block, the latter so that pruning is a range operation.
--
-- INVARIANT: the halves agree --- @slotOfClaim ! r == s@ iff @r@ occurs in
-- @claimsBySlot ! s@.
data ValidClaims
  = MkValidClaims
  { slotOfClaim :: !(Strict.Map RbHash SlotNo)
  , claimsBySlot :: !(Strict.Map SlotNo (NESet RbHash))
  }
  deriving stock (Show, Generic)

deriving via
  OnlyCheckWhnfNamed "ValidClaims" ValidClaims
  instance
    NoThunks ValidClaims

emptyValidClaims :: ValidClaims
emptyValidClaims = MkValidClaims Map.empty Map.empty

-- | Record that this claim is certified, as of the slot of the block that
-- announced the EB.
insertValidClaim :: SlotNo -> RbHash -> ValidClaims -> ValidClaims
insertValidClaim slot rbHash vc
  | Map.member rbHash (slotOfClaim vc) = vc
  | otherwise =
      MkValidClaims
        { slotOfClaim = Map.insert rbHash slot (slotOfClaim vc)
        , claimsBySlot =
            Map.insertWith (<>) slot (NESet.singleton rbHash) (claimsBySlot vc)
        }

memberValidClaim :: RbHash -> ValidClaims -> Bool
memberValidClaim rbHash = Map.member rbHash . slotOfClaim

-- | Forget every claim announced strictly before the given slot.
--
-- Called whenever the ChainDB's immutable tip advances: a claim announced below
-- the immutable tip can no longer be the subject of a block we might select.
-- The comparison is strict so that the immutable tip's own announcement
-- survives.
pruneValidClaims :: SlotNo -> ValidClaims -> ValidClaims
pruneValidClaims immTipSlot vc =
  MkValidClaims
    { slotOfClaim = Data.Foldable.foldl' dropSlot (slotOfClaim vc) pruned
    , claimsBySlot = claimsBySlot'
    }
 where
  (pruned, claimsBySlot') = Map.spanAntitone (< immTipSlot) (claimsBySlot vc)

  dropSlot fwd rs = Data.Foldable.foldl' (flip Map.delete) fwd rs

sizeValidClaims :: ValidClaims -> Int
sizeValidClaims = Map.size . slotOfClaim

{-------------------------------------------------------------------------------
  Deciding a CertRB's claim
-------------------------------------------------------------------------------}

-- | What the caller knows about the block that announced the EB a CertRB
-- certifies, which is that CertRB's predecessor.
data Announcing blk
  = -- | There is no announcing block: the CertRB's predecessor is genesis, so
    -- it would certify at genesis.
    AnnouncingAtGenesis
  | -- | The announcing block's slot, the claim its hash identifies, and the
    -- committee the ledger view seats at that slot --- 'Nothing' when the view
    -- has no committee or no threshold, which for a CertRB is itself a protocol
    -- violation rather than a gap in what we know.
    Announcing !(RealPoint blk) !RbHash !(Maybe (LeiosCommittee, Weight))

deriving stock instance StandardHash blk => Show (Announcing blk)

-- | What to do about a CertRB's claim.
data ClaimVerdict blk
  = -- | An equal claim is already recorded, so this certificate need not be
    -- checked at all --- not even if it is itself invalid.
    --
    -- Recall that this verdict is only for maintaining the set of
    -- 'ValidClaims'; every cert is fully validated before /being selected/, but
    -- different code does that.
    ClaimAlreadyEstablished
  | -- | The claim is established as of the given announcing slot, and belongs
    -- in 'ValidClaims'.
    ClaimEstablished !(RealPoint blk) !RbHash
  | ClaimRejected !LeiosForecastRejection

deriving stock instance StandardHash blk => Show (ClaimVerdict blk)

deriving stock instance StandardHash blk => Eq (ClaimVerdict blk)

-- | Decide a CertRB's claim: the whole of what ChainSel's cert precheck decides,
-- with none of what it reads or writes.
--
-- Verification is skipped when an equal claim is already recorded, which is
-- what makes the several /valid/ CertRBs making one claim still cost only one
-- full precheck.
decideClaim :: ValidClaims -> Announcing blk -> LeiosCert -> ClaimVerdict blk
decideClaim vc announcing cert = case announcing of
  AnnouncingAtGenesis -> ClaimRejected LeiosForecastAfterGenesis
  Announcing announcingPoint rbHash mbCommittee
    | memberValidClaim rbHash vc -> ClaimAlreadyEstablished
    | otherwise -> case mbCommittee of
        Nothing -> ClaimRejected $ LeiosForecastMissingCommittee rbHash
        Just (committee, threshold) ->
          case verifyLeiosCert committee threshold rbHash cert of
            Left invalid ->
              ClaimRejected $ LeiosForecastInvalidCertificate rbHash invalid
            Right _weight -> ClaimEstablished announcingPoint rbHash
