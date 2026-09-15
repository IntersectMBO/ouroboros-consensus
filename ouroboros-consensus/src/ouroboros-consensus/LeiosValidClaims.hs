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
  ) where

import Cardano.Slotting.Slot (SlotNo)
import qualified Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict as Strict (Map)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import GHC.Generics (Generic)
import LeiosDemoTypes (RbHash)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

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
