{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Protocol.Praos2 (tests) where

import Cardano.Ledger.BaseTypes
  ( BoundedRational (..)
  , Milliseconds32 (..)
  , ProtVer (..)
  , StrictMaybe (..)
  )
import Cardano.Ledger.Coin (knownNonZeroCoin)
import Cardano.Ledger.State (PoolDistr (..), emptyLeiosCommittee)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotInterval (..), SlotNo (..), WithOrigin (..))
import Cardano.Slotting.Time (slotLengthFromSec)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import qualified Data.Strict.Either as Strict
import Data.Word (Word64)
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement, LeiosLedgerView (..))
import Ouroboros.Consensus.Peras.Protocol (PerasLedgerView (..))
import Ouroboros.Consensus.Protocol.Abstract (TranslateProto (..))
import Ouroboros.Consensus.Protocol.PolyPraos
  ( PolyPraosState (..)
  , Ticked (..)
  , WhyNotCertifiable (..)
  , tickChainDepStatePolyPraos
  )
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.Praos2
  ( LeiosOnly (Praos2HasLeios)
  , PerasOnly (Praos2HasPeras)
  , Praos2
  )
import Test.Consensus.Protocol.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Praos2"
    [ testProperty
        "crossing into another era with Leios makes an announcement uncertifiable"
        prop_crossingEraMakesAnnouncementUncertifiable
    , testProperty
        "crossing into another epoch makes an announcement uncertifiable"
        prop_crossingEpochMakesAnnouncementUncertifiable
    , testProperty
        "crossing an era and an epoch in either order leaves the era's reason"
        prop_crossingEraAndEpochInEitherOrder
    , testProperty "WhyNotCertifiable's <> is associative" prop_whyNotCertifiableAssociative
    , testProperty "WhyNotCertifiable's <> is commutative" prop_whyNotCertifiableCommutative
    ]

-- | An endorser block may not be certified across an era boundary.
prop_crossingEraMakesAnnouncementUncertifiable ::
  PolyPraosState (Praos2 StandardCrypto) -> Property
prop_crossingEraMakesAnnouncementUncertifiable st =
  translateEra st === st{praosStateLeiosAnnouncement = Praos2HasLeios expected}
 where
  Praos2HasLeios before = praosStateLeiosAnnouncement st
  expected = afterEraBoundary before

-- | An endorser block may not be certified across an epoch boundary.
prop_crossingEpochMakesAnnouncementUncertifiable ::
  PolyPraosState (Praos2 StandardCrypto) -> Property
prop_crossingEpochMakesAnnouncementUncertifiable st =
  forAll (choose (1, 2 * epochSize)) $ \delta ->
    let slot = SlotNo $ lastSlot + delta
        crosses = epochOf (unSlotNo slot) > epochOf lastSlot
        expected
          | crosses = afterEpochBoundary before
          | otherwise = before
     in classify crosses "crosses an epoch boundary" $
          praosStateLeiosAnnouncement (tickTo slot st) === Praos2HasLeios expected
 where
  Praos2HasLeios before = praosStateLeiosAnnouncement st
  lastSlot = case praosStateLastSlot st of
    Origin -> 0
    At (SlotNo s) -> s

-- | The more specific reason survives, whichever boundary is crossed first.
prop_crossingEraAndEpochInEitherOrder ::
  PolyPraosState (Praos2 StandardCrypto) -> Property
prop_crossingEraAndEpochInEitherOrder st =
  praosStateLeiosAnnouncement (tickTo nextEpoch (translateEra st))
    === Praos2HasLeios expected
    .&&. praosStateLeiosAnnouncement (translateEra (tickTo nextEpoch st))
    === Praos2HasLeios expected
 where
  Praos2HasLeios before = praosStateLeiosAnnouncement st
  expected = afterEraBoundary before
  nextEpoch = SlotNo $ (epochOf lastSlot + 1) * epochSize
  lastSlot = case praosStateLastSlot st of
    Origin -> 0
    At (SlotNo s) -> s

prop_whyNotCertifiableAssociative ::
  WhyNotCertifiable -> WhyNotCertifiable -> WhyNotCertifiable -> Property
prop_whyNotCertifiableAssociative a b c = (a <> b) <> c === a <> (b <> c)

prop_whyNotCertifiableCommutative :: WhyNotCertifiable -> WhyNotCertifiable -> Property
prop_whyNotCertifiableCommutative a b = a <> b === b <> a

afterEraBoundary ::
  Strict.Either WhyNotCertifiable LeiosAnnouncement ->
  Strict.Either WhyNotCertifiable LeiosAnnouncement
afterEraBoundary = \case
  Strict.Left NothingAnnounced -> Strict.Left NothingAnnounced
  Strict.Left AnnouncedInPreviousEra -> Strict.Left AnnouncedInPreviousEra
  Strict.Left AnnouncedInPreviousEpoch -> Strict.Left AnnouncedInPreviousEra
  Strict.Right _ann -> Strict.Left AnnouncedInPreviousEra

afterEpochBoundary ::
  Strict.Either WhyNotCertifiable LeiosAnnouncement ->
  Strict.Either WhyNotCertifiable LeiosAnnouncement
afterEpochBoundary = \case
  Strict.Left NothingAnnounced -> Strict.Left NothingAnnounced
  Strict.Left AnnouncedInPreviousEra -> Strict.Left AnnouncedInPreviousEra
  Strict.Left AnnouncedInPreviousEpoch -> Strict.Left AnnouncedInPreviousEpoch
  Strict.Right _ann -> Strict.Left AnnouncedInPreviousEpoch

translateEra :: PolyPraosState (Praos2 StandardCrypto) -> PolyPraosState (Praos2 StandardCrypto)
translateEra =
  translateChainDepState (Proxy @(Praos2 StandardCrypto, Praos2 StandardCrypto, ()))

tickTo :: SlotNo -> PolyPraosState (Praos2 StandardCrypto) -> PolyPraosState (Praos2 StandardCrypto)
tickTo slot =
  tickedPraosStateChainDepState
    . tickChainDepStatePolyPraos
      (fixedEpochInfo (EpochSize epochSize) (slotLengthFromSec 1))
      ledgerView
      slot

epochSize :: Word64
epochSize = 5

epochOf :: Word64 -> Word64
epochOf s = s `div` epochSize

-- | The tick only carries it along.
ledgerView :: Views.PolyPraosLedgerView (Praos2 StandardCrypto)
ledgerView =
  Views.PraosLedgerView
    { Views.plvPoolDistr = PoolDistr Map.empty (knownNonZeroCoin @1)
    , Views.plvMaxHeaderSize = 0
    , Views.plvMaxBodySize = 0
    , Views.plvProtocolVersion = ProtVer minBound 0
    , Views.plvLeios =
        Praos2HasLeios
          MkLeiosLedgerView
            { llvCommittee = emptyLeiosCommittee
            , llvQuorumStakeThreshold = maxBound
            , llvAnnouncementPeriod = Milliseconds32 1000
            , llvVotePeriod = Milliseconds32 4000
            , llvDiffusionPeriod = Milliseconds32 1000
            , llvMaxEbBodySize = 0
            , llvMaxEbTxsSize = 0
            }
    , Views.plvPeras =
        Praos2HasPeras
          MkPerasLedgerView
            { plvMinCandidateBlockAge = SlotInterval 90
            , plvHealingFactor = fromMaybe (error "impossible") (boundRational 1)
            , plvCertBoost = 15
            , plvTargetCommitteeSize = 900
            , plvBootstrapRound = SNothing
            , plvQuorumThresholdSafetyMargin = fromMaybe (error "impossibe") (boundRational (2 % 100))
            }
    }
