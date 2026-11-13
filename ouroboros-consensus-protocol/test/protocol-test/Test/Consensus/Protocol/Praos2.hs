{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Protocol.Praos2 (tests) where

import Cardano.Ledger.BaseTypes
  ( BoundedRational (..)
  , Milliseconds32 (..)
  , Nonce (NeutralNonce)
  , ProtVer (..)
  , StrictMaybe (..)
  , natVersion
  )
import Cardano.Ledger.Coin (knownNonZeroCoin)
import Cardano.Ledger.State (PoolDistr (..), emptyLeiosCommittee)
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotInterval (..), SlotNo (..), WithOrigin (..))
import Cardano.Slotting.Time (slotLengthFromMillisec, slotLengthFromSec)
import Control.Monad.Except (runExcept)
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
  ( NotCertifiable (..)
  , PolyPraosState (..)
  , PolyPraosValidationErr (..)
  , Ticked (..)
  , WhyNotCertifiable (..)
  , certifiablePolyPraos
  , leiosHeaderChecks
  , tickChainDepStatePolyPraos
  )
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.Praos2
  ( LeiosOnly (Praos2HasLeios)
  , PerasOnly (Praos2HasPeras)
  , Praos2
  )
import Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary (genHeader)
import Test.Consensus.Protocol.Serialisation.Generators ()
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
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
    , testProperty
        "certifiablePolyPraos returns the parent's announcement only once the certification gap elapses"
        prop_certifiableReturnsAnnouncementOnceGapElapses
    , testProperty
        "leiosHeaderChecks rejects a header that contains a Leios certificate exactly when the parent's announcement is not certifiable"
        prop_headerChecksRejectLeiosCertWhenNotCertifiable
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

{-------------------------------------------------------------------------------
  Certifiability
-------------------------------------------------------------------------------}

-- | 'certifiablePolyPraos' agrees with CIP-0164's certification rule.
prop_certifiableReturnsAnnouncementOnceGapElapses :: Property
prop_certifiableReturnsAnnouncementOnceGapElapses =
  checkCoverage $
    forAll genCertifyCase $ \c ->
      coverCertifyCase c $
        runCertifiable c === expectedCertifiable c

-- | 'leiosHeaderChecks' rejects a header exactly when it contains a Leios
-- certificate that 'expectedCertifiable' forbids, and the error names the
-- reason and the slots.
prop_headerChecksRejectLeiosCertWhenNotCertifiable :: Property
prop_headerChecksRejectLeiosCertWhenNotCertifiable =
  checkCoverage $
    forAll genCertifyCase $ \c ->
      forAll arbitrary $ \containsCert ->
        cover 30 (not containsCert) "no certificate" $
          coverCertifyCase c $
            runCertificationCheck (headerView containsCert fixedHeader) c
              === expectedCertificationCheck containsCert c

expectedCertificationCheck ::
  Bool ->
  CertifyCase ->
  Either (PolyPraosValidationErr (Praos2 StandardCrypto) StandardCrypto) ()
expectedCertificationCheck containsCert c
  | not containsCert = Right ()
  | otherwise = case expectedCertifiable c of
      Right _ann -> Right ()
      Left (NotAnnounced why) ->
        Left $
          LeiosCertWithoutAnnouncement
            (Praos2HasLeios ())
            why
            (ccPredecessorSlot c)
            (ccSlot c)
      Left (TooYoung announcingSlot earliest) ->
        Left $
          LeiosCertTooYoung (Praos2HasLeios ()) announcingSlot (ccSlot c) earliest

-- | 'leiosHeaderChecks' reads only 'Views.hvLeios', which 'headerView' sets, so
-- one header serves every case. Generating and signing a header for each case
-- makes the property about 100 times slower.
fixedHeader :: LeiosCodec.Header StandardCrypto
fixedHeader = unGen (genHeader (natVersion @12)) (mkQCGen 0) 30

-- | Calls 'leiosHeaderChecks' on the header at the case's slot.
runCertificationCheck ::
  Views.PolyPraosValidateView (Praos2 StandardCrypto) StandardCrypto ->
  CertifyCase ->
  Either (PolyPraosValidationErr (Praos2 StandardCrypto) StandardCrypto) ()
runCertificationCheck view c =
  runExcept $
    leiosHeaderChecks (certifyEpochInfo c) view (ccSlot c) (certifyTickedState c)

-- | The view of a header that announces nothing and contains a Leios
-- certificate or not, as given. It is built as the Cardano block builds it
-- for 'Praos2'. 'leiosHeaderChecks' reads only 'Views.hvLeios', so the other
-- fields come from an arbitrary header.
headerView ::
  Bool ->
  LeiosCodec.Header StandardCrypto ->
  Views.PolyPraosValidateView (Praos2 StandardCrypto) StandardCrypto
headerView containsCert header =
  Views.HeaderView
    { Views.hvPrevHash = LeiosCodec.hbPrev body
    , Views.hvVK = LeiosCodec.hbVk body
    , Views.hvVrfVK = LeiosCodec.hbVrfVk body
    , Views.hvVrfRes = LeiosCodec.hbVrfRes body
    , Views.hvOCert = LeiosCodec.hbOCert body
    , Views.hvSlotNo = LeiosCodec.hbSlotNo body
    , Views.hvLeios = Praos2HasLeios (containsCert, SNothing)
    , Views.hvSigned = body
    , Views.hvSignature = LeiosCodec.headerSig header
    }
 where
  body = LeiosCodec.headerBody header

-- | The inputs of one test case: a new block at slot 'ccSlot', the slot of its
-- parent, what its parent announced, the slot length, and the three Leios
-- periods.
data CertifyCase = CertifyCase
  { ccPredecessorSlot :: WithOrigin SlotNo
  , ccAnnouncedByPredecessor :: Strict.Either WhyNotCertifiable LeiosAnnouncement
  , ccSlotLengthMs :: Integer
  , ccAnnouncementPeriodMs :: Integer
  -- ^ L_hdr
  , ccVotePeriodMs :: Integer
  -- ^ L_vote
  , ccDiffusionPeriodMs :: Integer
  -- ^ L_diff
  , ccSlot :: SlotNo
  }
  deriving Show

-- | The rule as CIP-0164 states it: a header may certify the endorser block its
-- predecessor announced if the header's slot starts at least
-- @3 * L_hdr + L_vote + L_diff@ after the predecessor's slot starts.
--
-- It compares start times in milliseconds. It does not reuse the rounding up
-- to whole slots that 'Leios.minCertificationSlot' does, so a wrong rounding
-- there makes the property fail.
expectedCertifiable :: CertifyCase -> Either NotCertifiable LeiosAnnouncement
expectedCertifiable c = case (ccAnnouncedByPredecessor c, ccPredecessorSlot c) of
  (_, Origin) -> Left $ NotAnnounced NothingAnnounced
  (Strict.Left why, At _) -> Left $ NotAnnounced why
  (Strict.Right ann, At predecessorSlot)
    | startsLateEnough c (ccSlot c) -> Right ann
    | otherwise -> Left $ TooYoung predecessorSlot (earliestCertifyingSlot c)

-- | Whether slot @s@ starts at least the gap after the predecessor's slot starts.
startsLateEnough :: CertifyCase -> SlotNo -> Bool
startsLateEnough c s =
  toInteger (unSlotNo s - unSlotNo (predecessorSlotNo c)) * ccSlotLengthMs c
    >= gapMs c

-- | The first slot that starts late enough, found by counting up from the
-- predecessor's slot.
earliestCertifyingSlot :: CertifyCase -> SlotNo
earliestCertifyingSlot c = until (startsLateEnough c) (+ 1) (predecessorSlotNo c)

gapMs :: CertifyCase -> Integer
gapMs c = 3 * ccAnnouncementPeriodMs c + ccVotePeriodMs c + ccDiffusionPeriodMs c

predecessorSlotNo :: CertifyCase -> SlotNo
predecessorSlotNo c = case ccPredecessorSlot c of
  Origin -> 0
  At s -> s

-- | Calls 'certifiablePolyPraos' on the case.
runCertifiable :: CertifyCase -> Either NotCertifiable LeiosAnnouncement
runCertifiable c = result
 where
  Praos2HasLeios result =
    certifiablePolyPraos (certifyEpochInfo c) (ccSlot c) (certifyTickedState c)

certifyEpochInfo :: Monad m => CertifyCase -> EpochInfo m
certifyEpochInfo c =
  fixedEpochInfo (EpochSize 1000000) (slotLengthFromMillisec (ccSlotLengthMs c))

-- | The predecessor's state ticked to 'ccSlot', with the case's periods in the
-- ledger view.
certifyTickedState :: CertifyCase -> Ticked (PolyPraosState (Praos2 StandardCrypto))
certifyTickedState c =
  TickedPraosState
    { tickedPraosStateChainDepState =
        predecessorState (ccPredecessorSlot c) (ccAnnouncedByPredecessor c)
    , tickedPraosStateLedgerView =
        ledgerView{Views.plvLeios = withPeriods <$> Views.plvLeios ledgerView}
    }
 where
  withPeriods llv =
    llv
      { llvAnnouncementPeriod = Milliseconds32 (fromInteger (ccAnnouncementPeriodMs c))
      , llvVotePeriod = Milliseconds32 (fromInteger (ccVotePeriodMs c))
      , llvDiffusionPeriod = Milliseconds32 (fromInteger (ccDiffusionPeriodMs c))
      }

-- | The rule reads only the last slot and the announcement. The other fields
-- are fixed.
predecessorState ::
  WithOrigin SlotNo ->
  Strict.Either WhyNotCertifiable LeiosAnnouncement ->
  PolyPraosState (Praos2 StandardCrypto)
predecessorState lastSlot announcement =
  PraosState
    { praosStateLastSlot = lastSlot
    , praosStateOCertCounters = Map.empty
    , praosStateEvolvingNonce = NeutralNonce
    , praosStateCandidateNonce = NeutralNonce
    , praosStateEpochNonce = NeutralNonce
    , praosStatePreviousEpochNonce = NeutralNonce
    , praosStateLabNonce = NeutralNonce
    , praosStateLastEpochBlockNonce = NeutralNonce
    , praosStateLeiosAnnouncement = Praos2HasLeios announcement
    }

genCertifyCase :: Gen CertifyCase
genCertifyCase = do
  predecessorSlot <-
    frequency [(1, pure Origin), (9, At . SlotNo <$> choose (0, 1000))]
  announced <-
    frequency [(1, Strict.Left <$> arbitrary), (3, Strict.Right <$> arbitrary)]
  slotLengthMs <- choose (1, 3000)
  let wholeSlots = (* slotLengthMs) <$> choose (0, 5)
      anyMs = choose (0, 10000)
  -- The certification gap is 3 * L_hdr + L_vote + L_diff. Generate one of
  -- three kinds: a gap of 0 ms, a gap of a whole number of slots, or a gap that
  -- rounds up to the next slot.
  (hdrMs, voteMs, diffMs) <-
    frequency
      [ (1, pure (0, 0, 0))
      , (3, (,,) <$> wholeSlots <*> wholeSlots <*> wholeSlots)
      , (6, (,,) <$> anyMs <*> anyMs <*> anyMs)
      ]
  let withoutSlot =
        CertifyCase
          { ccPredecessorSlot = predecessorSlot
          , ccAnnouncedByPredecessor = announced
          , ccSlotLengthMs = slotLengthMs
          , ccAnnouncementPeriodMs = hdrMs
          , ccVotePeriodMs = voteMs
          , ccDiffusionPeriodMs = diffMs
          , ccSlot = 0
          }
      earliest = earliestCertifyingSlot withoutSlot
      -- A header's slot is always after its predecessor's.
      afterPredecessor = max (predecessorSlotNo withoutSlot + 1)
  -- Mostly next to the earliest certifying slot, where an off-by-one shows.
  slot <-
    afterPredecessor
      <$> oneof
        [ pure (earliest - 1)
        , pure earliest
        , pure (earliest + 1)
        , (earliest +) . SlotNo <$> choose (0, 10)
        ]
  pure withoutSlot{ccSlot = slot}

-- | Fails the property unless each kind of case is generated often enough.
coverCertifyCase :: CertifyCase -> Property -> Property
coverCertifyCase c =
  cover 1 (ccPredecessorSlot c == Origin) "predecessor at Origin"
    . cover 5 (notAnnounced && ccPredecessorSlot c /= Origin) "predecessor announced nothing certifiable"
    . cover 10 (not notAnnounced && ccSlot c < earliest) "before the earliest slot"
    . cover 10 (not notAnnounced && ccSlot c == earliest) "at the earliest slot"
    . cover 10 (not notAnnounced && ccSlot c > earliest) "after the earliest slot"
    . cover 5 (gapMs c == 0) "gap of 0 ms"
    . cover 10 (gapMs c > 0 && gapMs c `mod` ccSlotLengthMs c == 0) "gap of whole slots"
    . cover 10 (gapMs c `mod` ccSlotLengthMs c /= 0) "gap that rounds up"
 where
  earliest = earliestCertifyingSlot c
  notAnnounced = case ccAnnouncedByPredecessor c of
    Strict.Left{} -> True
    Strict.Right{} -> False
