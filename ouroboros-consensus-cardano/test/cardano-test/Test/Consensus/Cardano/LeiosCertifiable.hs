{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests that the hard-fork combinator asks the era of the chain-dependent
-- state which announcement a block may certify.
module Test.Consensus.Cardano.LeiosCertifiable (tests) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes
  ( Milliseconds32 (..)
  , Nonce (NeutralNonce)
  , ProtVer (..)
  , StrictMaybe (SNothing)
  , boundRational
  , knownNonZeroBounded
  )
import Cardano.Ledger.Coin (knownNonZeroCoin)
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.State (PoolDistr (..), emptyLeiosCommittee)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo, hoistEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotInterval (..), SlotNo (..), WithOrigin (..))
import Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import Control.Monad.Except (Except)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.SOP (K (..), (:.:) (..))
import Data.SOP.Telescope (Telescope (..))
import qualified Data.Strict.Either as Strict
import Ouroboros.Consensus.Block (BlockProtocol)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.HardFork.Combinator (HardForkChainDepState, HardForkProtocol)
import Ouroboros.Consensus.HardFork.Combinator.Protocol (Ticked (..))
import Ouroboros.Consensus.HardFork.Combinator.State.Types
  ( Current (..)
  , HardForkState (..)
  , Past (..)
  )
import Ouroboros.Consensus.HardFork.History (PastHorizonException, initBound)
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  , NotCertifiable (..)
  , WhyNotCertifiable (..)
  )
import Ouroboros.Consensus.Leios.Types (minCertificationSlot)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Peras.Protocol (PerasLedgerView (..))
import Ouroboros.Consensus.Protocol.Abstract (ChainDepState, ConsensusConfig)
import Ouroboros.Consensus.Protocol.PolyPraos (PolyPraosState (..), Ticked (..))
import Ouroboros.Consensus.Protocol.Praos (LeiosOnly (..), PerasOnly (..), Praos)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.Praos2 (LeiosOnly (..), PerasOnly (..), Praos2)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.TypeFamilyWrappers (WrapChainDepState)
import Test.Consensus.Cardano.ProtocolInfo
  ( ByronSlotLengthInSeconds (..)
  , Era (Dijkstra)
  , ShelleySlotLengthInSeconds (..)
  , hardForkInto
  , mkSimpleTestProtocolInfo
  , protocolVersionZero
  )
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.ThreadNet.Infra.Shelley as Shelley

tests :: TestTree
tests =
  testGroup
    "leiosAnnouncementToCertify on the Cardano block"
    [ testCase
        "in Dijkstra, the announcement becomes certifiable once the certification gap elapses"
        unit_dijkstraCertifiesOnceTheGapElapses
    , testCase
        "in Conway, no announcement is certifiable"
        unit_conwayCertifiesNothing
    ]

-- | The last Dijkstra block, in 'announcingSlot', announced an endorser block.
--
-- * If the combinator asks an era without Leios, both checks fail.
-- * If it gives the era a wrong slot length, the earliest slot moves and one
--   check fails. The slots are two seconds long, so a slot length of one
--   second (the most likely wrong value) moves it.
unit_dijkstraCertifiesOnceTheGapElapses :: Assertion
unit_dijkstraCertifiesOnceTheGapElapses = do
  cfg <- cardanoConsensusConfig
  let certifiableAt slot =
        leiosAnnouncementToCertify cfg slot (inDijkstra announcingDijkstraState)
  certifiableAt earliest @?= Right announcement
  certifiableAt (pred earliest) @?= Left (TooYoung announcingSlot earliest)
 where
  earliest =
    minCertificationSlot
      slotLength
      (llvAnnouncementPeriod leiosLedgerView)
      (llvVotePeriod leiosLedgerView)
      (llvDiffusionPeriod leiosLedgerView)
      announcingSlot

-- | The chain is in Conway, which has no Leios.
--
-- If the combinator answers with the Dijkstra rule for an earlier era, this
-- fails.
unit_conwayCertifiesNothing :: Assertion
unit_conwayCertifiesNothing = do
  cfg <- cardanoConsensusConfig
  leiosAnnouncementToCertify cfg 1000 (inConway conwayState)
    @?= Left (NoCertifiableAnnouncement NothingAnnounced)

{-------------------------------------------------------------------------------
  The Cardano consensus config and chain-dependent states
-------------------------------------------------------------------------------}

cardanoConsensusConfig :: IO (ConsensusConfig (HardForkProtocol (CardanoEras StandardCrypto)))
cardanoConsensusConfig =
  topLevelConfigProtocol . pInfoConfig
    <$> mkSimpleTestProtocolInfo
      (Shelley.DecentralizationParam 1)
      (SecurityParam $ knownNonZeroBounded @10)
      (ByronSlotLengthInSeconds 2)
      (ShelleySlotLengthInSeconds 2)
      protocolVersionZero
      (hardForkInto Dijkstra)

-- | Put a ticked Dijkstra state at the Dijkstra position, after seven past
-- eras.
inDijkstra ::
  Ticked (PolyPraosState (Praos2 StandardCrypto)) ->
  Ticked (HardForkChainDepState (CardanoEras StandardCrypto))
inDijkstra st =
  TickedHardForkChainDepState
    ( HardForkState
        (TS past (TS past (TS past (TS past (TS past (TS past (TS past (TZ (current st)))))))))
    )
    epochInfo

-- | Put a ticked Conway state at the Conway position, after six past eras.
inConway ::
  Ticked (PolyPraosState (Praos StandardCrypto)) ->
  Ticked (HardForkChainDepState (CardanoEras StandardCrypto))
inConway st =
  TickedHardForkChainDepState
    (HardForkState (TS past (TS past (TS past (TS past (TS past (TS past (TZ (current st)))))))))
    epochInfo

past :: K Past blk
past = K (Past initBound initBound)

current ::
  Ticked (ChainDepState (BlockProtocol blk)) ->
  Current (Ticked :.: WrapChainDepState) blk
current st = Current initBound (Comp (WrapTickedChainDepState st))

-- | Every slot is 'slotLength' long.
epochInfo :: EpochInfo (Except PastHorizonException)
epochInfo = hoistEpochInfo (pure . runIdentity) (fixedEpochInfo (EpochSize 1000) slotLength)

slotLength :: SlotLength
slotLength = slotLengthFromSec 2

{-------------------------------------------------------------------------------
  The states
-------------------------------------------------------------------------------}

announcingSlot :: SlotNo
announcingSlot = 100

announcement :: LeiosAnnouncement
announcement =
  MkLeiosAnnouncement
    { announcementElectee = KeyHash (Hash.castHash (Hash.hashWith id "electee"))
    , announcementEbHash = Hash.castHash (Hash.hashWith id "endorser block")
    , announcementEbSize = 1000
    }

-- | The state after a block in 'announcingSlot' that announced 'announcement'.
announcingDijkstraState :: Ticked (PolyPraosState (Praos2 StandardCrypto))
announcingDijkstraState =
  TickedPraosState
    PraosState
      { praosStateLastSlot = At announcingSlot
      , praosStateOCertCounters = Map.empty
      , praosStateEvolvingNonce = NeutralNonce
      , praosStateCandidateNonce = NeutralNonce
      , praosStateEpochNonce = NeutralNonce
      , praosStatePreviousEpochNonce = NeutralNonce
      , praosStateLabNonce = NeutralNonce
      , praosStateLastEpochBlockNonce = NeutralNonce
      , praosStateLeiosAnnouncement = Praos2HasLeios (Strict.Right announcement)
      }
    Views.PraosLedgerView
      { Views.plvPoolDistr = PoolDistr Map.empty (knownNonZeroCoin @1)
      , Views.plvMaxHeaderSize = 0
      , Views.plvMaxBodySize = 0
      , Views.plvProtocolVersion = ProtVer minBound 0
      , Views.plvLeios = Praos2HasLeios leiosLedgerView
      , Views.plvPeras = Praos2HasPeras perasLedgerView
      }

conwayState :: Ticked (PolyPraosState (Praos StandardCrypto))
conwayState =
  TickedPraosState
    PraosState
      { praosStateLastSlot = At announcingSlot
      , praosStateOCertCounters = Map.empty
      , praosStateEvolvingNonce = NeutralNonce
      , praosStateCandidateNonce = NeutralNonce
      , praosStateEpochNonce = NeutralNonce
      , praosStatePreviousEpochNonce = NeutralNonce
      , praosStateLabNonce = NeutralNonce
      , praosStateLastEpochBlockNonce = NeutralNonce
      , praosStateLeiosAnnouncement = PraosLacksLeios ()
      }
    Views.PraosLedgerView
      { Views.plvPoolDistr = PoolDistr Map.empty (knownNonZeroCoin @1)
      , Views.plvMaxHeaderSize = 0
      , Views.plvMaxBodySize = 0
      , Views.plvProtocolVersion = ProtVer minBound 0
      , Views.plvLeios = PraosLacksLeios ()
      , Views.plvPeras = PraosLacksPeras ()
      }

-- | Only the three periods matter here.
leiosLedgerView :: LeiosLedgerView
leiosLedgerView =
  MkLeiosLedgerView
    { llvCommittee = emptyLeiosCommittee
    , llvQuorumStakeThreshold = maxBound
    , llvAnnouncementPeriod = Milliseconds32 1000
    , llvVotePeriod = Milliseconds32 4000
    , llvDiffusionPeriod = Milliseconds32 7000
    , llvMaxEbBodySize = 0
    , llvMaxEbTxsSize = 0
    }

perasLedgerView :: PerasLedgerView
perasLedgerView =
  MkPerasLedgerView
    { plvMinCandidateBlockAge = SlotInterval 90
    , plvHealingFactor = fromMaybe (error "impossible") (boundRational 1)
    , plvCertBoost = 15
    , plvTargetCommitteeSize = 900
    , plvBootstrapRound = SNothing
    , plvQuorumThresholdSafetyMargin = fromMaybe (error "impossible") (boundRational 0.02)
    }
