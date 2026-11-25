{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests that the node writes each endorser block that it forges to the
-- LeiosDB, and acquires and relays it as it would one fetched from a peer, and
-- that 'forge' puts a Leios certificate in a block exactly when it may, and
-- that the node certifies only an endorser block whose closure it holds.
module Test.Consensus.Leios.Forge (tests) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.BaseTypes (unboundRational)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (Tracer (..), emit, nullTracer)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (for_)
import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Traversable (for)
import Data.Word (Word16)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..), SystemTime (..))
import Ouroboros.Consensus.Config (configLedger)
import Ouroboros.Consensus.Ledger.Abstract (GetTip (..))
import Ouroboros.Consensus.Leios.Acquisitions
  ( LeiosAcquisition (..)
  , LeiosAcquisitions (..)
  )
import Ouroboros.Consensus.Leios.AnnounceState (onValidAnnouncement)
import Ouroboros.Consensus.Leios.NotifyLog (Notification (..), nextNotifyLog, notifyLogStart)
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  , NotCertifiable (..)
  , WhyNotCertifiable (..)
  )
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Leios.VoteState (addVote, getCertificate)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
  , validHeaderAnnouncement
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( AcceptedVote (..)
  , VoteWindow (..)
  , leiosVoteWindow
  )
import Ouroboros.Consensus.Node.Tracers
  ( TraceForgeEvent (..)
  , TraceLabelCreds (..)
  , Tracers' (..)
  , nullTracers
  )
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.NodeKernel.Forge (forge)
import Ouroboros.Consensus.Storage.ChainDB.API (getCurrentChain)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , withReader
  , withWriter
  )
import Ouroboros.Consensus.Util.EarlyExit (withEarlyExit_)
import Ouroboros.Consensus.Util.IOLike
  ( atomically
  , modifyTVar
  , newTVarIO
  , orElse
  , readTVar
  , readTVarIO
  , retry
  , uncheckedNewTVarM
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.Consensus.Leios.Committee (certOf, voteOf)
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestBlock (TestHash (..))

tests :: TestTree
tests =
  testGroup
    "Forging"
    [ testCase
        "the node stores, acquires and relays each endorser block that it forges, at the point that the header announces"
        unit_storesForgedEbs
    , testProperty
        "the block contains a Leios certificate exactly when its predecessor announces and the node has a certificate"
        prop_certifiesExactlyWhenAllowed
    , testProperty
        "in each forge, the node certifies the endorser block that its tip announces exactly when it has the certificate and the closure"
        prop_certifiesOnlyWithClosure
    ]

-- | 'forge' runs in the slot after the tip of a generated chain, with a
-- certificate callback that gives a certificate or not.
--
-- * If 'forge' asks for a certificate when the predecessor announces nothing,
--   or for an announcement other than the predecessor's, the first check
--   fails. For example, a forger that takes the latest announcement on the
--   chain fails it when only an older block announces.
-- * If the block does not contain the certificate that the callback gives, or
--   contains one that the callback did not give, the second check fails.
-- * If the traces do not say what 'forge' decided, the third or the fourth
--   check fails.
prop_certifiesExactlyWhenAllowed :: Scenario -> Property
prop_certifiesExactlyWhenAllowed scenario@Scenario{nodeHasCert} =
  checkCoverage
    . cover 15 (predecessorAnnounces && nodeHasCert) "the block certifies"
    . cover 15 (predecessorAnnounces && not nodeHasCert) "the node has no certificate"
    . cover 10 onlyAnOlderBlockAnnounces "only an older block announces"
    . cover 5 (null chain) "the predecessor is genesis"
    $ conjoin
      [ counterexample "forge asks for the wrong certificates" $
          certRequests outcome === maybeToList predecessorAnnouncement
      , counterexample "the block contains the wrong certificate" $
          certInBlock outcome === expectedCert
      , counterexample "forge traces the wrong certifying block" $
          certifyingBlockTraces outcome
            === [ (point, announcementEbHash announcement)
                | Just (point, announcement) <- [predecessorAnnouncement]
                , nodeHasCert
                ]
      , counterexample "forge traces the wrong reason for not certifying" $
          notCertifiableTraces outcome
            === [NoCertifiableAnnouncement NothingAnnounced | not predecessorAnnounces]
      ]
 where
  chain = chainOf (announces scenario)
  outcome = runSimOrThrow $ forgeOnTopOf chain (certificateCallback nodeHasCert)

  -- The point and the announcement of the block that the forged block
  -- extends, if that block announces.
  predecessorAnnouncement = case reverse chain of
    [] -> Nothing
    predecessor : _ ->
      (\announcement -> (blockRealPoint predecessor, announcement))
        <$> getLeiosAnnouncement (Proxy @LeiosTestProtocol) (getHeader predecessor)

  predecessorAnnounces = isJust predecessorAnnouncement

  onlyAnOlderBlockAnnounces = not predecessorAnnounces && or (announces scenario)

  expectedCert
    | nodeHasCert = certificateFor . fst <$> predecessorAnnouncement
    | otherwise = Nothing

-- | The chain that the node holds, and whether the node has a certificate.
data Scenario = Scenario
  { announces :: [Bool]
  -- ^ For each block of the chain, oldest first, whether it announces an
  -- endorser block.
  , nodeHasCert :: Bool
  -- ^ Whether the certificate callback gives a certificate.
  }
  deriving Show

instance Arbitrary Scenario where
  arbitrary = do
    chainLength <- choose (0, 6)
    Scenario <$> vectorOf chainLength arbitrary <*> arbitrary
  shrink Scenario{announces, nodeHasCert} =
    [ Scenario announces' nodeHasCert'
    | (announces', nodeHasCert') <- shrink (announces, nodeHasCert)
    ]

-- | A chain whose blocks announce an endorser block as the list says, oldest
-- first. Its first block is in slot 1, and each next block is in the next
-- slot.
chainOf :: [Bool] -> [Blk]
chainOf announcements =
  zipWith announceIf announcements (iterate successorLeiosBlock (firstLeiosBlock 1))
 where
  announceIf False blk = blk
  announceIf True blk = announce blk

-- | Make the block announce 'endorserBlockIn' its slot.
announce :: Blk -> Blk
announce blk = setAnnouncement (endorserBlockHash eb) (encodeEndorserBlockSize eb) blk
 where
  eb = endorserBlockIn (blockSlot blk)

-- | The endorser block that a block of 'chainOf' in this slot announces.
endorserBlockIn :: SlotNo -> EndorserBlock
endorserBlockIn slot = forgedEbBody (forgeLeiosEb minBound (txInSlot slot :| []))

-- | The callback gives the certificate of the announcing block, if the node has
-- certificates at all.
certificateCallback :: Bool -> RealPoint Blk -> LeiosAnnouncement -> IOSim s (Maybe LeiosCert)
certificateCallback nodeHasCert point _announcement
  | nodeHasCert = pure $ Just $ certificateFor point
  | otherwise = pure Nothing

-- | A certificate that the whole test committee makes for the block at this
-- point.
certificateFor :: RealPoint Blk -> LeiosCert
certificateFor point =
  certOf (toRankingBlockHash (Proxy @Blk) (realPointHash point)) [0, 1, 2]

-- | What 'forge' did.
data CertOutcome = CertOutcome
  { certRequests :: [(RealPoint Blk, LeiosAnnouncement)]
  -- ^ The arguments of each call of the certificate callback, oldest first.
  , certInBlock :: Maybe LeiosCert
  -- ^ The certificate in the forged block.
  , certifyingBlockTraces :: [(RealPoint Blk, EndorserBlockHash)]
  -- ^ The announcing block and the certified endorser block of each
  -- 'TraceForgedCertifyingBlock'.
  , notCertifiableTraces :: [NotCertifiable]
  -- ^ The reason of each 'TraceForgeNotCertifiable'.
  }

-- | Add the chain to the node, then run 'forge' with 'certForging' in the slot
-- after the chain's tip.
forgeOnTopOf ::
  [Blk] ->
  (RealPoint Blk -> LeiosAnnouncement -> IOSim s (Maybe LeiosCert)) ->
  IOSim s CertOutcome
forgeOnTopOf chain callback = withNode $ \node -> do
  addBlocks node chain
  requests <- newTVarIO []
  events <- uncheckedNewTVarM []
  let recordEvent (TraceLabelCreds _ event) = atomically $ modifyTVar events (event :)
      recordingCallback point announcement = do
        atomically $ modifyTVar requests ((point, announcement) :)
        callback point announcement
  withEarlyExit_ $
    forge
      (Tracer (emit recordEvent))
      nullTracer
      (nodeTopLevelConfig node)
      (nodeChainDB node)
      (getMempool (nodeKernel node))
      certForging
      recordingCallback
      (\_ _ _ -> pure ())
      slotAfterTip
  forgeEvents <- reverse <$> readTVarIO events
  certRequests <- reverse <$> readTVarIO requests
  pure
    CertOutcome
      { certRequests
      , certInBlock =
          case [blk | TraceForgedBlock _ _ blk _ _ <- forgeEvents] of
            [blk] -> blockLeiosCert blk
            blks -> error $ "forge forged " <> show (length blks) <> " blocks"
      , certifyingBlockTraces =
          [(point, ebHash) | TraceForgedCertifyingBlock _ _ point ebHash <- forgeEvents]
      , notCertifiableTraces =
          [reason | TraceForgeNotCertifiable _ reason <- forgeEvents]
      }
 where
  slotAfterTip = case reverse chain of
    [] -> 1
    tip : _ -> succ (blockSlot tip)

-- | A forging that leads in every slot. Each block extends the ledger tip and
-- contains the certificate that 'forge' gives it, if any. It announces no
-- endorser block.
certForging :: BlockForging (IOSim s) LeiosTestBlock
certForging =
  BlockForging
    { forgeLabel = "certForging"
    , canBeLeader = []
    , updateForgeState = \_ _ _ -> pure $ ForgeStateUpdated ()
    , checkCanForge = \_ _ _ _ _ -> Right ()
    , forgeBlock = \ForgeBlockArgs{fbCurrentSlotNo, fbCurrentTickedLedgerState, fbLeiosCert} -> do
        let tip = castPoint (getTip fbCurrentTickedLedgerState)
        pure
          ForgeBlockResult
            { rankingBlock =
                maybe id setLeiosCert fbLeiosCert $
                  setSlot fbCurrentSlotNo $
                    blockAfter tip
            , endorserBlock = Nothing
            }
    , finalize = pure ()
    }

-- | The node has a chain. Then it forges once in each slot after the tip, as
-- a generated plan says. Before each forge, the certificate of the tip's
-- announcement, or the closure of the tip's endorser block, or both, can
-- arrive at the node. Each forged block is valid or invalid, and announces an
-- endorser block or not. ChainSel rejects an invalid block, so the next forge
-- is on the same tip again. So the closure can arrive between two forges on
-- one tip.
--
-- For each forge, the test compares what the node has and what it forges
-- with 'expectedObservation':
--
-- * If the test does not deliver the certificate or the closure to the node,
--   what the node has differs from the plan. Then the fault is in the test.
-- * If 'getLeiosCertToForge' gives a certificate whose closure the node does
--   not have, the block contains a certificate that the plan does not.
-- * If the node does not see a closure that arrives after a forge on the same
--   tip, the block lacks the certificate that the plan has.
-- * If the node does not trace the closure or the certificate that it lacks,
--   the traces differ from the plan.
prop_certifiesOnlyWithClosure :: ForgeScenario -> Property
prop_certifiesOnlyWithClosure (ForgeScenario chainAnnouncements steps) =
  checkCoverage
    . cover 5 (anyForgeHas CertificateOnly) "a forge has the certificate but not the closure"
    . cover 5 (anyForgeHas ClosureOnly) "a forge has the closure but not the certificate"
    . cover 5 (anyForgeHas Both) "a forge certifies"
    . cover 5 (any hasNeitherOnAnnouncingTip forges) "a forge on an announcing tip has neither"
    . cover
      5
      (closureArrivesBetweenForgesOnOneTip forges)
      "the closure arrives between two forges on one tip"
    $ runSimOrThrow (runForges chain forges) === map expectedObservation forges
 where
  chain = chainOf chainAnnouncements
  forges = planForges chain steps
  anyForgeHas arrived = any ((== arrived) . forgeArrived) forges

-- | Whether each block of the initial chain announces an endorser block, and
-- the steps of the plan, one per forge.
data ForgeScenario = ForgeScenario [Bool] [Step]
  deriving Show

instance Arbitrary ForgeScenario where
  arbitrary = do
    chainLength <- choose (0, 6)
    -- Two forges at least, so that the closure can arrive between them. Five
    -- at most, so that each forge comes before the node drops the votes for
    -- the tip, 8 slots after the tip.
    stepCount <- choose (2, 5)
    ForgeScenario
      <$> vectorOf chainLength (frequency [(3, pure True), (1, pure False)])
      <*> vectorOf stepCount arbitrary
  shrink (ForgeScenario chainAnnouncements steps) =
    [ ForgeScenario chainAnnouncements' steps
    | chainAnnouncements' <- shrinkList (const []) chainAnnouncements
    ]
      <> [ ForgeScenario chainAnnouncements steps'
         | steps' <- shrinkList (const []) steps
         , not (null steps')
         ]

-- | One forge of the plan.
data Step = Step
  { stepArrival :: Arrival
  -- ^ What arrives at the node just before the forge.
  , stepValidity :: BlockValidity
  , stepAnnounces :: Announces
  }
  deriving Show

instance Arbitrary Step where
  arbitrary =
    Step
      <$> elements [Neither, CertificateOnly, ClosureOnly, Both]
      <*> elements [ValidBlock, InvalidBlock]
      <*> frequency [(3, pure Announces), (1, pure AnnouncesNothing)]

-- | What arrives at the node, or what arrived so far, for the announcement of
-- its tip: the certificate, the closure of the announced endorser block, or
-- both.
data Arrival = Neither | CertificateOnly | ClosureOnly | Both
  deriving (Eq, Show)

-- | Everything that arrived, once more arrives.
andThen :: Arrival -> Arrival -> Arrival
andThen Neither later = later
andThen earlier Neither = earlier
andThen earlier later
  | earlier == later = earlier
  | otherwise = Both

hasCertificate, hasClosure :: Arrival -> Bool
hasCertificate = (`elem` [CertificateOnly, Both])
hasClosure = (`elem` [ClosureOnly, Both])

-- | Whether ChainSel accepts the forged block.
data BlockValidity = ValidBlock | InvalidBlock
  deriving Show

-- | Whether the forged block announces an endorser block.
data Announces = Announces | AnnouncesNothing
  deriving Show

-- | A step of the plan, with what the test knows about the forge.
data PlannedForge = PlannedForge
  { forgeSlot :: SlotNo
  , forgeTip :: Maybe Blk
  -- ^ The block that the node forges on, or 'Nothing' for genesis.
  , forgeArrivedBefore :: Arrival
  -- ^ What arrived for the announcement of the tip, before this step.
  , forgeArrived :: Arrival
  -- ^ What arrived for the announcement of the tip, at the forge.
  , forgeStep :: Step
  }

-- | The forges of the steps, one per slot from the slot after the tip. Only
-- for a tip that announces does anything arrive. A valid block becomes the
-- tip, and the node has nothing for its announcement yet. After an invalid
-- block, the tip and what the node has stay.
planForges :: [Blk] -> [Step] -> [PlannedForge]
planForges chain = go (slotAfter chain) (listToMaybe (reverse chain)) Neither
 where
  go _ _ _ [] = []
  go slot tip before (step : steps) = planned : go (succ slot) nextTip nextArrived steps
   where
    arrived
      | isJust (tip >>= announcementOf) = before `andThen` stepArrival step
      | otherwise = before
    planned =
      PlannedForge
        { forgeSlot = slot
        , forgeTip = tip
        , forgeArrivedBefore = before
        , forgeArrived = arrived
        , forgeStep = step
        }
    (nextTip, nextArrived) = case stepValidity step of
      ValidBlock -> (Just (expectedBlock planned), Neither)
      InvalidBlock -> (tip, arrived)

-- | What arrived at the node just before a forge, and what the forge did.
data ForgeObservation = ForgeObservation
  { observedArrived :: Arrival
  , observedForge :: ForgeSummary
  }
  deriving (Eq, Show)

-- | What the test must observe in the forge.
expectedObservation :: PlannedForge -> ForgeObservation
expectedObservation planned =
  ForgeObservation
    { observedArrived = arrived
    , observedForge =
        ForgeSummary
          { summaryForgedOn = [(forgeSlot planned, tipPoint planned)]
          , summaryCert = expectedCertificate planned
          , summaryClosuresNotAcquired =
              [point | Just point <- [tipAnnouncement planned], not (hasClosure arrived)]
          , summaryCertificatesNotHeld =
              [blockRealPoint tip | Just tip <- [forgeTip planned], arrived == ClosureOnly]
          , summaryEnds = case stepValidity (forgeStep planned) of
              ValidBlock -> [Adopted]
              InvalidBlock -> [Rejected]
          }
    }
 where
  arrived = forgeArrived planned

-- | The certificate in the forged block: the node has both the certificate
-- and the closure of the tip's announcement.
expectedCertificate :: PlannedForge -> Maybe LeiosCert
expectedCertificate planned =
  listToMaybe
    [ certOf (rbHashOf tip) certificateSeats
    | Just tip <- [forgeTip planned]
    , forgeArrived planned == Both
    ]

-- | The block that the node forges.
expectedBlock :: PlannedForge -> Blk
expectedBlock planned =
  maybe id setLeiosCert (expectedCertificate planned) $ plannedBlock (tipPoint planned) planned

hasNeitherOnAnnouncingTip :: PlannedForge -> Bool
hasNeitherOnAnnouncingTip planned =
  isJust (tipAnnouncement planned) && forgeArrived planned == Neither

-- | A forge lacks the closure, and the next forge, on the same tip, certifies.
closureArrivesBetweenForgesOnOneTip :: [PlannedForge] -> Bool
closureArrivesBetweenForgesOnOneTip forges =
  or
    [ not (hasClosure (forgeArrived earlier))
        && isJust (expectedCertificate later)
        && forgeTip earlier == forgeTip later
    | (earlier, later) <- zip forges (drop 1 forges)
    ]

tipPoint :: PlannedForge -> Point Blk
tipPoint = maybe GenesisPoint blockPoint . forgeTip

tipAnnouncement :: PlannedForge -> Maybe LeiosPoint
tipAnnouncement planned = forgeTip planned >>= announcementOf

-- | Add the chain to the node. Then, for each forge, deliver to the node what
-- arrives before the forge, read what the node has, and wait until the node
-- forges in the slot of the forge.
runForges :: [Blk] -> [PlannedForge] -> IOSim s [ForgeObservation]
runForges chain forges = do
  events <- uncheckedNewTVarM []
  let recordEvent (TraceLabelCreds _ event) = atomically $ modifyTVar events (event :)
      recordedEvents = reverse <$> readTVar events
      -- Wait until ChainSel adopts, rejects or ignores the block of this slot.
      waitForForgeIn slot = atomically $ do
        recorded <- recordedEvents
        when (null (summaryEnds (forgeSummary slot recorded))) retry
  withNodeTracing nullTracers{forgeTracer = Tracer (emit recordEvent)} $ \node -> do
    let startForging = setBlockForging (nodeKernel node) [MkBlockForging (pure (plannedForging forges))]
    addBlocks node chain
    -- The node forges at once in the current slot. Half way into the slot
    -- after the tip, the node forges on the tip, away from the slot change.
    waitUntil (fromIntegral (unSlotNo (slotAfter chain)) + 0.5)
    arrivedAtEachForge <- for forges $ \planned -> do
      deliverArrival node planned
      arrived <- arrivedAt node (forgeTip planned)
      when (forgeSlot planned == slotAfter chain) startForging
      waitForForgeIn (forgeSlot planned)
      pure arrived
    recorded <- atomically recordedEvents
    pure
      [ ForgeObservation arrived (forgeSummary (forgeSlot planned) recorded)
      | (arrived, planned) <- zip arrivedAtEachForge forges
      ]

-- | Deliver to the node what arrives in this step and did not arrive before.
deliverArrival :: NodeUnderTest (IOSim s) -> PlannedForge -> IOSim s ()
deliverArrival node planned = for_ (forgeTip planned) $ \tip -> do
  when (arrivesNow hasCertificate) $ giveCertificate node tip
  when (arrivesNow hasClosure) $ acquireClosure node tip
 where
  arrivesNow has = not (has (forgeArrivedBefore planned)) && has (forgeArrived planned)

-- | What the node has for the announcement of the block.
arrivedAt :: NodeUnderTest (IOSim s) -> Maybe Blk -> IOSim s Arrival
arrivedAt node tip = atomically $ case tip of
  Just blk | Just point <- announcementOf blk -> do
    certified <- isJust <$> getCertificate (getLeiosVoteState (nodeKernel node)) (rbHashOf blk)
    acquired <- ($ point) <$> ChainDB.getIsLeiosClosureAcquired (nodeChainDB node)
    pure $ case (certified, acquired) of
      (False, False) -> Neither
      (True, False) -> CertificateOnly
      (False, True) -> ClosureOnly
      (True, True) -> Both
  _ -> pure Neither

-- | Forges the block of each planned forge, with the certificate that 'forge'
-- gives it. After the plan, it forges as 'certForging' does.
plannedForging :: [PlannedForge] -> BlockForging (IOSim s) LeiosTestBlock
plannedForging forges =
  certForging
    { forgeLabel = "plannedForging"
    , forgeBlock = \args@ForgeBlockArgs{fbCurrentSlotNo, fbCurrentTickedLedgerState, fbLeiosCert} ->
        case find ((== fbCurrentSlotNo) . forgeSlot) forges of
          Nothing -> forgeBlock certForging args
          Just planned ->
            pure
              ForgeBlockResult
                { rankingBlock =
                    maybe id setLeiosCert fbLeiosCert $
                      plannedBlock (castPoint (getTip fbCurrentTickedLedgerState)) planned
                , -- So the node does not acquire the closure itself.
                  endorserBlock = Nothing
                }
    }

-- | The block that the forge plans on top of this point, without a
-- certificate. An invalid block is a sibling of the valid one: ChainSel
-- ignores a block whose hash it knows as invalid, so a later valid block on
-- the same tip needs another hash.
plannedBlock :: Point Blk -> PlannedForge -> Blk
plannedBlock onTopOf PlannedForge{forgeSlot, forgeStep = Step{stepValidity, stepAnnounces}} =
  validity . announcement . setSlot forgeSlot $ blockAfter onTopOf
 where
  announcement = case stepAnnounces of
    Announces -> announce
    AnnouncesNothing -> id
  validity = case stepValidity of
    ValidBlock -> id
    InvalidBlock -> setInvalid . sibling
  sibling blk = LeiosTestBlock (ltbHeader blk){lthHash = TestHash (succ h :| hs)}
   where
    TestHash (h :| hs) = blockHash blk

-- | Wait until this time, in seconds since the start of the simulation.
waitUntil :: DiffTime -> IOSim s ()
waitUntil time = do
  Time now <- getMonotonicTime
  threadDelay (time - now)

-- | What one run of 'forge' did.
data ForgeSummary = ForgeSummary
  { summaryForgedOn :: [(SlotNo, Point Blk)]
  -- ^ The slot of each block that 'forge' forged, and the block it extends.
  , summaryCert :: Maybe LeiosCert
  -- ^ The certificate in the forged block.
  , summaryClosuresNotAcquired :: [LeiosPoint]
  -- ^ The point of each 'TraceForgeLeiosClosureNotAcquired'.
  , summaryCertificatesNotHeld :: [RealPoint Blk]
  -- ^ The point of each 'TraceForgeNoLeiosCert'.
  , summaryEnds :: [ForgeEnd]
  -- ^ What ChainSel did with each forged block.
  }
  deriving (Eq, Show)

-- | What ChainSel did with a forged block.
data ForgeEnd = Adopted | Rejected | NotAdopted
  deriving (Eq, Show)

-- | What the forge of this slot did, from the events of all forges.
forgeSummary :: SlotNo -> [TraceForgeEvent Blk] -> ForgeSummary
forgeSummary slot recorded =
  ForgeSummary
    { summaryForgedOn = [(forgeSlot, onTopOf) | TraceForgedBlock forgeSlot onTopOf _ _ _ <- events]
    , summaryCert = case [blk | TraceForgedBlock _ _ blk _ _ <- events] of
        [blk] -> blockLeiosCert blk
        _ -> Nothing
    , summaryClosuresNotAcquired = [point | TraceForgeLeiosClosureNotAcquired point <- events]
    , summaryCertificatesNotHeld = [point | TraceForgeNoLeiosCert point <- events]
    , summaryEnds = concatMap endOf events
    }
 where
  events = eventsOfSlot slot recorded
  endOf = \case
    TraceAdoptedBlock{} -> [Adopted]
    TraceForgedInvalidBlock{} -> [Rejected]
    TraceDidntAdoptBlock{} -> [NotAdopted]
    _ -> []

-- | The events of the forge of this slot. 'forge' runs for one slot after
-- the other, and each run starts with 'TraceStartLeadershipCheck'.
eventsOfSlot :: SlotNo -> [TraceForgeEvent Blk] -> [TraceForgeEvent Blk]
eventsOfSlot slot =
  takeWhile (not . startsLeadershipCheck) . drop 1 . dropWhile (not . startsSlot)
 where
  startsSlot = \case
    TraceStartLeadershipCheck s -> s == slot
    _ -> False
  startsLeadershipCheck = \case
    TraceStartLeadershipCheck{} -> True
    _ -> False

-- | Give the node the certificate that the votes of 'certificateSeats' make
-- for the block's announcement, as if the votes came from its peers.
giveCertificate :: NodeUnderTest (IOSim s) -> Blk -> IOSim s ()
giveCertificate node blk = do
  valid <-
    maybe (error "the block announces no endorser block") pure
      =<< validHeaderAnnouncement systemTime cfg (getHeader blk) onset (SJust leiosView)
  _ <- atomically $ onValidAnnouncement (getLeiosAnnounceState kernel) valid
  for_ certificateSeats $ \seat ->
    addVote (getLeiosVoteState kernel) systemTime $
      MkAcceptedVote
        { acceptedVote = voteOf (rbHashOf blk) seat
        , voteElectionSlot = blockSlot blk
        , voteCommittee = llvCommittee leiosView
        , acceptedQuorum = unboundRational (llvQuorumStakeThreshold leiosView)
        , voteWindow = window
        }
 where
  kernel = nodeKernel node
  cfg = nodeTopLevelConfig node
  leiosView = ltlcLeios (configLedger cfg)
  onset = RelativeTime (fromIntegral (unSlotNo (blockSlot blk)))
  window = leiosVoteWindow leiosView onset

  -- At the start of the vote window, 'addVote' records each vote at once.
  -- At the real time it would wait for the window to open, which is after
  -- the slot that the test forges in.
  systemTime = SystemTime{systemTimeCurrent = pure (voteFrom window), systemTimeWait = pure ()}

-- | Store the closure of the endorser block that the block announces, then
-- emit its acquisition, as LeiosFetch does when it fetches a closure. The
-- ChainDB reads the closure when it adds a block that certifies it.
acquireClosure :: NodeUnderTest (IOSim s) -> Blk -> IOSim s ()
acquireClosure node blk = do
  withWriter (getLeiosDB (nodeKernel node)) $ \writer -> do
    _ <- await =<< writeEbBody writer point (endorserBlockIn (blockSlot blk)) mempty mempty
    await =<< writeTxs writer point (Map.singleton 0 (txInSlot (blockSlot blk)))
  atomically $ emitLeiosAcquisition (nodeLeiosAcquisitions node) (AcquiredEbTxs point)
  -- The ChainDB records the acquisition in its own thread. If it never does,
  -- this waits forever.
  atomically $ do
    acquired <- ChainDB.getIsLeiosClosureAcquired (nodeChainDB node)
    unless (acquired point) retry
 where
  point = fromMaybe (error "the block announces no endorser block") (announcementOf blk)

-- | The seats whose votes make the certificate. Together they hold exactly
-- the quorum, and the certificate names only the seats up to the quorum.
certificateSeats :: [Word16]
certificateSeats = [0, 1]

-- | The point of the endorser block that the block announces, if any.
announcementOf :: Blk -> Maybe LeiosPoint
announcementOf blk =
  (\(ebHash, _size) -> MkLeiosPoint (blockSlot blk) ebHash) <$> lthAnnouncement (ltbHeader blk)

-- | The slot after the tip of the chain.
slotAfter :: [Blk] -> SlotNo
slotAfter chain = case reverse chain of
  [] -> 1
  tip : _ -> succ (blockSlot tip)

rbHashOf :: Blk -> RankingBlockHash
rbHashOf = toRankingBlockHash (Proxy @Blk) . blockHash

-- | The node runs 'forkBlockForging' with 'ebForging' for slots 0 to 3. Then
-- the test reads the chain and the LeiosDB.
--
-- * If the node forges no block, the other checks pass with no data. The
--   first check catches this.
-- * If 'forkBlockForging' does not hand the endorser block on from 'forge', or
--   if the node writes it at a slot or a hash that differs from the
--   announcement in the header, the second check fails.
-- * If the node does not write the transaction bytes, the third check fails.
-- * If the node does not emit both acquisitions at each such point, as it would
--   for an endorser block fetched from a peer, the fourth check fails.
-- * If the node does not relay the announcement and offer both the body and the
--   closure at each such point, the fifth check fails.
unit_storesForgedEbs :: Assertion
unit_storesForgedEbs = do
  let ForgeOutcome{announcedEbs, storedPoints, acquisitions, relayed} = runSimOrThrow forgedEbs
      points = [MkLeiosPoint (announcedSlot eb) (announcedHash eb) | eb <- announcedEbs]
  assertBool "the node forged no block" (not (null announcedEbs))
  sort storedPoints @?= sort points
  map storedTxs announcedEbs @?= map (Just . forgedTxs) announcedEbs
  sort acquisitions
    @?= sort (concat [[AcquiredBody point, AcquiredClosure point] | point <- points])
  sort relayed
    @?= sort
      (concat [[RelayedAnnouncement point, RelayedBody point, RelayedClosure point] | point <- points])

-- | What the chain and the LeiosDB hold after the node forges.
data ForgeOutcome = ForgeOutcome
  { announcedEbs :: [AnnouncedEb]
  -- ^ The endorser blocks that the blocks on the chain announce, oldest first.
  , storedPoints :: [LeiosPoint]
  -- ^ The points whose closures the LeiosDB holds.
  , acquisitions :: [Acquired]
  -- ^ What the node emitted as acquired.
  , relayed :: [Relayed]
  -- ^ What the node's NotifyLog holds.
  }

-- | An acquisition, comparably.
data Acquired = AcquiredBody LeiosPoint | AcquiredClosure LeiosPoint
  deriving (Eq, Ord, Show)

-- | A notification about an endorser block, comparably.
data Relayed
  = RelayedAnnouncement LeiosPoint
  | RelayedBody LeiosPoint
  | RelayedClosure LeiosPoint
  deriving (Eq, Ord, Show)

-- | An endorser block that a block on the chain announces.
data AnnouncedEb = AnnouncedEb
  { announcedSlot :: SlotNo
  , announcedHash :: EndorserBlockHash
  , forgedTxs :: [ByteString]
  -- ^ The bytes of the transactions that 'ebForging' puts in the endorser block.
  , storedTxs :: Maybe [ByteString]
  -- ^ The bytes of the transactions that the LeiosDB holds for 'announcedHash',
  -- or 'Nothing' if it does not hold all of them.
  }

-- | The node forges in each of the first slots.
forgedEbs :: IOSim s ForgeOutcome
forgedEbs = withNode $ \node -> do
  next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
  setBlockForging (nodeKernel node) [MkBlockForging (pure ebForging)]
  -- A slot is one second, so the node forges in slots 0 to 3. The half slot
  -- lets the forge of slot 3 add its block to the ChainDB.
  threadDelay 3.5
  chain <- atomically $ getCurrentChain (nodeChainDB node)
  withReader (getLeiosDB (nodeKernel node)) $ \reader -> do
    announcedEbs <-
      sequence
        [ AnnouncedEb (blockSlot hdr) ebHash [txInSlot (blockSlot hdr)]
            <$> (fmap (map leiosTxBytes) <$> lookupEbClosure reader (MkLeiosPoint (blockSlot hdr) ebHash))
        | hdr <- AF.toOldestFirst chain
        , Just (ebHash, _size) <- [lthAnnouncement hdr]
        ]
    storedPoints <- scanCompleteEbClosuresNotOlderThanSlot reader 0
    acquisitions <- drain next
    relayed <- atomically $ notifications (getLeiosNotifyLog (nodeKernel node)) notifyLogStart
    pure ForgeOutcome{announcedEbs, storedPoints, acquisitions, relayed}
 where
  drain next =
    atomically ((Just <$> next) `orElse` pure Nothing) >>= \case
      Nothing -> pure []
      Just notification -> (acquiredOf notification :) <$> drain next
  acquiredOf = \case
    AcquiredEb point _size -> AcquiredBody point
    AcquiredEbTxs point -> AcquiredClosure point
  notifications notifyLog cursor =
    ( nextNotifyLog notifyLog cursor >>= \(n, cursor') ->
        (relayedOf n ++) <$> notifications notifyLog cursor'
    )
      `orElse` pure []
  relayedOf = \case
    NotifyAnnouncement anc ->
      [ RelayedAnnouncement $
          MkLeiosPoint
            (electionSlot (ancElectionId anc))
            (announcementEbHash (ancAnnouncement anc))
      ]
    NotifyBlockOffer point _size -> [RelayedBody point]
    NotifyBlockTxsOffer point -> [RelayedClosure point]
    -- The node votes for nothing here.
    NotifyVote _ -> []

-- | A forging that leads in every slot. Each block extends the ledger tip and
-- announces an endorser block with one transaction, whose bytes depend on the
-- slot, so that each endorser block has its own hash.
ebForging :: BlockForging (IOSim s) LeiosTestBlock
ebForging =
  BlockForging
    { forgeLabel = "ebForging"
    , canBeLeader = []
    , updateForgeState = \_ _ _ -> pure $ ForgeStateUpdated ()
    , checkCanForge = \_ _ _ _ _ -> Right ()
    , forgeBlock = \ForgeBlockArgs{fbCurrentSlotNo, fbCurrentTickedLedgerState} -> do
        let forgedEb = forgeLeiosEb minBound (txInSlot fbCurrentSlotNo :| [])
            eb = forgedEbBody forgedEb
            ebHash = endorserBlockHash eb
            tip = castPoint (getTip fbCurrentTickedLedgerState)
        pure
          ForgeBlockResult
            { rankingBlock =
                setAnnouncement ebHash (encodeEndorserBlockSize eb) $
                  setSlot fbCurrentSlotNo $
                    blockAfter tip
            , endorserBlock = Just forgedEb
            }
    , finalize = pure ()
    }

-- | A block whose predecessor is the given point. 'forgeBlock' sets its slot.
blockAfter :: Point LeiosTestBlock -> LeiosTestBlock
blockAfter = \case
  GenesisPoint -> firstLeiosBlock 0
  BlockPoint _ (TestHash path) ->
    successorLeiosBlock $
      LeiosTestBlock (ltbHeader (firstLeiosBlock 0)){lthHash = TestHash path}

txInSlot :: SlotNo -> ByteString
txInSlot slot = BS8.pack ("tx in slot " <> show (unSlotNo slot))
