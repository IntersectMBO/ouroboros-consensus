{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests that the node writes each endorser block that it forges to the
-- LeiosDB, and acquires and relays it as it would one fetched from a peer, and
-- that 'forge' puts a Leios certificate in a block exactly when it may.
module Test.Consensus.Leios.Forge (tests) where

import Cardano.Crypto.Leios (LeiosCert)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (Tracer (..), emit, nullTracer)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, maybeToList)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract (GetTip (..))
import Ouroboros.Consensus.Leios.Acquisitions
  ( LeiosAcquisition (..)
  , LeiosAcquisitions (..)
  )
import Ouroboros.Consensus.Leios.NotifyLog (Notification (..), nextNotifyLog, notifyLogStart)
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosProtocol (..)
  , NotCertifiable (..)
  , WhyNotCertifiable (..)
  )
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
  )
import Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..), TraceLabelCreds (..))
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.NodeKernel.Forge (forge)
import Ouroboros.Consensus.Storage.ChainDB.API (getCurrentChain)
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbReader (..), withReader)
import Ouroboros.Consensus.Util.EarlyExit (withEarlyExit_)
import Ouroboros.Consensus.Util.IOLike
  ( atomically
  , modifyTVar
  , newTVarIO
  , orElse
  , readTVarIO
  , uncheckedNewTVarM
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.Consensus.Leios.Committee (certOf)
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
  chain = chainOf scenario
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

-- | The chain of the scenario. Its first block is in slot 1, and each next
-- block is in the next slot.
chainOf :: Scenario -> [Blk]
chainOf Scenario{announces} =
  zipWith announceIf announces (iterate successorLeiosBlock (firstLeiosBlock 1))
 where
  announceIf False blk = blk
  announceIf True blk =
    setAnnouncement (endorserBlockHash eb) (encodeEndorserBlockSize eb) blk
   where
    eb = forgedEbBody (forgeLeiosEb EvenProtVer (txInSlot (blockSlot blk) :| []))

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
      (\_ _ _ _ -> pure ())
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
    , canBeLeader = ()
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
    , canBeLeader = ()
    , updateForgeState = \_ _ _ -> pure $ ForgeStateUpdated ()
    , checkCanForge = \_ _ _ _ _ -> Right ()
    , forgeBlock = \ForgeBlockArgs{fbCurrentSlotNo, fbCurrentTickedLedgerState} -> do
        let forgedEb = forgeLeiosEb EvenProtVer (txInSlot fbCurrentSlotNo :| [])
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
