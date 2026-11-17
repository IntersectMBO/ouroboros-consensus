{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests that the node writes each endorser block that it forges to the
-- LeiosDB, and acquires and relays it as it would one fetched from a peer.
module Test.Consensus.Leios.Forge (tests) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Monad (unless, void)
import Control.Monad.Class.MonadThrow (finally)
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract (GetTip (..))
import Ouroboros.Consensus.Leios.Acquisitions
  ( LeiosAcquisition (..)
  , LeiosAcquisitions (..)
  )
import Ouroboros.Consensus.Leios.NotifyLog (Notification (..), nextNotifyLog, notifyLogStart)
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement (..))
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
  )
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.Storage.ChainDB.API (getCurrentChain)
import Ouroboros.Consensus.Storage.LeiosDB (newLeiosDBInMemory)
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , withReader
  )
import Ouroboros.Consensus.Util.IOLike (atomically, check, orElse)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.TestBlock (TestHash (..))

tests :: TestTree
tests =
  testGroup
    "Forging"
    [ testCase
        "the node stores, acquires and relays each endorser block that it forges, at the point that the header announces"
        unit_storesForgedEbs
    , testCase "the ranking block reaches ChainDB while its EB writer is blocked" $
        runSimOrThrow rankingBlockBeforeEbWrite @?= True
    ]

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

-- Gate the writer implementation supplied to the real node. The admission
-- callback must release the forge before this gate opens, without publishing
-- possession. This tests the actual forge-to-ChainDB path, not just callback
-- return or a hand-built stand-in ranking block.
rankingBlockBeforeEbWrite :: IOSim s Bool
rankingBlockBeforeEbWrite = do
  db <- newLeiosDBInMemory
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  let blocked =
        db
          { openWriter = do
              writer <- openWriter db
              pure
                writer
                  { writeEbBody = \point body fills txs -> do
                      atomically $ void (S.tryPutTMVar entered point)
                      atomically $ S.readTMVar release
                      writeEbBody writer point body fills txs
                  }
          }
  withNodeOver blocked $ \node ->
    ( do
        next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
        setBlockForging (nodeKernel node) [MkBlockForging (pure ebForging)]
        point <- atomically $ S.readTMVar entered
        progressed <- timeout 0.5 $ atomically $ do
          chain <- getCurrentChain (nodeChainDB node)
          check (any (announces point) (AF.toOldestFirst chain))
        before <- atomically $ (Just <$> next) `orElse` pure Nothing
        stored <- withReader (getLeiosDB (nodeKernel node)) $ \reader -> lookupEbBody reader point
        unless (isNothing before && isNothing stored) (error "writer-blocked forge published possession")
        atomically $ S.putTMVar release ()
        let awaitClosure =
              atomically next >>= \case
                AcquiredEbTxs acquired | acquired == point -> pure ()
                _ -> awaitClosure
        awaitClosure
        held <- withReader (getLeiosDB (nodeKernel node)) $ \reader -> lookupEbClosure reader point
        pure (progressed == Just () && maybe False (not . null) held)
    )
      `finally` atomically (void (S.tryPutTMVar release ()))
 where
  announces point header =
    blockSlot header == pointSlotNo point
      && maybe False ((== pointEbHash point) . fst) (lthAnnouncement header)

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
