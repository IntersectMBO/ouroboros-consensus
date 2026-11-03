{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests of the node's LeiosFetch client and server, over real connections,
-- against peers run by the Leios node-vs-environment tests.
module Test.Consensus.Leios.LeiosFetch.Node (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Network.NodeToNode (chainSyncMiniProtocolNum, defaultMiniProtocolParameters)
import Cardano.Network.NodeToNode.Version (NodeToNodeVersion (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM, forM_, replicateM, replicateM_, unless, void)
import Control.Monad.Class.MonadAsync (withAsync)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), addTime, diffTime, getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.Except (runExcept, runExceptT)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.Bifunctor (first)
import Data.Bits ((.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Time (NominalDiffTime, addUTCTime)
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec (PeerRole (..))
import Network.TypedProtocol.Core (IsPipelined (..), ReflRelativeAgency (..))
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.Block
  ( Header
  , Point
  , Proxy (..)
  , SlotNo (..)
  , WithOrigin (..)
  , blockHash
  , blockPoint
  , blockSlot
  , pointSlot
  , withOrigin
  , pattern GenesisPoint
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Default (defaultSystemTime)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemStart (SystemStart)
  )
import Ouroboros.Consensus.Config (configBlock, configLedger)
import Ouroboros.Consensus.Config.SupportsNode (getSystemStart)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Ouroboros.Consensus.Leios.Acquisitions
  ( LeiosAcquisitions (..)
  , LeiosEbNotification (..)
  , newLeiosAcquisitions
  )
import Ouroboros.Consensus.Leios.AnnounceState (newAnnounceState)
import qualified Ouroboros.Consensus.Leios.NotifyLog as NotifyLog
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , LeiosTx (..)
  , LeiosVote
  , TxHash
  , encodeEndorserBlockSize
  , endorserBlockHash
  , endorserBlockReferences
  , hashLeiosTx
  , mkEndorserBlock
  , toRankingBlockHash
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( ExnLeiosFetchInvalidReply (..)
  , LeiosFetchClientContext (..)
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosOutstanding (..)
  , PeerOffer (..)
  , WhetherTxsClosureOffered (..)
  , isFocusedPoint
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
  ( ExnLeiosTwoCertificationClaims (..)
  , LeiosFetchKernel (..)
  , LeiosFetchPeer (..)
  , LeiosPeerLifetimeId (..)
  , recordLeiosOffer
  , withLeiosFetchPeer
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
  ( ExnLeiosInvalidRequest (..)
  , LeiosFetchRequestHandler (..)
  , leiosFetchServerPeer
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
  ( leiosNotifyClientPeerPipelined
  , toLeiosNotifyClientPeerPipelined
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( LeiosAnnouncementAgeLimits (..)
  , defaultLeiosAnnouncementAgeLimits
  , mkAnnouncingHeader
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( ExnLeiosNotifyInvalidOffer (..)
  , checkLeiosBlockOffer
  , checkLeiosClosureOffer
  , emptyLeiosNotifyPeerState
  , onLeiosNotifyAnnouncement
  , pruneLeiosNotifyPeerState
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Server
  ( WhetherExcessiveRequests (..)
  , leiosNotifyServerPeerLookahead
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
  ( LeiosNotify
  , Message
    ( MsgCanceled
    , MsgLeiosBlockAnnouncement
    , MsgLeiosBlockOffer
    , MsgLeiosBlockTxsOffer
    , MsgLeiosVotes
    )
  , byteLimitsLeiosNotify
  , timeLimitsLeiosNotify
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type as LN
import Ouroboros.Consensus.Network.LeiosFetch (leiosFetchMiniProtocolNum)
import Ouroboros.Consensus.Network.LeiosNotify
  ( leiosNotifyMiniProtocolNum
  , runLookaheadFixedSenderPeerWithLimits
  )
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LeiosDB (newLeiosDBInMemory)
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , Promise (..)
  , withWriter
  )
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import Ouroboros.Network.Block (Tip (..))
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.Driver.Limits (runPipelinedPeerWithLimits)
import Ouroboros.Network.Driver.Simple (runAnnotatedPeer)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , TemperatureBundle (..)
  , WithProtocolTemperature (..)
  )
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import Test.Consensus.Leios.Committee (certOf)
import Test.Consensus.Leios.Environment
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
  ( LedgerState (..)
  , LeiosTestBlock (..)
  , firstLeiosBlock
  , forkLeiosBlock
  , leiosTestInitExtLedger
  , setAnnouncement
  , setLeiosCert
  , successorLeiosBlock
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , checkCoverage
  , choose
  , cover
  , elements
  , forAll
  , ioProperty
  , oneof
  , sublistOf
  , suchThat
  , tabulate
  , testProperty
  , withNumTests
  , (===)
  )
import Test.Util.ChainDB (emptyNodeDBs)

tests :: TestTree
tests =
  testGroup
    "node vs environment"
    [ testProperty
        "the node's server serves what it holds, disconnects a client that requests anything else, and ends when the client sends MsgDone"
        prop_serverAnswers
    , testCase
        "the node offers an endorser block it acquires only at the point it acquires it at"
        $ runSimFinishing relayOffersAtAcquiredPoint
          >>= ( @?=
                  [ RelayedBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
                  , RelayedBlockTxsOffer fetchedPoint
                  ]
              )
    , testCase
        "the node announces its acquisition again, and offers it there, each time a later election focuses an endorser block it holds"
        $ runSimFinishing nodeReacquiresHeld
          >>= ( @?=
                  ( concat
                      [ [AcquiredBody point, AcquiredClosure point]
                      | point <- [fetchedPoint, refocusedPoint, refocusedAgainPoint]
                      ]
                  , concatMap relayedFetchedAt [fetchedPoint, refocusedPoint, refocusedAgainPoint]
                  )
              )
    , testCase
        "the node announces its acquisition again, and offers it there, when a certificate focuses an endorser block it holds"
        $ runSimFinishing nodeReacquiresCertified
          >>= ( @?=
                  (
                    [ AcquiredBody fetchedPoint
                    , AcquiredClosure fetchedPoint
                    , AcquiredBody refocusedPoint
                    , AcquiredClosure refocusedPoint
                    ]
                  ,
                    [ RelayedAnnouncement
                    , RelayedBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
                    , RelayedBlockTxsOffer fetchedPoint
                    , RelayedAnnouncement
                    , RelayedAnnouncement
                    , RelayedBlockOffer refocusedPoint (encodeEndorserBlockSize fetchedEb)
                    , RelayedBlockTxsOffer refocusedPoint
                    ]
                  )
              )
    , testCase "the node fetches the endorser block a header its peer rolls forward certifies" $
        runSimFinishing nodeFetchesCertified
          >>= (@?= [AcquiredBody fetchedPoint, AcquiredClosure fetchedPoint])
    , testCase
        "the node fetches an endorser block that headers its peer rolls forward announce and certify, and offers it on"
        $ runSimFinishing nodeFetchesRolledForward
          >>= ( @?=
                  ( [AcquiredBody fetchedPoint, AcquiredClosure fetchedPoint]
                  ,
                    [ RelayedAnnouncement
                    , RelayedBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
                    , RelayedBlockTxsOffer fetchedPoint
                    ]
                  )
              )
    , testCase "the node takes a peer that jumps over a certifying header as offering what it certifies" $
        runSimFinishing nodeRecordsJumperOffers >>= (@?= [True, True])
    , testCase "the node fetches what a certifying header certifies from a peer that jumps over it" $
        runSimFinishing nodeFetchesFromJumper >>= (@?= lastPoint jumpedChain)
    , testCase
        "the node fetches what a certifying header certifies from a jumper whose jump over it starts at the announcing header"
        $ runSimFinishing nodeFetchesFromJumperInSteps
          >>= ( @?=
                  ( [NotOrigin (blockSlot announcingFetched), NotOrigin (blockSlot certifyingFetched)]
                  , lastPoint jumpedChain
                  )
              )
    , testCase "the node records no offer older than the slot it pruned its peers' offers to" $
        runSimFinishing nodeIgnoresPrunedOffer >>= (@?= [SlotNo 5])
    , testCase
        "the node disconnects a peer that rolls forward certificates of two endorser blocks for one election"
        $ runSimFinishing nodeDisconnectsDoubleCertifier
          >>= (@?= Just (endorserBlockHash fetchedEb, otherEbHash))
    , testCase
        "the node selects a block certifying an endorser block it acquired at an earlier election, whether it prunes that announcement before, after, or without seeing the endorser block announced again"
        $ forM_ [minBound .. maxBound]
        $ \whenReannounced ->
          runSimFinishing (nodeSelectsReannounced whenReannounced)
            >>= assertEqual (show whenReannounced) (lastPoint (reannouncedChain whenReannounced))
    , testCase
        "the node fetches and selects what a certificate certifies at an election's third announcement, which it does not relay"
        $ runSimFinishing nodeFetchesCertifiedThird
          >>= (@?= (blockPoint certifyingFetched, [RelayedAnnouncement, RelayedAnnouncement]))
    , testCase
        "the node keeps an endorser block that a certificate refocuses an election onto through garbage collection and a restart, whether or not it held the endorser block at start-up"
        $ forM_ [minBound .. maxBound]
        $ \restarts ->
          runSimFinishingWithin 200 (nodeKeepsRefocusedThroughGc restarts)
            >>= assertEqual (show restarts) (lastPoint gcRestartChain)
    , testCase
        "the node fetches an endorser block a later election announced, once it prunes the earlier announcement"
        $ runSimFinishing nodeFetchesAfterPruningEarlier
          >>= ( @?=
                  ( [AcquiredBody refocusedPoint, AcquiredClosure refocusedPoint]
                  ,
                    [ RelayedAnnouncement
                    , RelayedAnnouncement
                    , RelayedBlockOffer refocusedPoint (encodeEndorserBlockSize fetchedEb)
                    , RelayedBlockTxsOffer refocusedPoint
                    ]
                  )
              )
    , testCase
        "the node selects a block certifying an endorser block though a rival announcement took the focus first, whichever of the announcing and certifying blocks arrives first"
        $ forM_ [minBound .. maxBound]
        $ \arrival ->
          runSimFinishing (nodeSelectsCertifiedRival arrival)
            >>= assertEqual
              (show arrival)
              (True, blockPoint certifyingFetched, relayedFetchedAfterRival)
    , testCase
        "the node fetches an endorser block at the size its peer offers, though the announcement states another"
        $ runSimFinishing nodeFetchesAtOfferedSize
          >>= (@?= [AcquiredBody fetchedPoint, AcquiredClosure fetchedPoint])
    , testCase
        "the node selects a block certifying an endorser block that another election announced at another size"
        $ runSimFinishing nodeSelectsDespiteMisstatedSize
          >>= (@?= blockPoint certifyingFetched)
    , testProperty
        "the node acquires an endorser block whose body and closure the ledger admits, and otherwise acquires nothing of it and disconnects the peer that served it"
        prop_boundedEb
    , testCase
        "the node selects a block certifying an endorser block once the block arrives again after a restart forgot it"
        $ runSimFinishing nodeReselectsForgotten
          >>= (@?= ((1, True), (True, 0, lastPoint forgottenChain, relayedFetchedAfterRival)))
    , testCase
        "the node relays an announcement to a peer that connects after it, before offering that peer the endorser block"
        $ runSimFinishing nodeAnnouncesToLateListener
          >>= ( @?=
                  [ RelayedAnnouncement
                  , RelayedBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
                  , RelayedBlockTxsOffer fetchedPoint
                  ]
              )
    , testCase
        "the node disconnects a peer that offers an endorser block's body again once the node has requested it"
        $ runSimFinishing nodeDisconnectsReofferAfterRequest
          >>= (@?= Just (show (ExnLeiosRepeatedBlockOffer fetchedPoint)))
    , testCase
        "the node does not offer the closure of an endorser block that misstates the size of a transaction, whether the node holds that transaction already or fetches it afterwards"
        $ forM_ [minBound .. maxBound]
        $ \arrival ->
          runSimFinishing (nodeOffersLyingEb arrival)
            >>= assertEqual
              (show arrival)
              ( case arrival of
                  TxBeforeBody -> relayedHonestEb <> relayedLyingEb
                  BodyBeforeTx -> relayedLyingEb <> relayedHonestEb
              )
    , testProperty
        "the node relays an announcement, and offers at its point, to a listener that connects before the node stops relaying it, and nothing to one that connects after, whatever the listener's immutable tip and clock"
        prop_listenerReach
    , testCase "the node runs LeiosFetch from NodeToNodeV_16 on, and not before" $
        forM_ [minBound .. maxBound] $ \ntnVersion -> do
          let runs = ntnVersion >= NodeToNodeV_16
          runSimFinishing (nodeRunsLeiosFetch ntnVersion) >>= assertEqual (show ntnVersion) (runs, runs)
    ]

-- | Run the simulation, and fail the test if it has not finished after 100
-- simulated seconds.
runSimFinishing :: (forall s. IOSim s a) -> IO a
runSimFinishing = runSimFinishingWithin 100

-- | Run the simulation, and fail the test if it has not finished after this
-- many simulated seconds.
runSimFinishingWithin :: DiffTime -> (forall s. IOSim s a) -> IO a
runSimFinishingWithin limit sim = case runSimOrThrow (timeout limit sim) of
  Nothing -> assertFailure $ "the simulation did not finish within " <> show limit
  Just a -> pure a

{-------------------------------------------------------------------------------
  The node's client
-------------------------------------------------------------------------------}

-- | What the node announced acquiring, comparably.
data Acquired = AcquiredBody LeiosPoint | AcquiredClosure LeiosPoint
  deriving (Eq, Show)

-- | What the node relayed over LeiosNotify, comparably.
data Relayed
  = RelayedAnnouncement
  | RelayedBlockOffer LeiosPoint BytesSize
  | RelayedBlockTxsOffer LeiosPoint
  | RelayedVotes
  | -- | Something a receiving node would disconnect the node for, or an offer
    -- the node did not serve, and why.
    RelayedDishonestly String
  deriving (Eq, Show)

-- | The transactions of 'fetchedEb'.
fetchedTxs :: V.Vector LeiosTx
fetchedTxs = V.fromList [MkLeiosTx (BS.replicate 100 i) | i <- [1, 2, 3]]

-- | The endorser block the peer announces, offers, and serves.
fetchedEb :: EndorserBlock
fetchedEb =
  mkEndorserBlock
    [(hashLeiosTx tx, fromIntegral (BS.length (leiosTxBytes tx))) | tx <- V.toList fetchedTxs]

announcingFetched :: LeiosTestBlock
announcingFetched =
  setAnnouncement
    (endorserBlockHash fetchedEb)
    (encodeEndorserBlockSize fetchedEb)
    (firstLeiosBlock 0)

fetchedPoint :: LeiosPoint
fetchedPoint = MkLeiosPoint (blockSlot announcingFetched) (endorserBlockHash fetchedEb)

-- | A later block, which announces 'fetchedEb' again, in an election of its
-- own.
reannouncingFetched :: LeiosTestBlock
reannouncingFetched =
  setAnnouncement
    (endorserBlockHash fetchedEb)
    (encodeEndorserBlockSize fetchedEb)
    (successorLeiosBlock announcingFetched)

refocusedPoint :: LeiosPoint
refocusedPoint = MkLeiosPoint (blockSlot reannouncingFetched) (endorserBlockHash fetchedEb)

-- | A sibling of 'reannouncingFetched' by the same issuer, which announces
-- 'otherEbHash' instead.
reannouncingOther :: LeiosTestBlock
reannouncingOther =
  setAnnouncement otherEbHash (encodeEndorserBlockSize fetchedEb) (forkLeiosBlock reannouncingFetched)

-- | The successor of 'announcingFetched', which certifies 'fetchedEb'.
certifyingFetched :: LeiosTestBlock
certifyingFetched = certifying announcingFetched

-- | The successor of the block, containing a certificate of what it announced.
certifying :: LeiosTestBlock -> LeiosTestBlock
certifying announcing =
  setLeiosCert
    (certOf (toRankingBlockHash (Proxy @LeiosTestBlock) (blockHash announcing)) [0, 1, 2])
    (successorLeiosBlock announcing)

-- | Another endorser block, for the same election as 'fetchedEb'.
otherEbHash :: EndorserBlockHash
otherEbHash = UnsafeHash (SBS.replicate 32 3)

-- | A sibling of 'announcingFetched' by the same issuer, which announces
-- 'otherEbHash' instead.
announcingOther :: LeiosTestBlock
announcingOther =
  setAnnouncement otherEbHash (encodeEndorserBlockSize fetchedEb) (forkLeiosBlock announcingFetched)

-- | A third sibling of 'announcingFetched' by the same issuer, which announces
-- 'anotherEbHash'.
announcingAnother :: LeiosTestBlock
announcingAnother =
  setAnnouncement anotherEbHash (encodeEndorserBlockSize fetchedEb) (forkLeiosBlock announcingOther)

-- | A third endorser block for the election of 'fetchedEb'.
anotherEbHash :: EndorserBlockHash
anotherEbHash = UnsafeHash (SBS.replicate 32 4)

-- | One peer announces 'announcingOther' and 'announcingAnother', and the node
-- relays both. Only then does another peer announce 'fetchedEb' in the same
-- election, in 'announcingFetched', offer it, and serve it: the election's
-- third announcement, which the node takes in from that peer but does not
-- relay or fetch for. Then the node adds 'announcingFetched' and
-- 'certifyingFetched'. The tip of the node's selection, once that is
-- 'certifyingFetched' or ten seconds have passed, and the notifications the
-- node relays to the first peer, by 'relayHorizon' after that.
nodeFetchesCertifiedThird :: IOSim s (Point LeiosTestBlock, [Relayed])
nodeFetchesCertifiedThird = withNode $ \nut -> do
  heard <- newListener nut
  announcerNotifications <- newTQueueIO
  holderNotifications <- newTQueueIO
  atomically $ do
    mapM_
      (writeTQueue announcerNotifications . MsgLeiosBlockAnnouncement . ltbHeader)
      [announcingOther, announcingAnother]
    mapM_ (writeTQueue holderNotifications) (announcingAndOffering fetchedEb announcingFetched)
  let announcer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut announcerNotifications
          , epLeiosNotifyClient = Just (lsNotifyClient heard)
          , epLeiosFetchServer = silentLeiosFetchServer nut
          , epLeiosFetchClient = Just (lsFetchClient heard)
          }
      holder =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut holderNotifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  withNodeVsEnvConnection nut (PeerAddr 1) announcer $ \_announcerConn -> do
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar (lsRelayed heard) >>= check . (>= 2) . length
    withNodeVsEnvConnection nut (PeerAddr 2) holder $ \_holderConn -> do
      threadDelay 1
      addBlocks nut [announcingFetched, certifyingFetched]
      (,) <$> awaitTip nut (blockPoint certifyingFetched) <*> relayedBy heard 2

-- | The log holds three announcements of 'fetchedEb', at three points. The node
-- then acquires 'fetchedEb' and its closure at the first. The offers the relay
-- adds to the log.
relayOffersAtAcquiredPoint :: IOSim s [Relayed]
relayOffersAtAcquiredPoint = withNode $ \nut -> do
  notifyLog <- NotifyLog.newNotifyLog
  acquisitions <- newLeiosAcquisitions
  let announce blk =
        forM_ (mkAnnouncingHeader (nutTopLevelConfig nut) (ltbHeader blk)) $ \anc ->
          NotifyLog.appendNotifyLog notifyLog (NotifyLog.NotifyAnnouncement anc) (RelativeTime 1000)
      announcingAgain =
        setAnnouncement
          (endorserBlockHash fetchedEb)
          (encodeEndorserBlockSize fetchedEb)
          (successorLeiosBlock reannouncingFetched)
  atomically $ do
    announce announcingFetched
    announce reannouncingFetched
    announce announcingAgain
  withAsync (NotifyLog.leiosOfferRelay acquisitions notifyLog) $ \_ -> do
    -- Until the relay has subscribed.
    threadDelay 1
    atomically $ do
      announceLeiosAcquisition acquisitions (AcquiredEb fetchedPoint (encodeEndorserBlockSize fetchedEb))
      announceLeiosAcquisition acquisitions (AcquiredEbTxs fetchedPoint)
    threadDelay 1
    offersFrom notifyLog NotifyLog.notifyLogStart
 where
  offersFrom notifyLog cursor =
    atomically ((Just <$> NotifyLog.nextNotifyLog notifyLog cursor) `orElse` pure Nothing) >>= \case
      Nothing -> pure []
      Just (notification, cursor') ->
        let rest = offersFrom notifyLog cursor'
         in case notification of
              NotifyLog.NotifyBlockOffer p size -> (RelayedBlockOffer p size :) <$> rest
              NotifyLog.NotifyBlockTxsOffer p -> (RelayedBlockTxsOffer p :) <$> rest
              NotifyLog.NotifyAnnouncement{} -> rest
              NotifyLog.NotifyVote{} -> rest

-- | The peer announces 'fetchedEb', offers its body and its closure, and
-- serves both. Then it announces 'fetchedEb' again, in 'reannouncingFetched'
-- and then in 'reannouncingFetchedAgain', each once the node has acquired it at
-- the previous announcement, and offers nothing more.
-- The node's acquisitions, as it announces them, and the notifications it
-- relays to the peer, by 'relayHorizon' after the ninth.
nodeReacquiresHeld :: IOSim s ([Acquired], [Relayed])
nodeReacquiresHeld = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  listener <- newListener nut
  notifications <- newTQueueIO
  atomically $
    mapM_
      (writeTQueue notifications)
      [ MsgLeiosBlockAnnouncement (ltbHeader announcingFetched)
      , MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
      , MsgLeiosBlockTxsOffer fetchedPoint
      ]
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Just (lsNotifyClient listener)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Just (lsFetchClient listener)
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    acquired <- replicateM 2 $ acquiredOf <$> atomically next
    reacquired <- forM [reannouncingFetched, reannouncingFetchedAgain] $ \blk -> do
      atomically $ writeTQueue notifications (MsgLeiosBlockAnnouncement (ltbHeader blk))
      replicateM 2 $ acquiredOf <$> atomically next
    relays <- relayedBy listener 9
    pure (acquired <> concat reacquired, relays)

-- | The peer announces 'fetchedEb', offers its body and its closure, and
-- serves both. Then it announces 'otherEbHash' and then 'fetchedEb' in a later
-- election, so that the former takes its focus, and the node's ChainDB then
-- verifies a certificate of the latter. The node's acquisitions, as it
-- announces them, and the notifications it relays to the peer, by
-- 'relayHorizon' after the seventh.
nodeReacquiresCertified :: IOSim s ([Acquired], [Relayed])
nodeReacquiresCertified = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  listener <- newListener nut
  notifications <- newTQueueIO
  atomically $
    mapM_
      (writeTQueue notifications)
      [ MsgLeiosBlockAnnouncement (ltbHeader announcingFetched)
      , MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
      , MsgLeiosBlockTxsOffer fetchedPoint
      ]
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Just (lsNotifyClient listener)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Just (lsFetchClient listener)
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    acquired <- replicateM 2 $ acquiredOf <$> atomically next
    atomically $
      mapM_
        (writeTQueue notifications . MsgLeiosBlockAnnouncement . ltbHeader)
        [reannouncingOther, reannouncingFetched]
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar (lsRelayed listener) >>= check . (>= 5) . length
    addBlocks nut [announcingFetched, reannouncingFetched, certifying reannouncingFetched]
    reacquired <- replicateM 2 $ acquiredOf <$> atomically next
    relays <- relayedBy listener 7
    pure (acquired <> reacquired, relays)

-- | The peer announces 'fetchedEb', which so takes its election's focus, and
-- then offers nothing over LeiosNotify. It rolls forward the header that
-- certifies it, and serves it. The node's acquisitions, as it announces them.
nodeFetchesCertified :: IOSim s [Acquired]
nodeFetchesCertified = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  notifications <- newTQueueIO
  atomically $ writeTQueue notifications (MsgLeiosBlockAnnouncement (ltbHeader announcingFetched))
  let envPeer =
        EnvPeer
          { epChainSyncServer =
              scriptedChainSyncServer $
                SendRollForward . ltbHeader <$> [announcingFetched, certifyingFetched]
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn ->
    replicateM 2 $ acquiredOf <$> atomically next

-- | The peer says nothing over LeiosNotify. It rolls forward the header that
-- announces 'fetchedEb' and the one that certifies it, and serves it. The
-- node's acquisitions, as it announces them, and the notifications it relays
-- to the peer, by 'relayHorizon' after the third.
nodeFetchesRolledForward :: IOSim s ([Acquired], [Relayed])
nodeFetchesRolledForward = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  listener <- newListener nut
  let envPeer =
        EnvPeer
          { epChainSyncServer =
              scriptedChainSyncServer $
                SendRollForward . ltbHeader <$> [announcingFetched, certifyingFetched]
          , epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Just (lsNotifyClient listener)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Just (lsFetchClient listener)
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    acquired <- replicateM 2 $ acquiredOf <$> atomically next
    relays <- relayedBy listener 3
    pure (acquired, relays)

-- | Two peers serve the chain of 'announcingFetched' and 'certifyingFetched' to
-- a node that jumps one slot at a time. The first to connect leads the jumps,
-- so the second jumps over 'certifyingFetched' instead of sending it. Whether
-- the node takes each peer as offering 'fetchedEb', in the order they
-- connected.
--
-- A jump reaches only the predecessor of the header the leader last sent, so
-- the leader's chain then grows by a block.
nodeRecordsJumperOffers :: IOSim s [Bool]
nodeRecordsJumperOffers = withJumpingNode 1 $ \nut -> do
  leaderChain <- newEnvChain [announcingFetched, certifyingFetched]
  jumperChain <- newEnvChain longer
  withNodeVsEnvConnection nut (PeerAddr 1) (servingChain nut leaderChain) $ \_conn -> do
    threadDelay 3
    withNodeVsEnvConnection nut (PeerAddr 2) (servingChain nut jumperChain) $ \_conn -> do
      threadDelay 1
      serveChain leaderChain longer
      threadDelay 2
      mapM (offersFetched nut) [PeerAddr 1, PeerAddr 2]
 where
  longer = [announcingFetched, certifyingFetched, successorLeiosBlock certifyingFetched]

-- | The leader's chain once it has grown by a block, as in
-- 'nodeRecordsJumperOffers'.
jumpedChain :: [LeiosTestBlock]
jumpedChain = [announcingFetched, certifyingFetched, successorLeiosBlock certifyingFetched]

-- | As in 'nodeRecordsJumperOffers', but only the jumper serves 'fetchedEb'
-- over LeiosFetch, so the node can acquire it only by taking the jumper as
-- offering it. Once the leader's chain has grown, the node adds that chain.
-- The tip of the node's selection, once that is the last block of
-- 'jumpedChain' or ten seconds have passed.
nodeFetchesFromJumper :: IOSim s (Point LeiosTestBlock)
nodeFetchesFromJumper = withJumpingNode 1 $ \nut -> do
  leaderChain <- newEnvChain [announcingFetched, certifyingFetched]
  jumperChain <- newEnvChain jumpedChain
  let jumper = (servingChain nut jumperChain){epLeiosFetchServer = servingFetched nut}
  withNodeVsEnvConnection nut (PeerAddr 1) (servingChain nut leaderChain) $ \_conn -> do
    threadDelay 3
    withNodeVsEnvConnection nut (PeerAddr 2) jumper $ \_conn -> do
      threadDelay 1
      serveChain leaderChain jumpedChain
      threadDelay 2
      addBlocks nut jumpedChain
      awaitTip nut (lastPoint jumpedChain)

-- | As 'nodeFetchesFromJumper', but the leader's chain grows a block at a time
-- from 'announcingFetched', so that the jumper's second jump, which reaches
-- 'certifyingFetched', starts where its first ended, at 'announcingFetched'.
-- The slot the node takes the jumper's jumps to have reached after each block,
-- and the tip of the node's selection, once that is the last block of
-- 'jumpedChain' or ten seconds have passed.
nodeFetchesFromJumperInSteps :: IOSim s ([WithOrigin SlotNo], Point LeiosTestBlock)
nodeFetchesFromJumperInSteps = withJumpingNode 1 $ \nut -> do
  leaderChain <- newEnvChain (take 1 jumpedChain)
  jumperChain <- newEnvChain jumpedChain
  let jumper = (servingChain nut jumperChain){epLeiosFetchServer = servingFetched nut}
  withNodeVsEnvConnection nut (PeerAddr 1) (servingChain nut leaderChain) $ \_conn -> do
    threadDelay 3
    withNodeVsEnvConnection nut (PeerAddr 2) jumper $ \_conn -> do
      reached <- forM [2, 3] $ \n -> do
        threadDelay 1
        serveChain leaderChain (take n jumpedChain)
        threadDelay 2
        jumpedThrough nut (PeerAddr 2)
      addBlocks nut jumpedChain
      (,) reached <$> awaitTip nut (lastPoint jumpedChain)

-- | The slot of the header the node takes the peer at the address to have
-- jumped through.
jumpedThrough :: NodeUnderTest (IOSim s) -> PeerAddr -> IOSim s (WithOrigin SlotNo)
jumpedThrough nut addr = IOLike.atomically $ do
  peers <- IOLike.readTVar (lfkPeers (getLeiosFetchKernel (nutKernel nut)))
  case [peer | (MkLeiosPeerLifetimeId connId _, peer) <- Map.toList peers, remoteAddress connId == addr] of
    [] -> pure Origin
    peer : _ -> pointSlot <$> IOLike.readTVar (lfpJumpedThrough peer)

-- | Once the node has pruned its peers' offers to a slot, it records only the
-- offers at or after that slot. The slots of the offers it records.
nodeIgnoresPrunedOffer :: IOSim s [SlotNo]
nodeIgnoresPrunedOffer = withNode $ \nut -> do
  let kernel = getLeiosFetchKernel (nutKernel nut)
      offer =
        MkPeerOffer
          { poEbBounds = SNothing
          , poOfferedBody = SNothing
          , poClosure = TxsClosureOffered
          }
      peer = ConnectionId (PeerAddr 0) (PeerAddr 1)
  IOLike.atomically $ IOLike.writeTVar (lfkPrunedSlot kernel) (SlotNo 5)
  withLeiosFetchPeer kernel peer IsNotBigLedgerPeer $ \_lifetimeId peerVars -> do
    forM_ [SlotNo 4, SlotNo 5] $ \slot ->
      recordLeiosOffer kernel peerVars (MkLeiosPoint slot (endorserBlockHash fetchedEb)) offer
    map pointSlotNo . Map.keys <$> IOLike.readTVarIO (lfpOfferings peerVars)

-- | The peer rolls forward the header that certifies 'fetchedEb', rolls back,
-- and rolls forward one that certifies 'otherEbHash'. The two endorser blocks
-- the node's ChainSync client failed for claiming, if it did.
nodeDisconnectsDoubleCertifier :: IOSim s (Maybe (EndorserBlockHash, EndorserBlockHash))
nodeDisconnectsDoubleCertifier = withNode $ \nut -> do
  let envPeer =
        EnvPeer
          { epChainSyncServer =
              scriptedChainSyncServer
                [ SendRollForward (ltbHeader announcingFetched)
                , SendRollForward (ltbHeader certifyingFetched)
                , SendRollBackward GenesisPoint TipGenesis
                , SendRollForward (ltbHeader announcingOther)
                , SendRollForward (ltbHeader (certifying announcingOther))
                ]
          , epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = silentLeiosFetchServer nut
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    chainSync <-
      maybe (throwIO $ userError "the node does not run a ChainSync client") pure $
        Map.lookup chainSyncMiniProtocolNum (connNodeInitiators conn)
    atomically chainSync <&> \case
      Left e
        | Just (ExnLeiosTwoCertificationClaims _elId claimed claimedAgain) <- fromException e ->
            Just (claimed, claimedAgain)
      _ -> Nothing

-- | When the peer announces 'fetchedEb' again, relative to the node pruning
-- 'announcingFetched'.
data WhenReannounced
  = -- | The chain is only k blocks long, so the node prunes nothing, and the
    -- peer does not announce 'fetchedEb' again: only the certificate names the
    -- second announcement.
    NothingPruned
  | PruneThenReannounce
  | ReannounceThenPrune
  deriving (Show, Enum, Bounded)

-- | The chain the node adds, one block per slot from 'announcingFetched'. Its
-- second-to-last block announces 'fetchedEb' again, and its last block
-- certifies that announcement.
reannouncedChain :: WhenReannounced -> [LeiosTestBlock]
reannouncedChain whenReannounced = take beforeLength blocks <> [reannouncing, certifying reannouncing]
 where
  blocks = iterate successorLeiosBlock announcingFetched
  reannouncing =
    setAnnouncement
      (endorserBlockHash fetchedEb)
      (encodeEndorserBlockSize fetchedEb)
      (blocks !! beforeLength)
  beforeLength = case pruningPrefix whenReannounced of
    Nothing -> 8
    Just n -> n + 2

-- | How many blocks of 'reannouncedChain' the node adds first, so that its
-- immutable tip passes 'announcingFetched'. With k at 10, fourteen blocks put
-- it at the fourth.
pruningPrefix :: WhenReannounced -> Maybe Int
pruningPrefix = \case
  NothingPruned -> Nothing
  PruneThenReannounce -> Just 14
  ReannounceThenPrune -> Just 14

-- | The peer announces 'fetchedEb' in 'announcingFetched', offers it, and
-- serves it. Once the node has acquired it, the node adds the 'pruningPrefix'
-- of 'reannouncedChain' and prunes, and the peer announces and offers
-- 'fetchedEb' again where that chain does, in the order the argument says.
-- Then the node adds the whole chain. The tip of the node's selection, once
-- that is the chain's last block or ten seconds have passed.
nodeSelectsReannounced :: WhenReannounced -> IOSim s (Point LeiosTestBlock)
nodeSelectsReannounced whenReannounced = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  notifications <- newTQueueIO
  let chain = reannouncedChain whenReannounced
      announceAndOffer = atomically . mapM_ (writeTQueue notifications) . announcingAndOffering fetchedEb
      reannounce = do
        announceAndOffer (chain !! (length chain - 2))
        -- Let the node take it in.
        threadDelay 1
      prune = forM_ (pruningPrefix whenReannounced) $ \n -> do
        addBlocks nut (take n chain)
        awaitPrunedPast nut (blockSlot announcingFetched)
      envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    announceAndOffer announcingFetched
    _ <- replicateM 2 $ atomically next
    case whenReannounced of
      NothingPruned -> pure ()
      PruneThenReannounce -> prune >> reannounce
      ReannounceThenPrune -> reannounce >> prune
    addBlocks nut chain
    awaitTip nut (lastPoint chain)

-- | 'reannouncedChain' 'NothingPruned' and five more blocks, which put the
-- immutable tip past 'announcingFetched' but leave the certifying block
-- volatile, so that nothing pins 'fetchedEb'.
gcRestartChain :: [LeiosTestBlock]
gcRestartChain = chain <> take 5 (drop 1 (iterate successorLeiosBlock (last chain)))
 where
  chain = reannouncedChain NothingPruned

-- | When the node restarts in 'nodeKeepsRefocusedThroughGc'.
data Restarts
  = -- | Once, after the garbage collection.
    RestartAfterGc
  | -- | Also once it has acquired 'fetchedEb', before it adds
    -- 'gcRestartChain'. So the certificate moves the election onto an
    -- endorser block that the node already held when it started, but whose
    -- body its LeiosTxCache does not hold, so it fetches it again at the
    -- certified announcement, from a peer whose ChainSync headers offer it
    -- there.
    RestartBeforeChainToo
  deriving (Show, Enum, Bounded)

-- | The peer announces 'fetchedEb' in 'announcingFetched', offers it, and
-- serves it. Once the node has acquired it, the node adds 'gcRestartChain',
-- whose certificate moves the second announcement's election onto
-- 'fetchedEb', though nothing announced it to the node. Then the node waits
-- out a garbage collection of its LeiosDB to its immutable tip, which is past
-- 'announcingFetched', and restarts, and also earlier as the argument says.
-- The tip of its selection after the last restart, once that is the chain's
-- last block or ten seconds have passed.
nodeKeepsRefocusedThroughGc :: Restarts -> IOSim s (Point LeiosTestBlock)
nodeKeepsRefocusedThroughGc restarts = do
  nodeDBs <- emptyNodeDBs
  leiosDb <- newLeiosDBInMemory
  withNodeOverDBs nodeDBs leiosDb $ \nut -> do
    next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
    notifications <- newTQueueIO
    atomically $
      mapM_ (writeTQueue notifications) (announcingAndOffering fetchedEb announcingFetched)
    let envPeer =
          EnvPeer
            { epChainSyncServer = quietChainSyncServer
            , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
            , epLeiosNotifyClient = Nothing
            , epLeiosFetchServer = servingFetched nut
            , epLeiosFetchClient = Nothing
            }
    -- Let the slots of the chain's blocks pass, so none is from the future.
    threadDelay 20
    withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
      _ <- replicateM 2 $ atomically next
      case restarts of
        RestartBeforeChainToo -> pure ()
        RestartAfterGc -> do
          addBlocks nut gcRestartChain
          -- The acquisition announced again, at the certified announcement.
          _ <- replicateM 2 $ atomically next
          awaitChainThenGc nut
  case restarts of
    RestartAfterGc -> pure ()
    RestartBeforeChainToo ->
      withNodeOverDBs nodeDBs leiosDb $ \nut -> do
        chain <- newEnvChain gcRestartChain
        withNodeVsEnvConnection
          nut
          (PeerAddr 1)
          ((servingChain nut chain){epLeiosFetchServer = servingFetched nut})
          $ \_conn -> do
            addBlocks nut gcRestartChain
            awaitChainThenGc nut
  withNodeOverDBs nodeDBs leiosDb $ \nut -> awaitTip nut (lastPoint gcRestartChain)
 where
  awaitChainThenGc nut = do
    _ <- awaitTip nut (lastPoint gcRestartChain)
    -- Past the ChainDB's delay before it collects garbage, and its interval.
    threadDelay 80

-- | The peer announces 'fetchedEb' in 'announcingFetched' and again in
-- 'reannouncingFetched', and offers nothing. The node adds a chain of twelve
-- blocks from 'announcingFetched', which puts its immutable tip at
-- 'reannouncingFetched', and prunes. Only then does the peer offer 'fetchedEb',
-- at the second announcement, and serve it. The node's acquisitions, as it
-- emits them, and the notifications it relays to the peer, by 'relayHorizon'
-- after the fourth.
--
-- The node offers 'fetchedEb' only at the second announcement, the point it
-- acquired it at, although it still relays the first announcement, since it
-- relays an announcement for as long as its age allows, whatever its immutable
-- tip.
nodeFetchesAfterPruningEarlier :: IOSim s ([Acquired], [Relayed])
nodeFetchesAfterPruningEarlier = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  listener <- newListener nut
  notifications <- newTQueueIO
  atomically $
    mapM_
      (writeTQueue notifications . MsgLeiosBlockAnnouncement . ltbHeader)
      [announcingFetched, reannouncingFetched]
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Just (lsNotifyClient listener)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Just (lsFetchClient listener)
          }
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar (lsRelayed listener) >>= check . (>= 2) . length
    addBlocks nut $ announcingFetched : take 11 (iterate successorLeiosBlock reannouncingFetched)
    awaitPrunedPast nut (blockSlot announcingFetched)
    atomically $
      mapM_
        (writeTQueue notifications)
        [ MsgLeiosBlockOffer refocusedPoint (encodeEndorserBlockSize fetchedEb)
        , MsgLeiosBlockTxsOffer refocusedPoint
        ]
    (,) <$> replicateM 2 (acquiredOf <$> atomically next) <*> relayedBy listener 4

-- | What the node relays of a rival announcement of 'otherEbHash' and then
-- 'announcingAndOffering' 'fetchedEb' 'announcingFetched', once it acquires
-- 'fetchedEb'.
relayedFetchedAfterRival :: [Relayed]
relayedFetchedAfterRival =
  [ RelayedAnnouncement
  , RelayedAnnouncement
  , RelayedBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
  , RelayedBlockTxsOffer fetchedPoint
  ]

-- | Which of 'announcingFetched' and 'certifyingFetched' the node adds first.
data CertifyingArrival = CertifyingAfterAnnouncing | CertifyingBeforeAnnouncing
  deriving (Show, Enum, Bounded)

-- | The peer announces 'otherEbHash' in 'announcingOther', and then 'fetchedEb'
-- in 'announcingFetched', for the same election, so that the former takes its
-- focus. It offers 'fetchedEb' and serves it. The node then adds
-- 'announcingFetched' and 'certifyingFetched'. Whether 'otherEbHash' had the
-- focus until then, the tip of the node's selection, once that is
-- 'certifyingFetched' or ten seconds have passed, and the notifications the
-- node relays to the peer, by 'relayHorizon' after that.
nodeSelectsCertifiedRival ::
  CertifyingArrival -> IOSim s (Bool, Point LeiosTestBlock, [Relayed])
nodeSelectsCertifiedRival arrival = withNode $ \nut -> do
  listener <- newListener nut
  notifications <- newTQueueIO
  atomically $
    mapM_ (writeTQueue notifications) $
      MsgLeiosBlockAnnouncement (ltbHeader announcingOther)
        : announcingAndOffering fetchedEb announcingFetched
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Just (lsNotifyClient listener)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Just (lsFetchClient listener)
          }
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar (lsRelayed listener) >>= check . (>= 2) . length
    rivalFocused <-
      isFocusedPoint (MkLeiosPoint (blockSlot announcingOther) otherEbHash) <$> readOutstanding nut
    case arrival of
      CertifyingAfterAnnouncing -> addBlocks nut [announcingFetched, certifyingFetched]
      CertifyingBeforeAnnouncing -> do
        addBlocksAfter nut (NotOrigin announcingFetched) [certifyingFetched]
        addBlocks nut [announcingFetched]
    (,,) rivalFocused <$> awaitTip nut (blockPoint certifyingFetched) <*> relayedBy listener 2

-- | The peer announces 'fetchedEb' in 'announcingFetched', but at one byte more
-- than its size. It then offers it at its size, and serves it. The node's
-- acquisitions, as it announces them.
nodeFetchesAtOfferedSize :: IOSim s [Acquired]
nodeFetchesAtOfferedSize = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  notifications <- newTQueueIO
  atomically $
    mapM_ (writeTQueue notifications) $
      announcingAndOffering fetchedEb (misstating announcingFetched)
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn ->
    replicateM 2 $ acquiredOf <$> atomically next

-- | The peer announces 'fetchedEb' at one byte more than its size, in an
-- election of its own, and then in 'announcingFetched'. It offers 'fetchedEb'
-- there, and serves it. The node then adds 'announcingFetched' and
-- 'certifyingFetched'. The tip of the node's selection, once that is
-- 'certifyingFetched' or ten seconds have passed.
nodeSelectsDespiteMisstatedSize :: IOSim s (Point LeiosTestBlock)
nodeSelectsDespiteMisstatedSize = withNode $ \nut -> do
  notifications <- newTQueueIO
  atomically $
    mapM_ (writeTQueue notifications) $
      MsgLeiosBlockAnnouncement (ltbHeader (misstating (successorLeiosBlock (firstLeiosBlock 7))))
        : announcingAndOffering fetchedEb announcingFetched
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    addBlocks nut [announcingFetched, certifyingFetched]
    awaitTip nut (blockPoint certifyingFetched)

-- | The block, announcing 'fetchedEb' at one byte more than its size.
misstating :: LeiosTestBlock -> LeiosTestBlock
misstating =
  setAnnouncement (endorserBlockHash fetchedEb) (encodeEndorserBlockSize fetchedEb + 1)

-- | Property: the node acquires the endorser block of 'evenEb' that a peer
-- announces, offers and serves, as 'expectedBounded' says.
prop_boundedEb :: Property
prop_boundedEb =
  checkCoverage $
    forAll genEvenEb $ \(count, size) ->
      let ebAndTxs@(eb, _txs) = evenEb count size
       in cover
            10
            ( fromIntegral count * size == maxEbBodySize
                && encodeEndorserBlockSize eb <= maxEbBodySize
            )
            "transactions totalling the most, in a body the ledger admits"
            $ ioProperty
            $ do
              observed@(_acquired, failure) <- runSimFinishing (nodeTakesEvenEb ebAndTxs)
              pure $
                tabulate "the client" [maybe "fetched it" (("failed: " <>) . takeWhile (/= ' ')) failure] $
                  observed === expectedBounded eb

-- | How many transactions, and how big each, near where the endorser block's
-- body or the total size of its transactions reaches 'maxEbBodySize', the most
-- the node's ledger admits of either, and half the time totalling exactly that.
genEvenEb :: Gen (Int, BytesSize)
genEvenEb = oneof [near, exact]
 where
  most = fromIntegral maxEbBodySize :: Int
  near = do
    count <- choose (1, 200)
    let fitting = most `div` count
    size <- choose (max 1 (fitting - 2), fitting + 2)
    pure (count, fromIntegral size)
  exact = do
    count <- elements [c | c <- [1 .. 200], most `mod` c == 0]
    pure (count, fromIntegral (most `div` count))

-- | An endorser block of this many transactions, each of this many bytes, and
-- those transactions.
evenEb :: Int -> BytesSize -> (EndorserBlock, V.Vector LeiosTx)
evenEb count size = (mkEndorserBlock [(hashLeiosTx tx, size) | tx <- V.toList txs], txs)
 where
  txs =
    V.fromList
      [MkLeiosTx (BS.replicate (fromIntegral size) (fromIntegral i)) | i <- [0 .. count - 1]]

-- | Where 'nodeTakesEvenEb' announces the endorser block.
evenEbPoint :: EndorserBlock -> LeiosPoint
evenEbPoint eb = MkLeiosPoint 1 (endorserBlockHash eb)

-- | What the node acquires of the endorser block, and why its LeiosFetch client
-- fails, if it does. A body the ledger admits, of transactions the ledger
-- admits, it acquires, and then their closure. A body the ledger does not
-- admit, which a peer can only offer at the most the ledger admits, is not the
-- size offered; a closure the ledger does not admit is too big. Either way,
-- the node acquires nothing of it, and disconnects the peer that served it.
expectedBounded :: EndorserBlock -> ([Acquired], Maybe String)
expectedBounded eb
  | bodySize > maxEbBodySize =
      ([], Just (show (ExnLeiosBlockWrongSize point maxEbBodySize bodySize)))
  | closureSize > fromIntegral maxEbBodySize =
      ([], Just (show (ExnLeiosClosureTooBig point closureSize maxEbBodySize)))
  | otherwise = ([AcquiredBody point, AcquiredClosure point], Nothing)
 where
  point = evenEbPoint eb
  bodySize = encodeEndorserBlockSize eb
  closureSize = sum [fromIntegral size | (_txHash, size) <- endorserBlockReferences eb]

-- | A peer announces the endorser block, offers its body and its closure,
-- offering the body at its size or the most the ledger admits, whichever is
-- smaller, and serves both. What the node announces acquiring by
-- 'relayHorizon', and why its LeiosFetch client failed, if it did by then.
nodeTakesEvenEb :: (EndorserBlock, V.Vector LeiosTx) -> IOSim s ([Acquired], Maybe String)
nodeTakesEvenEb (eb, txs) = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  notifications <- newTQueueIO
  let offered = min maxEbBodySize (encodeEndorserBlockSize eb)
      point = evenEbPoint eb
      announcing = setAnnouncement (endorserBlockHash eb) offered (firstLeiosBlock 0)
      envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = serving eb txs nut
          , epLeiosFetchClient = Nothing
          }
  atomically $
    mapM_
      (writeTQueue notifications)
      [ MsgLeiosBlockAnnouncement (ltbHeader announcing)
      , MsgLeiosBlockOffer point offered
      , MsgLeiosBlockTxsOffer point
      ]
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    client <-
      maybe (throwIO $ userError "the node does not run a LeiosFetch client") pure $
        Map.lookup leiosFetchMiniProtocolNum (connNodeInitiators conn)
    threadDelay relayHorizon
    let drain acquired =
          atomically ((Just . acquiredOf <$> next) `orElse` pure Nothing)
            >>= maybe (pure (reverse acquired)) (drain . (: acquired))
    acquired <- drain []
    failure <- atomically $ (either (Just . show) (const Nothing) <$> client) `orElse` pure Nothing
    pure (acquired, failure)

-- | A chain without certificates, on a fork of its own, longer than
-- 'certifyingFetched''s.
betterChain :: [LeiosTestBlock]
betterChain = take 3 $ iterate successorLeiosBlock (firstLeiosBlock 1)

-- | 'announcingFetched' and 'certifyingFetched', and then enough blocks to make
-- the chain longer than 'betterChain'.
forgottenChain :: [LeiosTestBlock]
forgottenChain = announcingFetched : take 3 (iterate successorLeiosBlock certifyingFetched)

-- | Before a restart, the node adds 'announcingFetched' and
-- 'certifyingFetched', which it parks, since no peer offers 'fetchedEb', and
-- then 'betterChain', which it selects. After the restart, a rival peer
-- announces 'otherEbHash' for the same election, which so takes its focus. Then
-- a holder announces 'fetchedEb', offers it, and serves it, and the node adds
-- 'forgottenChain'.
--
-- Before the restart: how many endorser blocks the ChainDB says are certified,
-- and whether it holds 'certifyingFetched'. After it: whether the ChainDB
-- forgot 'certifyingFetched', how many endorser blocks it then said are
-- certified, the tip of the node's selection, once that is the last block of
-- 'forgottenChain' or ten seconds have passed, and the notifications the node
-- relays to the rival, by 'relayHorizon' after that.
nodeReselectsForgotten ::
  IOSim s ((Int, Bool), (Bool, Int, Point LeiosTestBlock, [Relayed]))
nodeReselectsForgotten = do
  nodeDBs <- emptyNodeDBs
  leiosDb <- newLeiosDBInMemory
  beforeRestart <- withNodeOverDBs nodeDBs leiosDb $ \nut -> do
    -- Let the slots of the chains' blocks pass, so none is from the future.
    threadDelay 20
    addBlocks nut [announcingFetched, certifyingFetched]
    addBlocks nut betterChain
    (,) <$> certifiedCount nut <*> holds nut certifyingFetched
  afterRestart <- withNodeOverDBs nodeDBs leiosDb $ \nut -> do
    forgotten <- not <$> holds nut certifyingFetched
    certifiedAtStart <- certifiedCount nut
    heardByRival <- newListener nut
    rivalNotifications <- newTQueueIO
    holderNotifications <- newTQueueIO
    atomically $ do
      writeTQueue rivalNotifications (MsgLeiosBlockAnnouncement (ltbHeader announcingOther))
      mapM_ (writeTQueue holderNotifications) (announcingAndOffering fetchedEb announcingFetched)
    let rival =
          EnvPeer
            { epChainSyncServer = quietChainSyncServer
            , epLeiosNotifyServer = feedingLeiosNotifyServer nut rivalNotifications
            , epLeiosNotifyClient = Just (lsNotifyClient heardByRival)
            , epLeiosFetchServer = silentLeiosFetchServer nut
            , epLeiosFetchClient = Just (lsFetchClient heardByRival)
            }
        holder =
          EnvPeer
            { epChainSyncServer = quietChainSyncServer
            , epLeiosNotifyServer = feedingLeiosNotifyServer nut holderNotifications
            , epLeiosNotifyClient = Nothing
            , epLeiosFetchServer = servingFetched nut
            , epLeiosFetchClient = Nothing
            }
    withNodeVsEnvConnection nut (PeerAddr 1) rival $ \_rivalConn -> do
      -- The node relays an announcement only once it has taken it in.
      atomically $ readTVar (lsRelayed heardByRival) >>= check . not . null
      withNodeVsEnvConnection nut (PeerAddr 2) holder $ \_holderConn -> do
        addBlocks nut forgottenChain
        (,,,) forgotten certifiedAtStart
          <$> awaitTip nut (lastPoint forgottenChain)
          <*> relayedBy heardByRival 1
  pure (beforeRestart, afterRestart)
 where
  certifiedCount nut = Map.size <$> atomically (ChainDB.getLeiosCertifiedEbs (nutChainDB nut))
  holds nut blk = atomically $ ($ blockPoint blk) <$> ChainDB.getIsFetched (nutChainDB nut)

-- | The holder announces 'fetchedEb', and the node relays the announcement,
-- before the listener connects. Only then does the holder offer 'fetchedEb' and
-- serve it. The notifications the node relays to the listener, by
-- 'relayHorizon' after the third.
nodeAnnouncesToLateListener :: IOSim s [Relayed]
nodeAnnouncesToLateListener = withNode $ \nut -> do
  heardByHolder <- newListener nut
  heardByListener <- newListener nut
  notifications <- newTQueueIO
  atomically $
    writeTQueue notifications (MsgLeiosBlockAnnouncement (ltbHeader announcingFetched))
  let holder =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Just (lsNotifyClient heardByHolder)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Just (lsFetchClient heardByHolder)
          }
      listener =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Just (lsNotifyClient heardByListener)
          , epLeiosFetchServer = silentLeiosFetchServer nut
          , epLeiosFetchClient = Just (lsFetchClient heardByListener)
          }
  withNodeVsEnvConnection nut (PeerAddr 1) holder $ \_holderConn -> do
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar (lsRelayed heardByHolder) >>= check . not . null
    withNodeVsEnvConnection nut (PeerAddr 2) listener $ \_listenerConn -> do
      atomically $
        mapM_
          (writeTQueue notifications)
          [ MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
          , MsgLeiosBlockTxsOffer fetchedPoint
          ]
      relayedBy heardByListener 3

-- | The peer announces 'fetchedEb' and offers its body, and once the node has
-- requested the body, which the peer never sends, offers it again. Why the
-- node's LeiosNotify client failed, if it did.
nodeDisconnectsReofferAfterRequest :: IOSim s (Maybe String)
nodeDisconnectsReofferAfterRequest = withNode $ \nut -> do
  requested <- newTVarIO False
  notifications <- newTQueueIO
  let offer ::
        Message (LeiosNotify LeiosPoint (Header LeiosTestBlock) LeiosVote) LN.StBusy LN.StIdle
      offer = MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
      envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = withholding nut requested
          , epLeiosFetchClient = Nothing
          }
  atomically $
    mapM_ (writeTQueue notifications) [MsgLeiosBlockAnnouncement (ltbHeader announcingFetched), offer]
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    atomically $ readTVar requested >>= check
    atomically $ writeTQueue notifications offer
    client <-
      maybe (throwIO $ userError "the node does not run a LeiosNotify client") pure $
        Map.lookup leiosNotifyMiniProtocolNum (connNodeInitiators conn)
    atomically client <&> \case
      Left e -> Just (show e)
      Right _ -> Nothing

-- | A LeiosFetch server that notes that it was sent a request, and never
-- answers it.
withholding ::
  NodeUnderTest (IOSim s) ->
  StrictTVar (IOSim s) Bool ->
  Channel (IOSim s) ByteString ->
  IOSim s ()
withholding nut requested chan =
  fst
    <$> runAnnotatedPeer nullTracer (envLeiosFetchCodec nut) chan (leiosFetchServerPeer withhold)
 where
  withhold = MkLeiosFetchRequestHandler $ \_request -> do
    atomically $ writeTVar requested True
    atomically retry

-- | Which arrives first: 'honestEb''s closure, which holds 'sharedTx', or the
-- body of 'lyingEb', which names it at the wrong size.
data LieArrival = TxBeforeBody | BodyBeforeTx
  deriving (Show, Enum, Bounded)

-- | A transaction that 'honestEb' and 'lyingEb' both name.
sharedTx :: LeiosTx
sharedTx = MkLeiosTx (BS.replicate 50 4)

-- | Names 'sharedTx' at its size.
honestEb :: EndorserBlock
honestEb = mkEndorserBlock [(hashLeiosTx sharedTx, 50)]

-- | Names 'sharedTx' at one byte.
lyingEb :: EndorserBlock
lyingEb = mkEndorserBlock [(hashLeiosTx sharedTx, 1)]

-- | What the node relays of 'honestEb': its announcement, and offers of its body
-- and its closure.
relayedHonestEb :: [Relayed]
relayedHonestEb =
  [ RelayedAnnouncement
  , RelayedBlockOffer honestPoint (encodeEndorserBlockSize honestEb)
  , RelayedBlockTxsOffer honestPoint
  ]
 where
  honestPoint = MkLeiosPoint (blockSlot announcingHonest) (endorserBlockHash honestEb)

-- | What the node relays of 'lyingEb': its announcement, and an offer of its
-- body, but none of its closure.
relayedLyingEb :: [Relayed]
relayedLyingEb =
  [RelayedAnnouncement, RelayedBlockOffer lyingPoint (encodeEndorserBlockSize lyingEb)]

lyingPoint :: LeiosPoint
lyingPoint = MkLeiosPoint (blockSlot announcingLying) (endorserBlockHash lyingEb)

-- | Announce 'honestEb' and 'lyingEb', in elections of their own.
announcingHonest, announcingLying :: LeiosTestBlock
announcingHonest =
  setAnnouncement
    (endorserBlockHash honestEb)
    (encodeEndorserBlockSize honestEb)
    (successorLeiosBlock (firstLeiosBlock 4))
announcingLying =
  setAnnouncement
    (endorserBlockHash lyingEb)
    (encodeEndorserBlockSize lyingEb)
    (iterate successorLeiosBlock (firstLeiosBlock 5) !! 3)

-- | A listener connects, and then a holder and a liar. The holder announces
-- 'honestEb', offers its body and closure, and serves both. The liar announces
-- 'lyingEb', offers only its body, and serves only that. Each starts once the
-- node has acquired what the other offered, in the order the argument says.
-- The notifications the node relays to the listener, by 'relayHorizon' after
-- the fifth.
nodeOffersLyingEb :: LieArrival -> IOSim s [Relayed]
nodeOffersLyingEb arrival = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  heard <- newListener nut
  holderNotifications <- newTQueueIO
  liarNotifications <- newTQueueIO
  let listener =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Just (lsNotifyClient heard)
          , epLeiosFetchServer = silentLeiosFetchServer nut
          , epLeiosFetchClient = Just (lsFetchClient heard)
          }
      holder =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut holderNotifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = serving honestEb (V.singleton sharedTx) nut
          , epLeiosFetchClient = Nothing
          }
      liar =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut liarNotifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = serving lyingEb V.empty nut
          , epLeiosFetchClient = Nothing
          }
      servesTheTx = do
        atomically $
          mapM_ (writeTQueue holderNotifications) (announcingAndOffering honestEb announcingHonest)
        awaitAcquired next $
          AcquiredClosure (MkLeiosPoint (blockSlot announcingHonest) (endorserBlockHash honestEb))
      servesTheLie = do
        atomically $
          mapM_
            (writeTQueue liarNotifications)
            [ MsgLeiosBlockAnnouncement (ltbHeader announcingLying)
            , MsgLeiosBlockOffer lyingPoint (encodeEndorserBlockSize lyingEb)
            ]
        awaitAcquired next (AcquiredBody lyingPoint)
  -- Let the slots of the announcing blocks pass, so neither is from the future.
  threadDelay 6
  withNodeVsEnvConnection nut (PeerAddr 1) listener $ \_listenerConn ->
    withNodeVsEnvConnection nut (PeerAddr 2) holder $ \_holderConn ->
      withNodeVsEnvConnection nut (PeerAddr 3) liar $ \_liarConn -> do
        case arrival of
          TxBeforeBody -> servesTheTx >> servesTheLie
          BodyBeforeTx -> servesTheLie >> servesTheTx
        relayedBy heard 5

-- | What the listener recorded, by 'relayHorizon' after it recorded this many
-- notifications, with each offer it had not fetched back by then.
relayedBy :: Listener s -> Int -> IOSim s [Relayed]
relayedBy listener n = do
  atomically $ readTVar (lsRelayed listener) >>= check . (>= n) . length
  threadDelay relayHorizon
  atomically $ do
    relays <- readTVar (lsRelayed listener)
    unfetched <- readTVar (lsUnfetched listener)
    pure $ relays <> [RelayedDishonestly ("not served: " <> show offer) | offer <- unfetched]

-- | How long a test goes on recording what the node relays once it has
-- recorded what it expects: the node relays as it takes something in or
-- acquires it, so what a test leads it to relay comes well within this.
relayHorizon :: DiffTime
relayHorizon = 5

-- | Wait until the node announces this acquisition.
awaitAcquired :: STM (IOSim s) LeiosEbNotification -> Acquired -> IOSim s ()
awaitAcquired next acquired = do
  got <- acquiredOf <$> atomically next
  unless (got == acquired) $ awaitAcquired next acquired

-- | The block's announcement of the endorser block, and offers of its body and
-- its closure there, at its size.
announcingAndOffering ::
  EndorserBlock ->
  LeiosTestBlock ->
  [Message (LeiosNotify LeiosPoint (Header LeiosTestBlock) LeiosVote) LN.StBusy LN.StIdle]
announcingAndOffering eb blk =
  [ MsgLeiosBlockAnnouncement (ltbHeader blk)
  , MsgLeiosBlockOffer point (encodeEndorserBlockSize eb)
  , MsgLeiosBlockTxsOffer point
  ]
 where
  point = MkLeiosPoint (blockSlot blk) (endorserBlockHash eb)

-- | The node's LeiosFetch state.
readOutstanding ::
  NodeUnderTest (IOSim s) -> IOSim s (LeiosOutstanding (LeiosPeerLifetimeId (ConnectionId PeerAddr)))
readOutstanding nut = IOLike.readMVar (lfcOutstanding (lfkContext (getLeiosFetchKernel (nutKernel nut))))

-- | Wait until the node has pruned what is older than the slot after it.
awaitPrunedPast :: NodeUnderTest (IOSim s) -> SlotNo -> IOSim s ()
awaitPrunedPast nut slot = do
  outstanding <- readOutstanding nut
  if outstandingPrunedSlot outstanding > slot
    then
      -- The ChainDB prunes its own record as it copies blocks to the
      -- ImmutableDB, which nothing observes, so give it a moment too.
      threadDelay 1
    else threadDelay 0.1 >> awaitPrunedPast nut slot

-- | Wait until the tip of the node's selection is the point, or ten seconds
-- have passed, and return the tip.
awaitTip :: NodeUnderTest (IOSim s) -> Point LeiosTestBlock -> IOSim s (Point LeiosTestBlock)
awaitTip nut point = do
  _ <- timeout 10 $ atomically $ ChainDB.getTipPoint (nutChainDB nut) >>= check . (== point)
  atomically $ ChainDB.getTipPoint (nutChainDB nut)

-- | The point of the chain's last block.
lastPoint :: [LeiosTestBlock] -> Point LeiosTestBlock
lastPoint = maybe GenesisPoint blockPoint . listToMaybe . reverse

acquiredOf :: LeiosEbNotification -> Acquired
acquiredOf = \case
  AcquiredEb point _bodySize -> AcquiredBody point
  AcquiredEbTxs point -> AcquiredClosure point

-- | A peer listening to what the node relays, as a receiving node would.
--
-- It holds each announcement and offer to a receiving node's checks, against
-- an immutable tip and a clock of its own, and fetches each body and closure
-- the node offers back from the node's LeiosFetch server. It records what
-- fails, and what the node has not served by the time a test reads it, as
-- 'RelayedDishonestly'. It ignores votes.
data Listener s = Listener
  { lsRelayed :: StrictTVar (IOSim s) [Relayed]
  -- ^ What the node relayed, in order.
  , lsUnfetched :: StrictTVar (IOSim s) [FetchBack]
  -- ^ The offers not yet fetched back.
  , lsNotifyClient :: Channel (IOSim s) ByteString -> IOSim s ()
  , lsFetchClient :: Channel (IOSim s) ByteString -> IOSim s ()
  }

-- | An offer for a listener to fetch back.
data FetchBack = FetchBackBody LeiosPoint BytesSize | FetchBackClosure LeiosPoint
  deriving (Eq, Show)

-- | A listener whose immutable tip is genesis, and whose clock is the node's.
newListener :: NodeUnderTest (IOSim s) -> IOSim s (Listener s)
newListener nut = newListenerWith nut GenesisPoint 0

-- | A listener with this immutable tip, and with its clock this far ahead of
-- the node's, or behind if negative.
newListenerWith ::
  NodeUnderTest (IOSim s) -> Point LeiosTestBlock -> NominalDiffTime -> IOSim s (Listener s)
newListenerWith nut immTip offset = do
  relayed <- newTVarIO []
  unfetched <- newTVarIO []
  toFetch <- newTQueueIO
  stVar <- newTVarIO emptyLeiosNotifyPeerState
  announceState <- newAnnounceState defaultLeiosAnnouncementAgeLimits =<< NotifyLog.newNotifyLog
  let record msg = do
        st <- pruneLeiosNotifyPeerState (withOrigin 0 id (pointSlot immTip)) <$> readTVarIO stVar
        checked <- case msg of
          MsgLeiosBlockAnnouncement hdr ->
            try @_ @InFutureCheck.HeaderArrivalException
              ( runExceptT $
                  onLeiosNotifyAnnouncement
                    defaultLeiosAnnouncementAgeLimits
                    systemTime
                    (InFutureCheck.realHeaderInFutureCheck InFutureCheck.defaultClockSkew systemTime)
                    cfg
                    announceState
                    (\_valid -> pure ())
                    immLedger
                    st
                    hdr
              )
              <&> either (Left . show) (first show)
          MsgLeiosBlockOffer point size ->
            pure $ first show $ runExcept $ checkLeiosBlockOffer cfg immLedger point size st
          MsgLeiosBlockTxsOffer point ->
            pure $ first show $ runExcept $ checkLeiosClosureOffer point st
          _ -> pure $ Right st
        atomically $ case checked of
          Left why -> modifyTVar relayed (<> [RelayedDishonestly why])
          Right st' -> do
            writeTVar stVar st'
            modifyTVar relayed (<> [relayedOf msg])
            case msg of
              MsgLeiosBlockOffer point size -> fetchBack (FetchBackBody point size)
              MsgLeiosBlockTxsOffer point -> fetchBack (FetchBackClosure point)
              _ -> pure ()
      fetchBack offer = do
        modifyTVar unfetched (<> [offer])
        writeTQueue toFetch offer
      settle offer whys = atomically $ do
        modifyTVar unfetched (List.delete offer)
        modifyTVar relayed (<> map RelayedDishonestly whys)
  pure
    Listener
      { lsRelayed = relayed
      , lsUnfetched = unfetched
      , lsNotifyClient = \chan ->
          fst
            <$> runPipelinedPeerWithLimits
              nullTracer
              (envLeiosNotifyCodec nut)
              (byteLimitsLeiosNotify (fromIntegral . BSL.length))
              timeLimitsLeiosNotify
              chan
              ( toLeiosNotifyClientPeerPipelined $
                  leiosNotifyClientPeerPipelined (pure (Right 1)) (pure record)
              )
      , lsFetchClient = \chan ->
          fst
            <$> runAnnotatedPeer
              nullTracer
              (envLeiosFetchCodec nut)
              chan
              (fetchingBack toFetch settle)
      }
 where
  cfg = nutTopLevelConfig nut

  immLedger =
    (forgetLedgerTables (leiosTestInitExtLedger (configLedger cfg)))
      { ledgerState = LeiosTestLedger immTip
      }

  systemTime = defaultSystemTime (SystemStart (addUTCTime (negate offset) start)) nullTracer
   where
    SystemStart start = getSystemStart (configBlock cfg)

-- | A LeiosFetch client that fetches back each offer from the queue, and
-- settles it with what is wrong with what the node served.
fetchingBack ::
  StrictTQueue (IOSim s) FetchBack -> (FetchBack -> [String] -> IOSim s ()) -> EnvClient s
fetchingBack toFetch settle = Effect $ do
  offer <- atomically $ readTQueue toFetch
  pure $ case offer of
    FetchBackBody point size ->
      requestingBody point $ \eb ->
        settling offer $
          [ "served " <> show (endorserBlockHash eb, encodeEndorserBlockSize eb)
          | endorserBlockHash eb /= pointEbHash point || encodeEndorserBlockSize eb /= size
          ]
    FetchBackClosure point ->
      requestingBody point $ \eb ->
        let refs = endorserBlockReferences eb
         in Yield ReflClientAgency (MsgLeiosBlockTxsRequest point (bitmapsNaming [0 .. length refs - 1])) $
              Await ReflServerAgency $ \case
                MsgLeiosBlockTxs txs ->
                  settling offer $
                    [ "served the closure of " <> show (endorserBlockHash eb)
                    | endorserBlockHash eb /= pointEbHash point
                    ]
                      <> [ "served transactions that do not match the body"
                         | [(hashLeiosTx tx, fromIntegral (BS.length (leiosTxBytes tx))) | tx <- V.toList txs]
                             /= refs
                         ]
 where
  settling offer whys = Effect $ fetchingBack toFetch settle <$ settle offer (map ((show offer <> ": ") <>) whys)

-- | A client that requests the endorser block at the point, and goes on as the
-- function says with the reply.
requestingBody :: LeiosPoint -> (EndorserBlock -> EnvClient s) -> EnvClient s
requestingBody point k =
  Yield ReflClientAgency (MsgLeiosBlockRequest point) $
    Await ReflServerAgency $ \case
      MsgLeiosBlock eb -> k eb

-- | A LeiosNotify server that sends what the queue holds, as the node asks.
feedingLeiosNotifyServer ::
  NodeUnderTest (IOSim s) ->
  StrictTQueue
    (IOSim s)
    (Message (LeiosNotify LeiosPoint (Header LeiosTestBlock) LeiosVote) LN.StBusy LN.StIdle) ->
  Channel (IOSim s) ByteString ->
  IOSim s ()
feedingLeiosNotifyServer nut notifications chan = do
  server <-
    leiosNotifyServerPeerLookahead
      (pure NotExcessiveRequests)
      (readTQueue notifications)
  void $
    runLookaheadFixedSenderPeerWithLimits
      nullTracer
      (envLeiosNotifyCodec nut)
      (byteLimitsLeiosNotify (fromIntegral . BSL.length))
      timeLimitsLeiosNotify
      chan
      server

-- | A LeiosFetch server that serves 'fetchedEb' and its transactions.
servingFetched :: NodeUnderTest (IOSim s) -> Channel (IOSim s) ByteString -> IOSim s ()
servingFetched = serving fetchedEb fetchedTxs

-- | A LeiosFetch server that serves the endorser block and its transactions.
serving ::
  EndorserBlock ->
  V.Vector LeiosTx ->
  NodeUnderTest (IOSim s) ->
  Channel (IOSim s) ByteString ->
  IOSim s ()
serving eb txs nut chan =
  fst
    <$> runAnnotatedPeer nullTracer (envLeiosFetchCodec nut) chan (leiosFetchServerPeer (serveEb eb txs))

-- | A peer that serves the chain over ChainSync, and says nothing over the Leios
-- mini-protocols.
servingChain :: NodeUnderTest (IOSim s) -> EnvChain (IOSim s) -> EnvPeer (IOSim s)
servingChain nut chain =
  EnvPeer
    { epChainSyncServer = chainSyncServerOf chain
    , epLeiosNotifyServer = \_chan -> atomically retry
    , epLeiosNotifyClient = Nothing
    , epLeiosFetchServer = silentLeiosFetchServer nut
    , epLeiosFetchClient = Nothing
    }

-- | Whether the node takes the peer at the address as offering 'fetchedEb'.
offersFetched :: NodeUnderTest (IOSim s) -> PeerAddr -> IOSim s Bool
offersFetched nut addr = IOLike.atomically $ do
  peers <- IOLike.readTVar (lfkPeers (getLeiosFetchKernel (nutKernel nut)))
  case [peer | (MkLeiosPeerLifetimeId connId _, peer) <- Map.toList peers, remoteAddress connId == addr] of
    [] -> pure False
    peer : _ -> Map.member fetchedPoint <$> IOLike.readTVar (lfpOfferings peer)

relayedOf ::
  Message (LeiosNotify LeiosPoint (Header LeiosTestBlock) LeiosVote) LN.StBusy LN.StIdle -> Relayed
relayedOf = \case
  MsgLeiosBlockAnnouncement _hdr -> RelayedAnnouncement
  MsgLeiosBlockOffer point size -> RelayedBlockOffer point size
  MsgLeiosBlockTxsOffer point -> RelayedBlockTxsOffer point
  MsgLeiosVotes _votes -> RelayedVotes
  MsgCanceled -> RelayedVotes

-- | Serve the endorser block and its transactions.
serveEb ::
  Monad m =>
  EndorserBlock -> V.Vector LeiosTx -> LeiosFetchRequestHandler LeiosPoint EndorserBlock LeiosTx m
serveEb eb txs = MkLeiosFetchRequestHandler $ \case
  MsgLeiosBlockRequest _point -> pure (MsgLeiosBlock eb)
  MsgLeiosBlockTxsRequest _point bitmaps ->
    pure $ MsgLeiosBlockTxs $ V.fromList $ mapMaybe (txs V.!?) (bitmapOffsets bitmaps)

{-------------------------------------------------------------------------------
  Listeners whose immutable tip or clock differs from the node's
-------------------------------------------------------------------------------}

-- | What the node relays of 'fetchedEb' at the point: an announcement, and
-- offers of its body and its closure.
relayedFetchedAt :: LeiosPoint -> [Relayed]
relayedFetchedAt point =
  [ RelayedAnnouncement
  , RelayedBlockOffer point (encodeEndorserBlockSize fetchedEb)
  , RelayedBlockTxsOffer point
  ]

-- | A point in slot 5.
immutableTipInSlot5 :: Point LeiosTestBlock
immutableTipInSlot5 = blockPoint $ iterate successorLeiosBlock (firstLeiosBlock 9) !! 4

-- | Announces 'fetchedEb' in slot 15.
announcingLater :: LeiosTestBlock
announcingLater =
  setAnnouncement
    (endorserBlockHash fetchedEb)
    (encodeEndorserBlockSize fetchedEb)
    (iterate successorLeiosBlock (firstLeiosBlock 10) !! 14)

laterPoint :: LeiosPoint
laterPoint = MkLeiosPoint (blockSlot announcingLater) (endorserBlockHash fetchedEb)

-- | Property: a listener hears the announcement of 'fetchedEb' in
-- 'announcingLater', and the offers at its point, if it connects while the
-- announcement is at most a second younger than 'maxAgeSend', and hears nothing
-- if it connects once the announcement is older than that by more than the
-- NotifyLog's prune cadence. Between the two, whether the node's NotifyLog has
-- pruned the announcement yet decides. That holds whatever the listener's
-- immutable tip, and however far its clock is from the node's within the clock
-- skew that the node allows.
prop_listenerReach :: Property
prop_listenerReach =
  withNumTests 50 $
    forAll genReach $ \(immTip, offset, age) ->
      let expected
            | age <= maxAgeSend - 1 = relayedFetchedAt laterPoint
            | otherwise = []
       in ioProperty $ do
            relays <-
              runSimFinishingWithin
                (realToFrac (maxAnnouncementAgeRecv defaultLeiosAnnouncementAgeLimits) + 100)
                (listenerReach immTip offset age (length expected))
            pure $
              tabulate "listener connects" [connects age] $
                tabulate "listener's clock" [clock offset] $
                  relays === expected
 where
  connects age
    | age < 0 = "before the announcement"
    | age <= maxAgeSend - 1 = "while the node relays it"
    | otherwise = "once the node no longer relays it"
  clock offset
    | offset == 0 = "the node's"
    | abs offset == realToFrac (InFutureCheck.unClockSkew InFutureCheck.defaultClockSkew) =
        "at the allowed skew"
    | otherwise = "within the allowed skew"

-- | A listener's immutable tip, how far its clock is ahead of the node's (or
-- behind, if negative), and how old the announcement is when it connects, by
-- the onset of its slot (negative if before that onset). The ages skip the
-- band in which the node's NotifyLog may or may not have pruned it yet.
genReach :: Gen (Point LeiosTestBlock, NominalDiffTime, DiffTime)
genReach = do
  immTip <- elements [GenesisPoint, immutableTipInSlot5]
  offset <- realToFrac <$> oneof [elements [-skew, 0, skew], choose (-skew, skew)]
  age <-
    fromInteger
      <$> oneof
        [ choose (-slot, -1)
        , choose (0, lastRelayed)
        , elements [lastRelayed, firstUnrelayed]
        , choose (firstUnrelayed, 2 * firstUnrelayed)
        ]
  pure (immTip, offset, age)
 where
  skew :: Double
  skew = realToFrac (InFutureCheck.unClockSkew InFutureCheck.defaultClockSkew)
  slot = fromIntegral (unSlotNo (blockSlot announcingLater))
  lastRelayed = round (maxAgeSend - 1)
  firstUnrelayed = round (maxAgeSend + realToFrac NotifyLog.notifyLogPruneCadence + 1)

-- | A listener with this immutable tip, and with its clock this far ahead of
-- the node's, or behind if negative, connects when the announcement of
-- 'fetchedEb' in 'announcingLater' is this old, by the onset of its slot. At
-- that onset, by the node's clock, a holder announces 'fetchedEb' there,
-- offers its body and its closure, serves both, and disconnects once the node
-- has acquired them. The notifications the listener records, by 'relayHorizon'
-- after the given number.
listenerReach :: Point LeiosTestBlock -> NominalDiffTime -> DiffTime -> Int -> IOSim s [Relayed]
listenerReach immTip offset age expected = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  notifications <- newTQueueIO
  listener <- newListenerWith nut immTip offset
  let
    -- A slot's onset is as many seconds into the simulation as its number.
    onset = Time (fromIntegral (unSlotNo (blockSlot announcingLater)))
    waitUntil t = getMonotonicTime >>= threadDelay . diffTime t
    holder = do
      waitUntil onset
      atomically $ mapM_ (writeTQueue notifications) (announcingAndOffering fetchedEb announcingLater)
      withNodeVsEnvConnection nut (PeerAddr 1) (holdingFetched nut notifications) $ \_holderConn ->
        replicateM_ 2 $ atomically next
  withAsync holder $ \_ -> do
    waitUntil (addTime age onset)
    withNodeVsEnvConnection nut (PeerAddr 2) (listening nut listener) $ \_listenerConn ->
      relayedBy listener expected

-- | How old an announcement the node relays.
maxAgeSend :: DiffTime
maxAgeSend = realToFrac (maxAnnouncementAgeSend defaultLeiosAnnouncementAgeLimits)

-- | A peer that sends the queue's notifications to the node, and serves
-- 'fetchedEb'.
holdingFetched ::
  NodeUnderTest (IOSim s) ->
  StrictTQueue
    (IOSim s)
    (Message (LeiosNotify LeiosPoint (Header LeiosTestBlock) LeiosVote) LN.StBusy LN.StIdle) ->
  EnvPeer (IOSim s)
holdingFetched nut notifications =
  EnvPeer
    { epChainSyncServer = quietChainSyncServer
    , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
    , epLeiosNotifyClient = Nothing
    , epLeiosFetchServer = servingFetched nut
    , epLeiosFetchClient = Nothing
    }

-- | A peer that only listens.
listening :: NodeUnderTest (IOSim s) -> Listener s -> EnvPeer (IOSim s)
listening nut listener =
  EnvPeer
    { epChainSyncServer = quietChainSyncServer
    , epLeiosNotifyServer = \_chan -> atomically retry
    , epLeiosNotifyClient = Just (lsNotifyClient listener)
    , epLeiosFetchServer = silentLeiosFetchServer nut
    , epLeiosFetchClient = Just (lsFetchClient listener)
    }

-- | Announces 'fetchedEb' again, in an election after that of
-- 'reannouncingFetched'.
reannouncingFetchedAgain :: LeiosTestBlock
reannouncingFetchedAgain =
  setAnnouncement
    (endorserBlockHash fetchedEb)
    (encodeEndorserBlockSize fetchedEb)
    (successorLeiosBlock reannouncingFetched)

refocusedAgainPoint :: LeiosPoint
refocusedAgainPoint =
  MkLeiosPoint (blockSlot reannouncingFetchedAgain) (endorserBlockHash fetchedEb)

{-------------------------------------------------------------------------------
  The node's server
-------------------------------------------------------------------------------}

data ServerOutcome = ServerEnded | ServerFailed String
  deriving (Eq, Show)

type EnvClient s =
  Peer (LeiosFetch LeiosPoint EndorserBlock LeiosTx) AsClient NonPipelined StIdle (IOSim s) ()

-- | Run the node's server, with 'heldEb' in its LeiosDB, against a peer that
-- runs this client.
nodeServerAgainst :: EnvClient s -> IOSim s ServerOutcome
nodeServerAgainst client = withNode $ \nut -> do
  holdHeldEb nut
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = refusingLeiosFetchServer nut
          , epLeiosFetchClient = Just $ runEnvClient nut client
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    server <- nodeLeiosFetchServer conn
    atomically server >>= \case
      Right () -> pure ServerEnded
      Left e -> pure $ ServerFailed (show e)

runEnvClient :: NodeUnderTest (IOSim s) -> EnvClient s -> Channel (IOSim s) ByteString -> IOSim s ()
runEnvClient nut client chan = fst <$> runAnnotatedPeer nullTracer (envLeiosFetchCodec nut) chan client

doneClient :: EnvClient s
doneClient = Yield ReflClientAgency MsgDone $ Done ReflNobodyAgency ()

data Reply = ReplyBlock EndorserBlock | ReplyTxs [LeiosTx]
  deriving (Eq, Show)

-- | A request the environment's client sends the node's server.
data Request
  = RequestBlock !LeiosPoint
  | -- | at these offsets
    RequestTxs !LeiosPoint ![Int]
  deriving Show

-- | Property: the node's server answers each request, in order, as
-- 'expectedAnswers' says.
prop_serverAnswers :: Property
prop_serverAnswers =
  forAll genRequests $ \requests -> ioProperty $ do
    answers@(replies, outcome) <- runSimFinishing (nodeAnswers requests)
    pure $
      tabulate "how the server ended" [ending outcome] $
        tabulate "replies before then" [show (length replies)] $
          answers === expectedAnswers requests
 where
  ending = \case
    ServerEnded -> "ended"
    ServerFailed e -> "failed: " <> takeWhile (/= ' ') e

-- | Up to three requests for 'heldEb', at 'heldPoint' or at 'unknownPoint':
-- its body, or transactions at offsets up to well past its end.
genRequests :: Gen [Request]
genRequests = do
  n <- choose (0, 3)
  replicateM n $ do
    point <- elements [heldPoint, unknownPoint]
    -- Often only offsets the node holds at 'heldPoint'.
    highest <- elements [last heldOffsets, last heldOffsets + 1, 130]
    oneof
      [ pure (RequestBlock point)
      , RequestTxs point <$> (sublistOf [0 .. highest] `suchThat` (not . null))
      ]

-- | What the node's server replies to these requests, in order, and how it
-- ends. It serves 'heldEb' and its 'heldOffsets' at 'heldPoint', and
-- disconnects the client at its first request for anything else.
expectedAnswers :: [Request] -> ([Reply], ServerOutcome)
expectedAnswers = go []
 where
  go replies = \case
    [] -> (reverse replies, ServerEnded)
    RequestBlock point : rest
      | point == heldPoint -> go (ReplyBlock heldEb : replies) rest
      | otherwise -> (reverse replies, ServerFailed (show (ExnLeiosUnknownBlockRequested point)))
    RequestTxs point offsets : rest
      | null missing -> go (ReplyTxs (map heldTx ascending) : replies) rest
      | otherwise -> (reverse replies, ServerFailed (show (ExnLeiosUnknownTxsRequested point missing)))
     where
      ascending = List.nub (List.sort offsets)
      missing = [offset | offset <- ascending, point /= heldPoint || offset `notElem` heldOffsets]

-- | The node's replies to these requests, in order, and how its server ended.
nodeAnswers :: [Request] -> IOSim s ([Reply], ServerOutcome)
nodeAnswers requests = do
  repliesVar <- newTVarIO []
  outcome <- nodeServerAgainst (requestingClient requests repliesVar)
  replies <- readTVarIO repliesVar
  pure (reverse replies, outcome)

-- | A client that sends these requests one at a time, keeps the replies,
-- newest first, and then sends 'MsgDone'.
requestingClient :: [Request] -> StrictTVar (IOSim s) [Reply] -> EnvClient s
requestingClient requests repliesVar = case requests of
  [] -> doneClient
  RequestBlock point : rest ->
    Yield ReflClientAgency (MsgLeiosBlockRequest point) $
      Await ReflServerAgency $ \case
        MsgLeiosBlock eb -> keep (ReplyBlock eb) rest
  RequestTxs point offsets : rest ->
    Yield ReflClientAgency (MsgLeiosBlockTxsRequest point (bitmapsNaming offsets)) $
      Await ReflServerAgency $ \case
        MsgLeiosBlockTxs txs -> keep (ReplyTxs (V.toList txs)) rest
 where
  keep reply rest =
    Effect $ requestingClient rest repliesVar <$ atomically (modifyTVar repliesVar (reply :))

-- | The bitmaps that name these offsets.
bitmapsNaming :: [Int] -> TxBitmaps
bitmapsNaming offsets =
  Map.toList $
    Map.fromListWith
      (.|.)
      [(fromIntegral (offset `div` 64), Bits.bit (63 - offset `mod` 64)) | offset <- offsets]

{-------------------------------------------------------------------------------
  What the node's LeiosDB holds
-------------------------------------------------------------------------------}

-- | An endorser block of 70 transactions, so that a request for them takes two
-- bitmaps.
heldEb :: EndorserBlock
heldEb =
  mkEndorserBlock $
    [(heldTxHash i, fromIntegral (BS.length (heldTxBytes i))) | i <- [0 .. 69]]

heldPoint :: LeiosPoint
heldPoint = MkLeiosPoint 1 (UnsafeHash (SBS.replicate 32 1))

unknownPoint :: LeiosPoint
unknownPoint = MkLeiosPoint 1 (UnsafeHash (SBS.replicate 32 2))

heldTxHash :: Int -> TxHash
heldTxHash i = UnsafeHash (SBS.replicate 31 0 <> SBS.singleton (fromIntegral i))

heldTxBytes :: Int -> BS.ByteString
heldTxBytes i = BS.replicate (10 + i) (fromIntegral i)

heldTx :: Int -> LeiosTx
heldTx = MkLeiosTx . heldTxBytes

-- | The offsets of the transactions of 'heldEb' that 'holdHeldEb' writes: all
-- but the last.
heldOffsets :: [Int]
heldOffsets = [0 .. 68]

-- | Write 'heldEb' and its 'heldOffsets'.
holdHeldEb :: NodeUnderTest (IOSim s) -> IOSim s ()
holdHeldEb nut = withWriter (getLeiosDB (nutKernel nut)) $ \writer -> do
  _ <- await =<< writeEbBody writer heldPoint heldEb []
  _ <- await =<< writeTxs writer heldPoint [(i, heldTxBytes i) | i <- heldOffsets]
  pure ()

{-------------------------------------------------------------------------------
  Registration
-------------------------------------------------------------------------------}

-- | Whether the node's initiator bundle, and its initiator-and-responder bundle,
-- include LeiosFetch among their hot mini-protocols for this version.
nodeRunsLeiosFetch :: NodeToNodeVersion -> IOSim s (Bool, Bool)
nodeRunsLeiosFetch ntnVersion = withNode $ \nut ->
  pure
    ( runsLeiosFetch $
        NTN.initiator mempty defaultMiniProtocolParameters ntnVersion versionData (nutApps nut ())
    , runsLeiosFetch $
        NTN.initiatorAndResponder
          mempty
          defaultMiniProtocolParameters
          ntnVersion
          versionData
          (nutApps nut ())
    )
 where
  versionData = nodeVersionData ntnVersion
  runsLeiosFetch bundle = case withHot bundle of
    WithHot protocols -> leiosFetchMiniProtocolNum `elem` map miniProtocolNum protocols

{-------------------------------------------------------------------------------
  The node, connected to peers run by the test
-------------------------------------------------------------------------------}

-- | The node's LeiosFetch server on the connection.
nodeLeiosFetchServer ::
  NodeVsEnvConnection (IOSim s) -> IOSim s (STM (IOSim s) (Either SomeException ()))
nodeLeiosFetchServer conn =
  maybe (throwIO $ userError "the node does not run a LeiosFetch server") pure $
    Map.lookup leiosFetchMiniProtocolNum (connNodeResponders conn)
