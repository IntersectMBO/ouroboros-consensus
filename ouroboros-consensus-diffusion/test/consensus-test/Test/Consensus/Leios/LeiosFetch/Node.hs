{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests of the node's LeiosFetch client and server, over real connections,
-- against peers run by the Leios node-vs-environment tests.
module Test.Consensus.Leios.LeiosFetch.Node (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Network.NodeToNode (defaultMiniProtocolParameters)
import Cardano.Network.NodeToNode.Version
  ( NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , mkLeiosFetchRequestLimit
  )
import qualified Codec.CBOR.Read as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM, forM_, replicateM, replicateM_, unless, void, when)
import Control.Monad.Class.MonadAsync (wait, withAsync)
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
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Time (NominalDiffTime, addUTCTime)
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import Network.TypedProtocol.Codec (AnnotatedCodec, PeerRole (..))
import Network.TypedProtocol.Core (IsPipelined (..), ReflRelativeAgency (..))
import Network.TypedProtocol.Peer (Peer (..))
import qualified Network.TypedProtocol.Peer.Client as AtomicPeer
import Ouroboros.Consensus.Block
  ( Header
  , Point
  , Proxy (..)
  , SlotNo (..)
  , blockHash
  , blockPoint
  , blockSlot
  , pointSlot
  , withOrigin
  , pattern GenesisPoint
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Default (defaultSystemTime)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (SystemStart))
import Ouroboros.Consensus.Config (configBlock, configLedger)
import Ouroboros.Consensus.Config.SupportsNode (getSystemStart)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Ouroboros.Consensus.Leios.Acquisitions
  ( LeiosAcquisitions (..)
  , LeiosEbNotification (..)
  )
import Ouroboros.Consensus.Leios.AnnounceState (newAnnounceState)
import qualified Ouroboros.Consensus.Leios.NotifyLog as NotifyLog
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , LeiosVote
  , TxHash
  , decodeEndorserBlock
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeEndorserBlock
  , encodeEndorserBlockSize
  , encodeLeiosPoint
  , encodeLeiosTx
  , endorserBlockBytes
  , endorserBlockHash
  , endorserBlockReferences
  , hashLeiosTx
  , mkEndorserBlock
  , toRankingBlockHash
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Acquisition as AtomicAcquisition
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission as Admission
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Body as BodyPolicy
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Client as AtomicClient
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Closure as ClosurePolicy
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline as Deadline
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor (PayloadBytes (..))
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as Handoff
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Node
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Receive as AtomicReceive
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Retry as Retry
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server as AtomicServer
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server.State as AtomicServerState
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type as Atomic
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Wait as FetchWait
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( ExnLeiosFetchInvalidReply (..)
  , LeiosFetchClientContext (..)
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosOutstanding (..)
  , OfferedHalves (..)
  , PeerOffer (..)
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (WireOrdinal (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
  ( LeiosFetchExtension (..)
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
import Ouroboros.Consensus.Network.LeiosFetch
  ( leiosFetchMiniProtocolNum
  , runLookaheadPeerWithAnnotatedLimits
  )
import qualified Ouroboros.Consensus.Network.LeiosFetch as FetchTransport
import Ouroboros.Consensus.Network.LeiosNotify
  ( leiosNotifyMiniProtocolNum
  , runLookaheadFixedSenderPeerWithLimits
  )
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Network.NodeToNode.LeiosFetch (withLeiosFetchAtomicServer)
import qualified Ouroboros.Consensus.Network.NodeToNode.LeiosFetch as FetchAssembly
import Ouroboros.Consensus.Node.RethrowPolicy (consensusRethrowPolicy)
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LeiosDB (newLeiosDBInMemory)
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , withReader
  , withWriter
  )
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import Ouroboros.Network.Channel (Channel (..))
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.Driver.Limits (runPipelinedPeerWithLimits)
import Ouroboros.Network.Driver.Simple (runAnnotatedPeer)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , MiniProtocolLimits (..)
  , TemperatureBundle (..)
  , WithProtocolTemperature (..)
  )
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import Ouroboros.Network.RethrowPolicy (ErrorContext (..), RethrowPolicy (..))
import Test.Consensus.Leios.Committee (certOf)
import Test.Consensus.Leios.Environment
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
  ( LedgerState (..)
  , LeiosTestBlock (..)
  , firstLeiosBlock
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

tests :: TestTree
tests =
  testGroup
    "node vs environment"
    [ testProperty
        "the node's server serves what it holds, disconnects a client that requests anything else, and ends when the client sends MsgDone"
        prop_serverAnswers
    , testCase "the node records no offer older than the slot it pruned its peers' offers to" $
        runSimFinishing nodeIgnoresPrunedOffer >>= (@?= [SlotNo 5])
    , testCase
        "the node fetches an endorser block a later election announced, once it prunes the earlier announcement"
        $ runSimFinishing nodeFetchesAfterPruningEarlier
          >>= ( @?=
                  ( [AcquiredBody refocusedPoint, AcquiredClosure refocusedPoint]
                  , RelayedAnnouncement : relayedFetchedAt refocusedPoint
                  )
              )
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
        "the node relays an announcement to a peer that connects after it, before offering that peer the endorser block"
        $ runSimFinishing nodeAnnouncesToLateListener
          >>= ( @?=
                  relayedFetchedAt fetchedPoint
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
    , testGroup
        "atomic node integration"
        [ testCase "normal default assembly negotiates atomic fetch with its matching mux budget" $
            runSimFinishing (atomicDefaultActivation False) >>= (@?= True)
        , testCase "a response-budget override also resizes the negotiated atomic mux" $
            runSimFinishing (atomicDefaultActivation True) >>= (@?= True)
        , testCase "offered work reclaims an empty unoffered acquisition without waiting for pruning" $
            runSimFinishing atomicReclaimsEmpty >>= (@?= True)
        , testCase "arena pressure preserves an in-flight body and its original generation" $
            runSimFinishing atomicPreservesWanted >>= (@?= True)
        , testCase "arena pressure preserves a verified body awaiting its durable receipt" $
            runSimFinishing atomicPreservesAccepted >>= (@?= True)
        , testCase "version 17 fetches the body before any closure offer, then pipelines durable checkpoints" $
            runSimFinishing (atomicHonest 0) >>= (@?= (1, 3))
        , testCase "local durable possession removes only already held TXs" $
            runSimFinishing (atomicHonest 1) >>= (@?= (1, 2))
        , testCase "a locally held closure completes after the independent body with no TX request" $
            runSimFinishing (atomicHonest 3) >>= (@?= (1, 0))
        , testCase "a previously held point is neither fetched nor notified again" $
            runSimFinishing atomicAlreadyHeld >>= (@?= (0, 0))
        , testCase "new acquisition relays durable body and closure offers to a legacy downstream" $
            runSimFinishing atomicRelaysDurably >>= (@?= relayedFetchedAt fetchedPoint)
        , testCase "an earlier body offer cannot backdate the same peer's later closure offer" $
            runSimFinishing atomicOfferOrder >>= (@?= (0, 3))
        , testCase "the new server serves real body bytes and the exact requested transaction vector" $
            runSimFinishing atomicNodeServes >>= (@?= True)
        , testCase "a valid understated body remains durable after the peer fault" $
            runSimFinishing atomicMisstatement >>= (@?= True)
        , testCase "legacy body receipts feed new-version checkpoint acquisition" $
            runSimFinishing (atomicMixed True) >>= (@?= (0, 3))
        , testCase "legacy whole-vector receipts finish a new-version body without duplicate notifications" $
            runSimFinishing (atomicMixed False) >>= (@?= (1, 0))
        , testCase "three EBs share one connection and reuse the two-acquisition capacity" $
            runSimFinishing atomicConcurrentEbs >>= (@?= (3, 7))
        , testCase
            "peer protocol/data faults and deliberate removal stay peer-local; storage receipt invariants are fatal"
            $ do
              let classify e = show $ runRethrowPolicy (consensusRethrowPolicy (Proxy @Blk)) OutboundError e
              mapM_
                ((@?= "ShutdownPeer") . classify)
                [ toException AtomicClient.ClientBodySizeMisstated
                , toException AtomicClient.ClientClosed
                , toException AtomicReceive.ReceiveWrongCount
                , toException (AtomicReceive.ReceiveStorageError Handoff.InvalidTransaction)
                , toException AtomicServerState.ServerDataCapacity
                , toException
                    (IOLike.ExceptionInLinkedThread "test" (toException AtomicClient.ClientBodySizeMisstated))
                ]
              classify (toException (Handoff.StorageException Handoff.InvalidDescriptor)) @?= "ShutdownNode"
        ]
    , testGroup
        "shared fetch coordinator"
        [ testCase "atomic ordinary requests start promptly after body durability" $
            runSimFinishing (coordinatorAssignment True) >>= (@?= True)
        , testCase "fast wakes preserve the half-second legacy scheduling gate" $
            runSimFinishing (coordinatorAssignment False) >>= (@?= True)
        , testCase "readiness flooding cannot starve a body trigger deadline" $
            runSimFinishing coordinatorReadinessFlood >>= (@?= True)
        , testCase "an old atomic finalizer cannot close a replacement invocation" $
            runSimFinishing (coordinatorClientCleanup False) >>= (@?= True)
        , testCase "an old legacy finalizer cannot close a replacement atomic invocation" $
            runSimFinishing (coordinatorClientCleanup True) >>= (@?= True)
        , testCase "explicit focus loss frees the arena and refocus fences a committed old body" $
            runSimFinishing coordinatorFocusRefocus >>= (@?= True)
        , testCase "immutable-tip pruning frees the old acquisition in a single-arena atomic node" $
            runSimFinishing coordinatorPrunes >>= (@?= True)
        , testCase "same-address reconnect retains a writing claim until durable receipt" $
            runSimFinishing coordinatorReconnectStorage >>= (@?= True)
        , testCase "node shutdown drains a disconnected peer's accepted storage" $
            runSimFinishing coordinatorShutdownStorage >>= (@?= True)
        ]
    , testGroup
        "closure rescue over connections"
        [ testCase "a later full offer rescues a silent primary and takes over its queued tail" $
            runSimFinishing (closureRecovery False) >>= (@?= True)
        , testCase "a verified prefix survives a committed partial response and only missing TXs are rescued" $
            runSimFinishing (closureRecovery True) >>= (@?= True)
        , testCase "a sole slow closure primary persists and completes after expiry" $
            runSimFinishing closureSoleSource >>= (@?= True)
        , testCase "a failed rescue rotates obligations FIFO before retrying the original head" $
            runSimFinishing closureRescueRotation >>= (@?= True)
        , testCase
            "recycled rescue attempts snapshot backoff, cool down after five failures and recover on useful TXs"
            $ runSimFinishing closureRetryCycles >>= (@?= True)
        , testCase "another EB's body and ordinary closure continue during retry cooldown" $
            runSimFinishing closureCooldownIndependent >>= (@?= True)
        ]
    , testGroup
        "body hedging over connections"
        [ testCase "a sole slow primary completes without cancellation or restart" $
            runSimFinishing bodySoleSource >>= (@?= (1, 1, 0))
        , testCase "a new alternative can hedge a retained primary after its trigger fired" $
            runSimFinishing (bodyNewAlternative False) >>= (@?= (2, 1, 0))
        , testCase "a valid understated hedge body wins before its peer is disconnected" $
            runSimFinishing (bodyNewAlternative True) >>= (@?= (2, 1, 0))
        , testCase "round-robin alternatives share cumulative backoff and recycle only after wire drain" $
            runSimFinishing bodyRoundRobin >>= (@?= True)
        , testCase
            "concurrent EBs each get a hedge even when the second primary waits behind a blocked FIFO head"
            $ runSimFinishing bodyConcurrentEbs >>= (@?= True)
        , testCase
            "a body queued behind another EB's stalled TX reply is rescued without starting its own timer"
            $ runSimFinishing bodyBehindTransactions >>= (@?= True)
        , testCase "a late persistent primary wins and cancels its hedge" $
            runSimFinishing (bodyLateWinner True) >>= (@?= True)
        , testCase "a retiring committed hedge can win; original and ACK debt retire independently" $
            runSimFinishing (bodyLateWinner False) >>= (@?= True)
        ]
    , testCase "the node runs LeiosFetch from NodeToNodeV_16 on, and not before" $
        forM_ [minBound .. NodeToNodeV_16] $ \ntnVersion -> do
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

-- | What the node emitted as acquired, comparably.
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

-- | The successor of 'announcingFetched', which certifies 'fetchedEb'.
certifyingFetched :: LeiosTestBlock
certifyingFetched = certifying announcingFetched

-- | The successor of the block, containing a certificate of what it announced.
certifying :: LeiosTestBlock -> LeiosTestBlock
certifying announcing =
  setLeiosCert
    (certOf (toRankingBlockHash (Proxy @LeiosTestBlock) (blockHash announcing)) [0, 1, 2])
    (successorLeiosBlock announcing)

-- | Once the node has pruned its peers' offers to a slot, it records only the
-- offers at or after that slot. The slots of the offers it records.
nodeIgnoresPrunedOffer :: IOSim s [SlotNo]
nodeIgnoresPrunedOffer = withNode $ \node -> do
  let kernel = getLeiosFetchKernel (nodeKernel node)
      offer =
        MkPeerOffer
          { poEbBounds = SNothing
          , poOffered = OfferedClosure
          }
      peer = ConnectionId (PeerAddr 0) (PeerAddr 1)
  IOLike.atomically $ IOLike.writeTVar (lfkPrunedSlot kernel) (SlotNo 5)
  withLeiosFetchPeer kernel peer IsNotBigLedgerPeer $ \_lifetimeId peerVars -> do
    forM_ [SlotNo 4, SlotNo 5] $ \slot ->
      recordLeiosOffer kernel peerVars (MkLeiosPoint slot (endorserBlockHash fetchedEb)) offer
    map pointSlotNo . Map.keys <$> IOLike.readTVarIO (lfpOfferings peerVars)

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
nodeFetchesAfterPruningEarlier = withNode $ \node -> do
  next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
  listener <- newListener node
  notifications <- newTQueueIO
  atomically $
    mapM_
      (writeTQueue notifications . MsgLeiosBlockAnnouncement . ltbHeader)
      [announcingFetched, reannouncingFetched]
  let envPeer =
        holdingFetchedListening node notifications listener
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  withNodeVsEnvConnection node (PeerAddr 1) envPeer $ \_conn -> do
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar (lsRelayed listener) >>= check . (>= 2) . length
    addBlocks node $ announcingFetched : take 11 (iterate successorLeiosBlock reannouncingFetched)
    awaitPrunedPast node (blockSlot announcingFetched)
    atomically $
      mapM_
        (writeTQueue notifications)
        [ MsgLeiosBlockOffer refocusedPoint (encodeEndorserBlockSize fetchedEb)
        , MsgLeiosBlockTxsOffer refocusedPoint
        ]
    (,) <$> replicateM 2 (acquiredOf <$> atomically next) <*> relayedBy listener 4

-- | Which of 'announcingFetched' and 'certifyingFetched' the node adds first.
data CertifyingArrival = CertifyingAfterAnnouncing | CertifyingBeforeAnnouncing
  deriving (Show, Enum, Bounded)

-- | The peer announces 'fetchedEb' in 'announcingFetched', but at one byte more
-- than its size. It then offers it at its size, and serves it. The node's
-- acquisitions, as it emits them.
nodeFetchesAtOfferedSize :: IOSim s [Acquired]
nodeFetchesAtOfferedSize = withNode $ \node -> do
  next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
  notifications <- newTQueueIO
  atomically $
    mapM_ (writeTQueue notifications) $
      announcingAndOffering fetchedEb (misstating announcingFetched)
  let envPeer =
        EnvPeer
          { epLeiosNotifyServer = feedingLeiosNotifyServer node notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = servingFetched node
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection node (PeerAddr 1) envPeer $ \_conn ->
    replicateM 2 $ acquiredOf <$> atomically next

-- | The peer announces 'fetchedEb' at one byte more than its size, in an
-- election of its own, and then in 'announcingFetched'. It offers 'fetchedEb'
-- there, and serves it. The node then adds 'announcingFetched' and
-- 'certifyingFetched'. The tip of the node's selection, once that is
-- 'certifyingFetched' or ten seconds have passed.
nodeSelectsDespiteMisstatedSize :: IOSim s (Point LeiosTestBlock)
nodeSelectsDespiteMisstatedSize = withNode $ \node -> do
  notifications <- newTQueueIO
  atomically $
    mapM_ (writeTQueue notifications) $
      MsgLeiosBlockAnnouncement (ltbHeader (misstating (successorLeiosBlock (firstLeiosBlock 7))))
        : announcingAndOffering fetchedEb announcingFetched
  let envPeer =
        EnvPeer
          { epLeiosNotifyServer = feedingLeiosNotifyServer node notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = servingFetched node
          , epLeiosFetchClient = Nothing
          }
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  withNodeVsEnvConnection node (PeerAddr 1) envPeer $ \_conn -> do
    addBlocks node [announcingFetched, certifyingFetched]
    awaitTip node (blockPoint certifyingFetched)

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
-- smaller, and serves both. What the node emits as acquired by
-- 'relayHorizon', and why its LeiosFetch client failed, if it did by then.
nodeTakesEvenEb :: (EndorserBlock, V.Vector LeiosTx) -> IOSim s ([Acquired], Maybe String)
nodeTakesEvenEb (eb, txs) = withNode $ \node -> do
  next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
  notifications <- newTQueueIO
  let offered = min maxEbBodySize (encodeEndorserBlockSize eb)
      point = evenEbPoint eb
      announcing = setAnnouncement (endorserBlockHash eb) offered (firstLeiosBlock 0)
      envPeer =
        EnvPeer
          { epLeiosNotifyServer = feedingLeiosNotifyServer node notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = serving eb txs node
          , epLeiosFetchClient = Nothing
          }
  atomically $
    mapM_
      (writeTQueue notifications)
      [ MsgLeiosBlockAnnouncement (ltbHeader announcing)
      , MsgLeiosBlockOffer point offered
      , MsgLeiosBlockTxsOffer point
      ]
  withNodeVsEnvConnection node (PeerAddr 1) envPeer $ \conn -> do
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

-- | The holder announces 'fetchedEb', and the node relays the announcement,
-- before the listener connects. Only then does the holder offer 'fetchedEb' and
-- serve it. The notifications the node relays to the listener, by
-- 'relayHorizon' after the third.
nodeAnnouncesToLateListener :: IOSim s [Relayed]
nodeAnnouncesToLateListener = withNode $ \node -> do
  heardByHolder <- newListener node
  heardByListener <- newListener node
  notifications <- newTQueueIO
  atomically $
    writeTQueue notifications (MsgLeiosBlockAnnouncement (ltbHeader announcingFetched))
  let holder =
        holdingFetchedListening node notifications heardByHolder
      listener =
        listening node heardByListener
  withNodeVsEnvConnection node (PeerAddr 1) holder $ \_holderConn -> do
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar (lsRelayed heardByHolder) >>= check . not . null
    withNodeVsEnvConnection node (PeerAddr 2) listener $ \_listenerConn -> do
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
nodeDisconnectsReofferAfterRequest = withNode $ \node -> do
  requested <- newTVarIO False
  notifications <- newTQueueIO
  let offer ::
        Message (LeiosNotify LeiosPoint (Header LeiosTestBlock) LeiosVote) LN.StBusy LN.StIdle
      offer = MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
      envPeer =
        EnvPeer
          { epLeiosNotifyServer = feedingLeiosNotifyServer node notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = withholding node requested
          , epLeiosFetchClient = Nothing
          }
  atomically $
    mapM_ (writeTQueue notifications) [MsgLeiosBlockAnnouncement (ltbHeader announcingFetched), offer]
  withNodeVsEnvConnection node (PeerAddr 1) envPeer $ \conn -> do
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
withholding node requested chan =
  fst
    <$> runAnnotatedPeer nullTracer (envLeiosFetchCodec node) chan (leiosFetchServerPeer withhold)
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
nodeOffersLyingEb arrival = withNode $ \node -> do
  next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
  heard <- newListener node
  holderNotifications <- newTQueueIO
  liarNotifications <- newTQueueIO
  let listener =
        listening node heard
      holder =
        EnvPeer
          { epLeiosNotifyServer = feedingLeiosNotifyServer node holderNotifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = serving honestEb (V.singleton sharedTx) node
          , epLeiosFetchClient = Nothing
          }
      liar =
        EnvPeer
          { epLeiosNotifyServer = feedingLeiosNotifyServer node liarNotifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = serving lyingEb V.empty node
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
  withNodeVsEnvConnection node (PeerAddr 1) listener $ \_listenerConn ->
    withNodeVsEnvConnection node (PeerAddr 2) holder $ \_holderConn ->
      withNodeVsEnvConnection node (PeerAddr 3) liar $ \_liarConn -> do
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

-- | Wait until the node emits this acquisition.
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
readOutstanding node = IOLike.readMVar (lfcOutstanding (lfkContext (getLeiosFetchKernel (nodeKernel node))))

-- | Wait until the node has pruned what is older than the slot after it.
awaitPrunedPast :: NodeUnderTest (IOSim s) -> SlotNo -> IOSim s ()
awaitPrunedPast node slot = do
  outstanding <- readOutstanding node
  if outstandingPrunedSlot outstanding > slot
    then
      -- The ChainDB prunes its own record as it copies blocks to the
      -- ImmutableDB, which nothing observes, so give it a moment too.
      threadDelay 1
    else threadDelay 0.1 >> awaitPrunedPast node slot

-- | Wait until the tip of the node's selection is the point, or ten seconds
-- have passed, and return the tip.
awaitTip :: NodeUnderTest (IOSim s) -> Point LeiosTestBlock -> IOSim s (Point LeiosTestBlock)
awaitTip node point = do
  _ <- timeout 10 $ atomically $ ChainDB.getTipPoint (nodeChainDB node) >>= check . (== point)
  atomically $ ChainDB.getTipPoint (nodeChainDB node)

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
newListener node = newListenerWith node GenesisPoint 0

-- | A listener with this immutable tip, and with its clock this far ahead of
-- the node's, or behind if negative.
newListenerWith ::
  NodeUnderTest (IOSim s) -> Point LeiosTestBlock -> NominalDiffTime -> IOSim s (Listener s)
newListenerWith node immTip offset = do
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
              (envLeiosNotifyCodec node)
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
              (envLeiosFetchCodec node)
              chan
              (fetchingBack toFetch settle)
      }
 where
  cfg = nodeTopLevelConfig node

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
feedingLeiosNotifyServer node notifications chan = do
  server <-
    leiosNotifyServerPeerLookahead
      (pure NotExcessiveRequests)
      (readTQueue notifications)
  void $
    runLookaheadFixedSenderPeerWithLimits
      nullTracer
      (envLeiosNotifyCodec node)
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
serving eb txs node chan =
  fst
    <$> runAnnotatedPeer nullTracer (envLeiosFetchCodec node) chan (leiosFetchServerPeer (serveEb eb txs))

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
listenerReach immTip offset age expected = withNode $ \node -> do
  next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
  notifications <- newTQueueIO
  listener <- newListenerWith node immTip offset
  let
    -- A slot's onset is as many seconds into the simulation as its number.
    onset = Time (fromIntegral (unSlotNo (blockSlot announcingLater)))
    waitUntil t = getMonotonicTime >>= threadDelay . diffTime t
    holder = do
      waitUntil onset
      atomically $ mapM_ (writeTQueue notifications) (announcingAndOffering fetchedEb announcingLater)
      withNodeVsEnvConnection node (PeerAddr 1) (holdingFetched node notifications) $ \_holderConn ->
        replicateM_ 2 $ atomically next
  withAsync holder $ \_ -> do
    waitUntil (addTime age onset)
    withNodeVsEnvConnection node (PeerAddr 2) (listening node listener) $ \_listenerConn ->
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
holdingFetched node notifications =
  EnvPeer
    { epLeiosNotifyServer = feedingLeiosNotifyServer node notifications
    , epLeiosNotifyClient = Nothing
    , epLeiosFetchServer = servingFetched node
    , epLeiosFetchClient = Nothing
    }

-- | 'holdingFetched', which also listens to the node as the listener does.
holdingFetchedListening ::
  NodeUnderTest (IOSim s) ->
  StrictTQueue
    (IOSim s)
    (Message (LeiosNotify LeiosPoint (Header LeiosTestBlock) LeiosVote) LN.StBusy LN.StIdle) ->
  Listener s ->
  EnvPeer (IOSim s)
holdingFetchedListening node notifications listener =
  (holdingFetched node notifications)
    { epLeiosNotifyClient = Just (lsNotifyClient listener)
    , epLeiosFetchClient = Just (lsFetchClient listener)
    }

-- | A peer that only listens.
listening :: NodeUnderTest (IOSim s) -> Listener s -> EnvPeer (IOSim s)
listening node listener =
  EnvPeer
    { epLeiosNotifyServer = \_chan -> atomically retry
    , epLeiosNotifyClient = Just (lsNotifyClient listener)
    , epLeiosFetchServer = silentLeiosFetchServer node
    , epLeiosFetchClient = Just (lsFetchClient listener)
    }

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
nodeServerAgainst client = withNode $ \node -> do
  holdHeldEb node
  let envPeer =
        EnvPeer
          { epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = refusingLeiosFetchServer node
          , epLeiosFetchClient = Just $ runEnvClient node client
          }
  withNodeVsEnvConnection node (PeerAddr 1) envPeer $ \conn -> do
    server <- nodeLeiosFetchServer conn
    atomically server >>= \case
      Right () -> pure ServerEnded
      Left e -> pure $ ServerFailed (show e)

runEnvClient :: NodeUnderTest (IOSim s) -> EnvClient s -> Channel (IOSim s) ByteString -> IOSim s ()
runEnvClient node client chan = fst <$> runAnnotatedPeer nullTracer (envLeiosFetchCodec node) chan client

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
holdHeldEb node = withWriter (getLeiosDB (nodeKernel node)) $ \writer -> do
  _ <- await =<< writeEbBody writer heldPoint heldEb
  _ <- await =<< writeTxs writer heldPoint [(i, heldTxBytes i) | i <- heldOffsets]
  pure ()

{-------------------------------------------------------------------------------
  Registration
-------------------------------------------------------------------------------}

-- | Whether the node's initiator bundle, and its initiator-and-responder bundle,
-- include LeiosFetch among their hot mini-protocols for this version.
nodeRunsLeiosFetch :: NodeToNodeVersion -> IOSim s (Bool, Bool)
nodeRunsLeiosFetch ntnVersion = withNode $ \node ->
  pure
    ( runsLeiosFetch $
        NTN.initiator mempty defaultMiniProtocolParameters ntnVersion versionData (nodeApps node ())
    , runsLeiosFetch $
        NTN.initiatorAndResponder
          mempty
          defaultMiniProtocolParameters
          ntnVersion
          versionData
          (nodeApps node ())
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

-- Explicit, small fixture geometry exercises several wire batches without
-- changing normal node defaults or tying service checkpoints to publication.
atomicNodeConfig :: AtomicNodeConfig
atomicNodeConfig =
  AtomicNodeConfig
    { nodeStorageLimits = Handoff.StorageLimits 2097152 2048 16 2 100
    , nodeClosureRetryConfig = Retry.defaultRetryConfig
    , nodeResponseBytes = 1048576
    , nodeCheckpointLimits = must (AtomicAcquisition.mkCheckpointLimits 100 500000 20000 550000)
    , nodeTimingConfig = must (Deadline.mkTimingConfig 1500000 14600 0.5 2 60 (PayloadBytes 600000))
    }
 where
  must result = either (error . show) id result

atomicCodec ::
  AnnotatedCodec
    (Atomic.LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx)
    CBOR.DeserialiseFailure
    (IOSim s)
    ByteString
atomicCodec =
  Atomic.codecLeiosFetchAtomic
    20000
    encodeLeiosPoint
    decodeLeiosPoint
    encodeEndorserBlock
    decodeEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

fillLeiosDb ::
  LeiosDbHandle (IOSim s) -> LeiosPoint -> EndorserBlock -> V.Vector LeiosTx -> IOSim s ()
fillLeiosDb db point eb txs = withWriter db $ \writer -> do
  void $ writeEbBody writer point eb >>= await
  void $ writeTxs writer point (zip [0 ..] (map leiosTxBytes (V.toList txs))) >>= await

atomicServing :: LeiosDbHandle (IOSim s) -> Channel (IOSim s) ByteString -> IOSim s ()
atomicServing db = atomicServingCounted db (pure ())

atomicServingCounted ::
  LeiosDbHandle (IOSim s) -> STM (IOSim s) () -> Channel (IOSim s) ByteString -> IOSim s ()
atomicServingCounted db counted channel = withReader db $ \reader ->
  let instrumented =
        reader
          { batchRetrieveTxs = \point offsets -> atomically counted >> batchRetrieveTxs reader point offsets
          }
   in withLeiosFetchAtomicServer nullTracer (AtomicServerState.defaultServerLimits 64) instrumented $ \server ->
        void $
          runLookaheadPeerWithAnnotatedLimits
            nullTracer
            atomicCodec
            (Atomic.byteLimitsLeiosFetchAtomic (fromIntegral . BSL.length))
            Atomic.timeLimitsLeiosFetchAtomic
            channel
            (AtomicServer.atomicServerPeer server)

atomicObservation :: NodeUnderTest (IOSim s) -> IOSim s NodeObservation
atomicObservation node = case nodeAtomicFetch node of
  Nothing -> throwIO $ userError "the explicitly selected atomic node did not start"
  Just runtime -> atomically (readNodeObservation runtime)

ensure :: Bool -> String -> IOSim s ()
ensure condition message = unless condition (throwIO (userError message))

checkStored ::
  NodeUnderTest (IOSim s) -> LeiosPoint -> EndorserBlock -> V.Vector LeiosTx -> IOSim s ()
checkStored node point eb txs = withReader (getLeiosDB (nodeKernel node)) $ \reader -> do
  body <- lookupEbBody reader point
  closure <- lookupEbClosure reader point
  ensure (body == Just (endorserBlockBytes eb)) "stored body bytes differ from the wire body"
  ensure
    (closure == Just [(hashLeiosTx tx, leiosTxBytes tx) | tx <- V.toList txs])
    "stored closure differs from the requested TXs"

-- Body-only announcement must not authorize a closure request. Seed local
-- possession after kernel startup, so the next body receipt must discover it.
atomicHonest :: Int -> IOSim s (Word64, Word64)
atomicHonest cached = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  fillLeiosDb serverDb fetchedPoint fetchedEb fetchedTxs
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    when (cached > 0) $ fillLeiosDb db fetchedPoint fetchedEb (V.take cached fetchedTxs)
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    notifications <- newTQueueIO
    atomically $
      mapM_
        (writeTQueue notifications)
        [ MsgLeiosBlockAnnouncement (ltbHeader announcingFetched)
        , MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
        ]
    let peer = EnvPeer (feedingLeiosNotifyServer node notifications) Nothing (atomicServing serverDb) Nothing
    withNodeVsEnvConnection node (PeerAddr 1) peer $ \conn -> do
      ensure (connVersion conn == NodeToNodeV_17) "atomic fixture negotiated the legacy version"
      firstAcquired <- acquiredOf <$> atomically next
      ensure
        (firstAcquired == AcquiredBody fetchedPoint)
        "closure arrived before the independently durable body"
      threadDelay 1
      before <- atomicObservation node
      ensure (observedCheckpoints before == 0) "a body-only offer authorized TX requests"
      atomically $ writeTQueue notifications (MsgLeiosBlockTxsOffer fetchedPoint)
      awaitAcquired next (AcquiredClosure fetchedPoint)
      checkStored node fetchedPoint fetchedEb fetchedTxs
      observation <- atomicObservation node
      ensure
        (observedAtomicConnections observation == 1 && observedLegacyConnections observation == 0)
        "the new client was not the one that ran"
      ensure
        (observedScopedRttReaders observation + observedRttFallbacks observation == 1)
        "RTT capture was not accounted for"
      when (cached == 0) $ ensure (observedPeakData observation >= 2) "checkpoints were not pipelined"
      connTerminate conn
      result <- atomically $ (connNodeInitiators conn Map.! leiosFetchMiniProtocolNum)
      either throwIO (const (pure ())) result
      pure (observedBodyRequests observation, observedCheckpoints observation)

atomicAlreadyHeld :: IOSim s (Word64, Word64)
atomicAlreadyHeld = do
  db <- newLeiosDBInMemory
  fillLeiosDb db fetchedPoint fetchedEb fetchedTxs
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    notifications <- newTQueueIO
    atomically $ mapM_ (writeTQueue notifications) (announcingAndOffering fetchedEb announcingFetched)
    let peer =
          EnvPeer
            (feedingLeiosNotifyServer node notifications)
            Nothing
            (\_ -> throwIO (userError "a held point was fetched"))
            Nothing
    withNodeVsEnvConnection node (PeerAddr 1) peer $ \_ -> do
      threadDelay 2
      checkStored node fetchedPoint fetchedEb fetchedTxs
      observation <- atomicObservation node
      pure (observedBodyRequests observation, observedCheckpoints observation)

atomicNodeServes :: forall s. IOSim s Bool
atomicNodeServes = do
  db <- newLeiosDBInMemory
  fillLeiosDb db fetchedPoint fetchedEb fetchedTxs
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    notifications <- newTQueueIO
    let requester :: Channel (IOSim s) ByteString -> IOSim s ()
        requester channel = void $ runAnnotatedPeer nullTracer atomicCodec channel requestPeer
        requestPeer ::
          AtomicPeer.Client
            (Atomic.LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx)
            NonPipelined
            Atomic.StIdle
            (IOSim s)
            ()
        requestPeer = AtomicPeer.Yield (Atomic.MsgLeiosBlockRequest (WireOrdinal 0) fetchedPoint) $
          AtomicPeer.Await $ \case
            Atomic.MsgLeiosBlock body -> AtomicPeer.Effect $ do
              ensure
                (endorserBlockBytes body == endorserBlockBytes fetchedEb)
                "atomic server changed original body bytes"
              let bitmap = NE.fromList (bitmapsNaming [0, 2])
              pure $
                AtomicPeer.Yield (Atomic.MsgLeiosBlockTxsRequest (WireOrdinal 1) fetchedPoint bitmap) $
                  AtomicPeer.Await $ \case
                    Atomic.MsgLeiosBlockTxs txs -> AtomicPeer.Effect $ do
                      ensure
                        (txs == V.fromList [fetchedTxs V.! 0, fetchedTxs V.! 2])
                        "atomic server returned the wrong bitmap vector"
                      pure $ AtomicPeer.Yield Atomic.MsgDone (AtomicPeer.Done ())
                    Atomic.MsgBlockTxsCancelled -> AtomicPeer.Effect $ throwIO (userError "honest TX request was cancelled")
            Atomic.MsgBlockCancelled -> AtomicPeer.Effect $ throwIO (userError "honest body request was cancelled")
        peer = EnvPeer (feedingLeiosNotifyServer node notifications) Nothing (atomicServing db) (Just requester)
    withNodeVsEnvConnection node (PeerAddr 1) peer $ \conn -> do
      case connPeerLeiosFetchClient conn of
        Nothing -> throwIO $ userError "the atomic requesting peer was not started"
        Just outcome -> atomically outcome >>= either throwIO pure
      pure (connVersion conn == NodeToNodeV_17)

atomicMisstatement :: IOSim s Bool
atomicMisstatement = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  fillLeiosDb serverDb fetchedPoint fetchedEb fetchedTxs
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    notifications <- newTQueueIO
    atomically $
      mapM_
        (writeTQueue notifications)
        [ MsgLeiosBlockAnnouncement (ltbHeader announcingFetched)
        , MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb - 1)
        ]
    let peer = EnvPeer (feedingLeiosNotifyServer node notifications) Nothing (atomicServing serverDb) Nothing
    withNodeVsEnvConnection node (PeerAddr 1) peer $ \conn -> do
      outcome <- atomically (connNodeInitiators conn Map.! leiosFetchMiniProtocolNum)
      awaitAcquired next (AcquiredBody fetchedPoint)
      stored <- withReader db $ \reader -> lookupEbBody reader fetchedPoint
      pure
        ( case outcome of
            Left e ->
              show e == show AtomicClient.ClientBodySizeMisstated && stored == Just (endorserBlockBytes fetchedEb)
            Right _ -> False
        )

atomicMixed :: Bool -> IOSim s (Word64, Word64)
atomicMixed legacyBody = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  fillLeiosDb serverDb fetchedPoint fetchedEb fetchedTxs
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    atomicOffers <- newTQueueIO
    legacyOffers <- newTQueueIO
    let newPeer = EnvPeer (feedingLeiosNotifyServer node atomicOffers) Nothing (atomicServing serverDb) Nothing
        oldPeer = EnvPeer (feedingLeiosNotifyServer node legacyOffers) Nothing (servingFetched node) Nothing
        bodyOffers = if legacyBody then legacyOffers else atomicOffers
        txOffers = if legacyBody then atomicOffers else legacyOffers
    withNodeVsEnvConnection node (PeerAddr 1) newPeer $ \newConn ->
      withNodeVsEnvConnectionVersion NodeToNodeV_16 node (PeerAddr 2) oldPeer $ \oldConn -> do
        ensure
          (connVersion newConn == NodeToNodeV_17 && connVersion oldConn == NodeToNodeV_16)
          "mixed fixture negotiated wrong versions"
        atomically $
          mapM_
            (writeTQueue bodyOffers)
            [ MsgLeiosBlockAnnouncement (ltbHeader announcingFetched)
            , MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
            ]
        awaitAcquired next (AcquiredBody fetchedPoint)
        atomically $
          mapM_
            (writeTQueue txOffers)
            [MsgLeiosBlockAnnouncement (ltbHeader announcingFetched), MsgLeiosBlockTxsOffer fetchedPoint]
        awaitAcquired next (AcquiredClosure fetchedPoint)
        duplicate <- timeout 1 (atomically next)
        ensure
          (case duplicate of Nothing -> True; _ -> False)
          "mixed acquisition emitted a duplicate notification"
        checkStored node fetchedPoint fetchedEb fetchedTxs
        observation <- atomicObservation node
        ensure
          (observedAtomicConnections observation == 1 && observedLegacyConnections observation == 1)
          "mixed paths were not both exercised"
        pure (observedBodyRequests observation, observedCheckpoints observation)

atomicConcurrentEbs :: IOSim s (Word64, Word64)
atomicConcurrentEbs = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  let thirdAnnouncement =
        setAnnouncement
          (endorserBlockHash fetchedEb)
          (encodeEndorserBlockSize fetchedEb)
          (successorLeiosBlock reannouncingFetched)
      thirdPoint = MkLeiosPoint (blockSlot thirdAnnouncement) (endorserBlockHash fetchedEb)
      otherPoint = MkLeiosPoint (blockSlot announcingHonest) (endorserBlockHash honestEb)
  fillLeiosDb serverDb thirdPoint fetchedEb fetchedTxs
  fillLeiosDb serverDb fetchedPoint fetchedEb fetchedTxs
  fillLeiosDb serverDb otherPoint honestEb (V.singleton sharedTx)
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    notifications <- newTQueueIO
    threadDelay 6
    atomically $
      mapM_
        (writeTQueue notifications)
        ( announcingAndOffering fetchedEb announcingFetched
            ++ announcingAndOffering fetchedEb thirdAnnouncement
            ++ announcingAndOffering honestEb announcingHonest
        )
    let peer = EnvPeer (feedingLeiosNotifyServer node notifications) Nothing (atomicServing serverDb) Nothing
    withNodeVsEnvConnection node (PeerAddr 1) peer $ \_ -> do
      acquired <- replicateM 6 (acquiredOf <$> atomically next)
      ensure
        ( List.sortOn show acquired
            == List.sortOn
              show
              [ AcquiredBody fetchedPoint
              , AcquiredClosure fetchedPoint
              , AcquiredBody otherPoint
              , AcquiredClosure otherPoint
              , AcquiredBody thirdPoint
              , AcquiredClosure thirdPoint
              ]
        )
        "concurrent EB acquisitions differ"
      checkStored node thirdPoint fetchedEb fetchedTxs
      checkStored node fetchedPoint fetchedEb fetchedTxs
      checkStored node otherPoint honestEb (V.singleton sharedTx)
      observation <- atomicObservation node
      ensure
        (observedPeakData observation >= 2 && observedPeakData observation <= 64)
        "concurrent EBs bypassed the negotiated window"
      pure (observedBodyRequests observation, observedCheckpoints observation)

atomicRelaysDurably :: IOSim s [Relayed]
atomicRelaysDurably = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  fillLeiosDb serverDb fetchedPoint fetchedEb fetchedTxs
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    listener <- newListener node
    notifications <- newTQueueIO
    atomically $ mapM_ (writeTQueue notifications) (announcingAndOffering fetchedEb announcingFetched)
    let peer = EnvPeer (feedingLeiosNotifyServer node notifications) Nothing (atomicServing serverDb) Nothing
    withNodeVsEnvConnectionVersion NodeToNodeV_16 node (PeerAddr 2) (listening node listener) $ \_ ->
      withNodeVsEnvConnection node (PeerAddr 1) peer $ \_ -> relayedBy listener 3

atomicOfferOrder :: IOSim s (Int, Int)
atomicOfferOrder = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  fillLeiosDb serverDb fetchedPoint fetchedEb fetchedTxs
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offersA <- newTQueueIO
    offersB <- newTQueueIO
    requestsA <- newTVarIO 0
    requestsB <- newTVarIO 0
    let peer offers counter =
          EnvPeer
            (feedingLeiosNotifyServer node offers)
            Nothing
            (atomicServingCounted serverDb (modifyTVar counter (+ 1)))
            Nothing
        kernel = getLeiosFetchKernel (nodeKernel node)
        waitOffered addr = IOLike.atomically $ do
          peers <- IOLike.readTVar (lfkPeers kernel)
          found <- forM (Map.toList peers) $ \(MkLeiosPeerLifetimeId conn _, vars) -> do
            offers <- IOLike.readTVar (lfpOfferings vars)
            pure
              ( remoteAddress conn == addr && case poOffered <$> Map.lookup fetchedPoint offers of
                  Just OfferedClosure -> True
                  Just OfferedBodyAndClosure{} -> True
                  _ -> False
              )
          IOLike.check (or found)
    withNodeVsEnvConnection node (PeerAddr 1) (peer offersA requestsA) $ \_ ->
      withNodeVsEnvConnection node (PeerAddr 2) (peer offersB requestsB) $ \_ -> do
        atomically $ do
          mapM_
            (writeTQueue offersA)
            [ MsgLeiosBlockAnnouncement (ltbHeader announcingFetched)
            , MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
            ]
          writeTQueue offersB (MsgLeiosBlockAnnouncement (ltbHeader announcingFetched))
        awaitAcquired next (AcquiredBody fetchedPoint)
        -- Let both announcement callbacks finish before holding their shared
        -- decision lock. Offers themselves use only STM and a nonblocking wake.
        threadDelay 0.01
        IOLike.withMVar (lfcOutstanding (lfkContext kernel)) $ \_ -> do
          atomically $ writeTQueue offersB (MsgLeiosBlockTxsOffer fetchedPoint)
          waitOffered (PeerAddr 2)
          threadDelay 0.01
          atomically $ writeTQueue offersA (MsgLeiosBlockTxsOffer fetchedPoint)
          waitOffered (PeerAddr 1)
        awaitAcquired next (AcquiredClosure fetchedPoint)
        atomically $ (,) <$> readTVar requestsA <*> readTVar requestsB

-- Body fixtures record only a small, bounded schedule in these short tests.
type BodyLog s = StrictTVar (IOSim s) [(PeerAddr, Time)]

bodyOffer ::
  NodeUnderTest (IOSim s) ->
  StrictTQueue (IOSim s) (Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) LN.StBusy LN.StIdle) ->
  IOSim s ()
bodyOffer _ notifications =
  atomically $
    mapM_
      (writeTQueue notifications)
      [ MsgLeiosBlockAnnouncement (ltbHeader announcingFetched)
      , MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
      ]

bodyProvider ::
  BodyLog s ->
  PeerAddr ->
  Maybe DiffTime ->
  AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s)
bodyProvider logVar addr delay = AtomicServer.AtomicRequestHandler $ \case
  AtomicServer.ServeBody _ _ -> do
    now <- getMonotonicTime
    atomically $ modifyTVar logVar (<> [(addr, now)])
    case delay of { Nothing -> atomically retry; Just duration -> threadDelay duration }
    pure
      (AtomicServer.PreparedBody (fromIntegral (BS.length (endorserBlockBytes fetchedEb)) + 2) fetchedEb)
  AtomicServer.ServeTransactions{} -> throwIO $ userError "a body-only offer authorized a closure request"

bodyServing ::
  AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s) ->
  (Channel (IOSim s) ByteString -> Channel (IOSim s) ByteString) ->
  Channel (IOSim s) ByteString ->
  IOSim s ()
bodyServing provider adjust channel = AtomicServer.withAtomicServer (AtomicServerState.defaultServerLimits 64) provider $ \server ->
  void $
    runLookaheadPeerWithAnnotatedLimits
      nullTracer
      atomicCodec
      (Atomic.byteLimitsLeiosFetchAtomic (fromIntegral . BSL.length))
      Atomic.timeLimitsLeiosFetchAtomic
      (adjust channel)
      (AtomicServer.atomicServerPeer server)

-- The first body is irrevocably committed before its first partial send. Its
-- remaining bytes and the later cancel ACK each have an independent barrier.
committedBodyChannel ::
  StrictTVar (IOSim s) Int ->
  StrictTVar (IOSim s) Bool ->
  StrictTVar (IOSim s) Bool ->
  Channel (IOSim s) ByteString ->
  Channel (IOSim s) ByteString
committedBodyChannel sends bodyGate ackGate channel =
  channel
    { send = \bytes -> do
        index <- atomically $ do
          n <- readTVar sends
          writeTVar sends (n + 1)
          pure n
        case index of
          0 -> do
            send channel (BSL.take 8 bytes)
            atomically $ readTVar bodyGate >>= check
            send channel (BSL.drop 8 bytes)
          1 -> atomically (readTVar ackGate >>= check) >> send channel bytes
          _ -> send channel bytes
    }

waitBodyRequests :: Int -> BodyLog s -> IOSim s [(PeerAddr, Time)]
waitBodyRequests count logVar = atomically $ do
  seen <- readTVar logVar
  check (length seen >= count)
  pure seen

bodySnapshotOf ::
  NodeUnderTest (IOSim s) -> IOSim s (BodyPolicy.BodySnapshot (ConnectionId PeerAddr))
bodySnapshotOf node = case nodeAtomicFetch node of
  Nothing -> throwIO $ userError "body policy is not installed"
  Just runtime -> atomically $ do
    snapshots <- readNodeBodySnapshots runtime
    maybe retry pure (Map.lookup fetchedPoint snapshots)

bodyUsageOf :: NodeUnderTest (IOSim s) -> PeerAddr -> IOSim s Admission.Usage
bodyUsageOf node addr = case nodeAtomicFetch node of
  Nothing -> throwIO $ userError "body policy is not installed"
  Just runtime -> atomically $ do
    usages <- readNodeClientUsage runtime
    case [u | (MkLeiosPeerLifetimeId conn _, Right u) <- Map.toList usages, remoteAddress conn == addr] of
      [usage] -> pure usage
      _ -> retry

bodySoleSource :: IOSim s (Word64, Word64, Word64)
bodySoleSource = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offers <- newTQueueIO
    logVar <- newTVarIO []
    bodyOffer node offers
    let upstream =
          EnvPeer
            (feedingLeiosNotifyServer node offers)
            Nothing
            (bodyServing (bodyProvider logVar (PeerAddr 1) (Just 2.5)) id)
            Nothing
    withNodeVsEnvConnection node (PeerAddr 1) upstream $ \_ -> do
      awaitAcquired next (AcquiredBody fetchedPoint)
      observation <- atomicObservation node
      pure
        ( observedBodyRequests observation
        , observedBodyTriggers observation
        , observedBodyFailures observation
        )

bodyNewAlternative :: Bool -> IOSim s (Word64, Word64, Word64)
bodyNewAlternative misstate = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    firstOffers <- newTQueueIO
    nextOffers <- newTQueueIO
    logVar <- newTVarIO []
    let upstream offers addr delay =
          EnvPeer
            (feedingLeiosNotifyServer node offers)
            Nothing
            (bodyServing (bodyProvider logVar addr delay) id)
            Nothing
    withNodeVsEnvConnection node (PeerAddr 1) (upstream firstOffers (PeerAddr 1) Nothing) $ \_ ->
      withNodeVsEnvConnection node (PeerAddr 2) (upstream nextOffers (PeerAddr 2) (Just 0)) $ \alternative -> do
        bodyOffer node firstOffers
        void $ waitBodyRequests 1 logVar
        threadDelay 1.2
        if misstate
          then
            atomically $
              mapM_
                (writeTQueue nextOffers)
                [ MsgLeiosBlockAnnouncement (ltbHeader announcingFetched)
                , MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb - 1)
                ]
          else bodyOffer node nextOffers
        awaitAcquired next (AcquiredBody fetchedPoint)
        when misstate $ do
          outcome <- atomically (connNodeInitiators alternative Map.! leiosFetchMiniProtocolNum)
          ensure
            (case outcome of Left e -> show e == show AtomicClient.ClientBodySizeMisstated; _ -> False)
            "a hedge body-size misstatement lost its original peer fault"
        observation <- atomicObservation node
        ensure
          (observedPeakWantedBodies observation == 2)
          "body hedge exceeded or missed the wanted-copy bound"
        pure
          ( observedBodyRequests observation
          , observedBodyTriggers observation
          , observedBodyFailures observation
          )

bodyRoundRobin :: IOSim s Bool
bodyRoundRobin = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    offers <- mapM (const newTQueueIO) [1 .. 4 :: Int]
    logVar <- newTVarIO []
    let connect [] action = action
        connect ((n, notifications) : rest) action =
          let upstream =
                EnvPeer
                  (feedingLeiosNotifyServer node notifications)
                  Nothing
                  (bodyServing (bodyProvider logVar (PeerAddr n) Nothing) id)
                  Nothing
           in withNodeVsEnvConnection node (PeerAddr n) upstream (const (connect rest action))
    connect (zip [1 .. 4] offers) $ do
      bodyOffer node (head offers)
      void $ waitBodyRequests 1 logVar
      mapM_ (bodyOffer node) (tail offers)
      seen <- take 5 <$> waitBodyRequests 5 logVar
      observation <- atomicObservation node
      let deltas = zipWith (\(_, a) (_, b) -> diffTime b a) seen (tail seen)
          near a b = abs (a - b) < 0.001
      ensure
        (map fst seen == map PeerAddr [1, 2, 3, 4, 2])
        "body retries did not follow arrived-peer round robin"
      ensure
        (and (zipWith near deltas [1, 1, 1.5, 2]))
        "body backoff did not span peers or respect the ceiling"
      pure
        ( observedBodyTriggers observation == 1
            && observedBodyFailures observation == 3
            && observedPeakWantedBodies observation == 2
        )

bodyLateWinner :: Bool -> IOSim s Bool
bodyLateWinner primaryWins = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offers <- mapM (const newTQueueIO) [1 .. 4 :: Int]
    logVar <- newTVarIO []
    sends <- newTVarIO 0
    bodyGate <- newTVarIO False
    ackGate <- newTVarIO primaryWins
    let blocked = if primaryWins then 1 else 2
        connect [] action = action
        connect ((n, notifications) : rest) action =
          let adjust = if n == blocked then committedBodyChannel sends bodyGate ackGate else id
              delay = if n == blocked then Just 0 else Nothing
              upstream =
                EnvPeer
                  (feedingLeiosNotifyServer node notifications)
                  Nothing
                  (bodyServing (bodyProvider logVar (PeerAddr n) delay) adjust)
                  Nothing
           in withNodeVsEnvConnection node (PeerAddr n) upstream (const (connect rest action))
    connect (zip [1 .. 4] offers) $ do
      bodyOffer node (head offers)
      void $ waitBodyRequests 1 logVar
      mapM_ (bodyOffer node) (tail offers)
      void $ waitBodyRequests (if primaryWins then 2 else 4) logVar
      before <- bodySnapshotOf node
      ensure (BodyPolicy.bodyWanted before == 2) "body logical slots were not bounded"
      unless primaryWins $ do
        usage <- bodyUsageOf node (PeerAddr 2)
        ensure
          ( Admission.usedData usage == 1
              && Admission.usedExchanges usage == 2
              && Admission.usedResponseBytes usage > 0
          )
          "a retiring hedge prematurely released its original/cancel debt"
        ensure (BodyPolicy.bodyDraining before >= 1) "the retiring committed hedge was forgotten"
      atomically $ writeTVar bodyGate True
      awaitAcquired next (AcquiredBody fetchedPoint)
      threadDelay 0.01
      fulfilled <- bodySnapshotOf node
      ensure
        (BodyPolicy.bodyFulfilled fulfilled && BodyPolicy.bodyWanted fulfilled == 0)
        "a valid late body did not fulfill the acquisition"
      let failures = if primaryWins then 0 else 2
      ensure
        (BodyPolicy.bodyFailures fulfilled == failures)
        "late fulfillment lost or duplicated a hedge failure"
      unless primaryWins $ do
        usage <- bodyUsageOf node (PeerAddr 2)
        ensure
          (Admission.usedData usage == 1 && Admission.usedExchanges usage == 1)
          "consuming an original prematurely released its pending ACK record"
        threadDelay 0.6
        observation <- atomicObservation node
        ensure
          (observedBodyFailures observation == fromIntegral failures)
          "a redundant/cancel ACK deadline added body backoff"
        atomically $ writeTVar ackGate True
        atomically $ case nodeAtomicFetch node of
          Nothing -> retry
          Just runtime -> do
            usages <- readNodeClientUsage runtime
            let retired =
                  [ Admission.usedData u == 0
                  | (MkLeiosPeerLifetimeId conn _, Right u) <- Map.toList usages
                  , remoteAddress conn == PeerAddr 2
                  ]
            check (retired == [True])
      observation <- atomicObservation node
      pure (observedBodyTriggers observation == 1 && observedPeakWantedBodies observation == 2)

-- A second EB's primary is queued behind the first EB's stalled body. Its own
-- timer must stay inactive, but that FIFO dependency must not strand its body.
bodyConcurrentEbs :: forall s. IOSim s Bool
bodyConcurrentEbs = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offersA <- newTQueueIO
    offersB <- newTQueueIO
    let otherPoint = MkLeiosPoint (blockSlot announcingHonest) (endorserBlockHash honestEb)
        bodies = Map.fromList [(fetchedPoint, fetchedEb), (otherPoint, honestEb)]
        provider :: Bool -> AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s)
        provider withheld = AtomicServer.AtomicRequestHandler $ \case
          AtomicServer.ServeBody _ point -> do
            when withheld (atomically retry)
            let body = bodies Map.! point
            pure (AtomicServer.PreparedBody (fromIntegral (BS.length (endorserBlockBytes body)) + 2) body)
          AtomicServer.ServeTransactions{} -> throwIO $ userError "body-only concurrent offers authorized TX fetching"
        upstream notifications withheld =
          EnvPeer
            (feedingLeiosNotifyServer node notifications)
            Nothing
            (bodyServing (provider withheld) id)
            Nothing
        publish ::
          StrictTQueue (IOSim s) (Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) LN.StBusy LN.StIdle) ->
          IOSim s ()
        publish notifications =
          atomically $
            mapM_
              (writeTQueue notifications)
              [ MsgLeiosBlockAnnouncement (ltbHeader announcingFetched)
              , MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
              , MsgLeiosBlockAnnouncement (ltbHeader announcingHonest)
              , MsgLeiosBlockOffer otherPoint (encodeEndorserBlockSize honestEb)
              ]
    threadDelay 4
    withNodeVsEnvConnection node (PeerAddr 1) (upstream offersA True) $ \_ ->
      withNodeVsEnvConnection node (PeerAddr 2) (upstream offersB False) $ \_ -> do
        publish offersA
        atomically $ case nodeAtomicFetch node of
          Nothing -> retry
          Just runtime -> readNodeObservation runtime >>= check . (>= 2) . observedBodyRequests
        publish offersB
        acquired <- replicateM 2 (acquiredOf <$> atomically next)
        ensure
          (List.sortOn show acquired == List.sortOn show [AcquiredBody fetchedPoint, AcquiredBody otherPoint])
          "queued EB body was stranded behind another EB's stalled original"
        observation <- atomicObservation node
        pure
          ( observedBodyHedges observation == 2
              && observedBodyFailures observation == 0
              && observedPeakWantedBodies observation == 2
          )

-- With the large trusted reservation, the common predecessor is TX data from
-- another EB, rather than another body. Exercise that actual mixed reply path.
bodyBehindTransactions :: forall s. IOSim s Bool
bodyBehindTransactions = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offersA <- newTQueueIO
    offersB <- newTQueueIO
    txStarted <- newTVarIO False
    unexpectedBodyStarted <- newTVarIO False
    let otherPoint = MkLeiosPoint (blockSlot announcingHonest) (endorserBlockHash honestEb)
        provider :: Bool -> AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s)
        provider blocksTxs = AtomicServer.AtomicRequestHandler $ \case
          AtomicServer.ServeBody _ point -> do
            when (blocksTxs && point == otherPoint) $ atomically $ writeTVar unexpectedBodyStarted True
            let body = if point == fetchedPoint then fetchedEb else honestEb
            pure (AtomicServer.PreparedBody (fromIntegral (BS.length (endorserBlockBytes body)) + 2) body)
          AtomicServer.ServeTransactions{} -> do
            atomically $ writeTVar txStarted True
            atomically retry
        upstream notifications blocksTxs =
          EnvPeer
            (feedingLeiosNotifyServer node notifications)
            Nothing
            (bodyServing (provider blocksTxs) id)
            Nothing
        publish ::
          StrictTQueue (IOSim s) (Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) LN.StBusy LN.StIdle) ->
          IOSim s ()
        publish notifications =
          atomically $
            mapM_
              (writeTQueue notifications)
              [ MsgLeiosBlockAnnouncement (ltbHeader announcingHonest)
              , MsgLeiosBlockOffer otherPoint (encodeEndorserBlockSize honestEb)
              ]
    threadDelay 4
    withNodeVsEnvConnection node (PeerAddr 1) (upstream offersA True) $ \_ ->
      withNodeVsEnvConnection node (PeerAddr 2) (upstream offersB False) $ \_ -> do
        atomically $ mapM_ (writeTQueue offersA) (announcingAndOffering fetchedEb announcingFetched)
        awaitAcquired next (AcquiredBody fetchedPoint)
        atomically $ readTVar txStarted >>= check
        publish offersA
        atomically $ case nodeAtomicFetch node of
          Nothing -> retry
          Just runtime -> readNodeObservation runtime >>= check . (>= 2) . observedBodyRequests
        publish offersB
        awaitAcquired next (AcquiredBody otherPoint)
        wasServed <- readTVarIO unexpectedBodyStarted
        observation <- atomicObservation node
        ensure (not wasServed) "the supposedly queued body reached its serving head"
        pure
          ( observedBodyRequests observation == 3
              && observedBodyHedges observation == 1
              && observedBodyTriggers observation == 0
              && observedBodyFailures observation == 0
          )

-- A stalled committed original retains its descriptor and verified prefix.
-- Rescue logs only request membership; production retains fixed-size counters.
closureRecovery :: forall s. Bool -> IOSim s Bool
closureRecovery partial = do
  db <- newLeiosDBInMemory
  let config =
        atomicNodeConfig
          { nodeCheckpointLimits =
              must (AtomicAcquisition.mkCheckpointLimits (if partial then 200 else 100) 500000 20000 550000)
          }
      must = either (error . show) id
  withAtomicNodeOver config db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offersA <- newTQueueIO
    offersB <- newTQueueIO
    started <- newTVarIO False
    logVar <- newTVarIO ([] :: [(Bool, [Int])])
    sendCount <- newTVarIO (0 :: Int)
    gate <- newTVarIO False
    let provider :: Bool -> AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s)
        provider isPrimary = AtomicServer.AtomicRequestHandler $ \case
          AtomicServer.ServeBody _ _ -> pure (AtomicServer.PreparedBody (encodeEndorserBlockSize fetchedEb) fetchedEb)
          AtomicServer.ServeTransactions _ _ bitmaps -> do
            let offsets = bitmapOffsets (NE.toList bitmaps)
                txs = V.fromList [fetchedTxs V.! off | off <- offsets]
            atomically $ do
              modifyTVar logVar (<> [(isPrimary, offsets)])
              when isPrimary $ writeTVar started True
            when (isPrimary && not partial) $ atomically retry
            pure (AtomicServer.PreparedTransactions (fromIntegral (V.length txs) * 103 + 8) txs)
        adjust channel =
          channel
            { send = \bytes -> do
                index <- atomically $ do
                  index <- readTVar sendCount
                  writeTVar sendCount (index + 1)
                  pure index
                if index == 1
                  then do
                    -- A complete isPrimary 100 B TX and an unfinished second TX.
                    send channel (BSL.take 110 bytes)
                    atomically $ readTVar gate >>= check
                    send channel (BSL.drop 110 bytes)
                  else send channel bytes
            }
        upstream notifications isPrimary =
          EnvPeer
            (feedingLeiosNotifyServer node notifications)
            Nothing
            (bodyServing (provider isPrimary) (if isPrimary && partial then adjust else id))
            Nothing
    withNodeVsEnvConnection node (PeerAddr 1) (upstream offersA True) $ \_ ->
      withNodeVsEnvConnection node (PeerAddr 2) (upstream offersB False) $ \_ -> do
        atomically $ mapM_ (writeTQueue offersA) (announcingAndOffering fetchedEb announcingFetched)
        awaitAcquired next (AcquiredBody fetchedPoint)
        atomically $ readTVar started >>= check
        atomically $ case nodeAtomicFetch node of
          Nothing -> retry
          Just runtime -> do
            observation <- readNodeObservation runtime
            check (observedClosureStalls observation == 1)
            when partial $ do
              snapshots <- readNodeClosureSnapshots runtime
              let copies = ClosurePolicy.closureCopies <$> Map.lookup fetchedPoint snapshots
              check (maybe False (IntMap.notMember 0) copies)
        atomically $ mapM_ (writeTQueue offersB) (announcingAndOffering fetchedEb announcingFetched)
        awaitAcquired next (AcquiredClosure fetchedPoint)
        checkStored node fetchedPoint fetchedEb fetchedTxs
        observation <- atomicObservation node
        seen <- readTVarIO logVar
        usage <- bodyUsageOf node (PeerAddr 1)
        ensure (observedClosureStalls observation == 1) "queued tail falsely charged its own service timer"
        ensure (observedClosureRescues observation == 1) "closure rescue was missing or exceeded its slot"
        ensure
          (observedClosureHandoffs observation == if partial then 1 else 2)
          "productive rescue did not take over the whole-batch tail"
        ensure (observedPeakClosureCopies observation == 2) "wanted closure copy bound was not observed"
        ensure (observedClosureFailures observation == 0) "successful rescue charged a hedge failure"
        let rescued = [offsets | (False, offsets) <- seen]
        ensure
          (rescued == if partial then [[1], [2]] else [[0], [1], [2]])
          "replacement requested a verified prefix or lost a needed transaction"
        when partial $ do
          ensure
            (Admission.usedData usage > 0)
            "committed partial original released its reservation before wire drain"
          atomically $ writeTVar gate True
          atomically $ do
            usages <- case nodeAtomicFetch node of Nothing -> retry; Just runtime -> readNodeClientUsage runtime
            let active =
                  [u | (MkLeiosPeerLifetimeId conn _, Right u) <- Map.toList usages, remoteAddress conn == PeerAddr 1]
            check
              (not (null active) && all (\u -> Admission.usedData u == 0 && Admission.usedExchanges u == 0) active)
        pure True

closureSoleSource :: forall s. IOSim s Bool
closureSoleSource = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offers <- newTQueueIO
    initialRequest <- newTVarIO True
    let provider :: AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s)
        provider = AtomicServer.AtomicRequestHandler $ \case
          AtomicServer.ServeBody _ _ -> pure (AtomicServer.PreparedBody (encodeEndorserBlockSize fetchedEb) fetchedEb)
          AtomicServer.ServeTransactions _ _ bitmaps -> do
            shouldDelay <- atomically $ do
              before <- readTVar initialRequest
              writeTVar initialRequest False
              pure before
            when shouldDelay $ threadDelay 1.2
            let txs = V.fromList [fetchedTxs V.! off | off <- bitmapOffsets (NE.toList bitmaps)]
            pure (AtomicServer.PreparedTransactions (fromIntegral (V.length txs) * 103 + 8) txs)
        upstream = EnvPeer (feedingLeiosNotifyServer node offers) Nothing (bodyServing provider id) Nothing
    withNodeVsEnvConnection node (PeerAddr 1) upstream $ \_ -> do
      atomically $ mapM_ (writeTQueue offers) (announcingAndOffering fetchedEb announcingFetched)
      awaitAcquired next (AcquiredClosure fetchedPoint)
      checkStored node fetchedPoint fetchedEb fetchedTxs
      observation <- atomicObservation node
      ensure
        ( observedClosureStalls observation == 1
            && observedClosureRescues observation == 0
            && observedClosureCancels observation == 0
        )
        (show observation)
      pure True

closureRescueRotation :: forall s. IOSim s Bool
closureRescueRotation = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offers <- mapM (const newTQueueIO) [1 .. 3 :: Int]
    requests <- newTVarIO ([] :: [(Int, [Int], Time)])
    cancellationGate <- newTVarIO False
    let provider :: Int -> AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s)
        provider n = AtomicServer.AtomicRequestHandler $ \case
          AtomicServer.ServeBody _ _ -> pure (AtomicServer.PreparedBody (encodeEndorserBlockSize fetchedEb) fetchedEb)
          AtomicServer.ServeTransactions _ _ bitmaps -> do
            let offsets = bitmapOffsets (NE.toList bitmaps)
                txs = V.fromList [fetchedTxs V.! off | off <- offsets]
            now <- getMonotonicTime
            atomically $ modifyTVar requests (<> [(n, offsets, now)])
            when (n < 3) $ atomically retry
            pure (AtomicServer.PreparedTransactions (fromIntegral (V.length txs) * 103 + 8) txs)
        holdCancellation channel =
          channel
            { send = \bytes -> do
                atomically $ readTVar cancellationGate >>= check
                send channel bytes
            }
        connect [] action = action
        connect ((n, notifications) : rest) action =
          let upstream =
                EnvPeer
                  (feedingLeiosNotifyServer node notifications)
                  Nothing
                  (bodyServing (provider n) (if n == 2 then holdCancellation else id))
                  Nothing
           in withNodeVsEnvConnection node (PeerAddr n) upstream (const (connect rest action))
        publish n =
          atomically $
            mapM_ (writeTQueue (offers !! (n - 1))) (announcingAndOffering fetchedEb announcingFetched)
        requested n = atomically $ do
          seen <- readTVar requests
          check (any (\(p, _, _) -> p == n) seen)
    connect (zip [1 .. 3] offers) $ do
      publish 1
      requested 1
      publish 2
      requested 2
      publish 3
      threadDelay 0.1
      before <- readTVarIO requests
      ensure (all (\(n, _, _) -> n < 3) before) "a second rescue started before the first rescue's expiry"
      awaitAcquired next (AcquiredClosure fetchedPoint)
      checkStored node fetchedPoint fetchedEb fetchedTxs
      observation <- atomicObservation node
      debt <- bodyUsageOf node (PeerAddr 2)
      ensure
        (Admission.usedData debt == 1 && Admission.usedExchanges debt == 2)
        "canceled rescue was reused or uncharged before its original/ACK arrived"
      atomically $ writeTVar cancellationGate True
      atomically $ case nodeAtomicFetch node of
        Nothing -> retry
        Just runtime -> do
          usages <- readNodeClientUsage runtime
          let active =
                [u | (MkLeiosPeerLifetimeId conn _, Right u) <- Map.toList usages, remoteAddress conn == PeerAddr 2]
          check
            (not (null active) && all (\u -> Admission.usedData u == 0 && Admission.usedExchanges u == 0) active)
      seen <- readTVarIO requests
      ensure
        ([offsets | (3, offsets, _) <- seen] == [[1], [2], [0]])
        ("unexpected rotated rescue schedule: " ++ show (seen, observation))
      ensure
        ( observedClosureRescues observation == 3
            && observedClosureFailures observation == 1
            && observedClosureStalls observation == 1
            && observedPeakClosureCopies observation == 2
            && observedClosureHandoffs observation == 1
        )
        (show observation)
      pure True

retrySnapshotOf :: NodeUnderTest (IOSim s) -> LeiosPoint -> IOSim s Retry.RetrySnapshot
retrySnapshotOf node point = atomically $ case nodeAtomicFetch node of
  Nothing -> retry
  Just runtime -> do
    snapshots <- readNodeClosureSnapshots runtime
    maybe retry (pure . ClosurePolicy.closureRetry) (Map.lookup point snapshots)

-- Five silent, fully retired rescues exercise the actual admission/timing path.
-- Hold subsequent honest replies so pressure can be inspected before completion.
closureRetryCycles :: forall s. IOSim s Bool
closureRetryCycles = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offersA <- newTQueueIO
    offersB <- newTQueueIO
    primaryStarted <- newTVarIO False
    requests <- newTVarIO ([] :: [([Int], Time)])
    releaseSixth <- newTVarIO False
    releaseSeventh <- newTVarIO False
    let provider :: Bool -> AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s)
        provider primarySource = AtomicServer.AtomicRequestHandler $ \case
          AtomicServer.ServeBody _ _ -> pure (AtomicServer.PreparedBody (encodeEndorserBlockSize fetchedEb) fetchedEb)
          AtomicServer.ServeTransactions _ _ bitmaps -> do
            let offsets = bitmapOffsets (NE.toList bitmaps)
                txs = V.fromList [fetchedTxs V.! off | off <- offsets]
            if primarySource
              then atomically (writeTVar primaryStarted True) >> atomically retry
              else do
                now <- getMonotonicTime
                count <- atomically $ do
                  modifyTVar requests (<> [(offsets, now)])
                  length <$> readTVar requests
                if count <= 5
                  then atomically retry
                  else
                    when (count == 6 || count == 7) $
                      atomically $
                        readTVar (if count == 6 then releaseSixth else releaseSeventh) >>= check
            pure (AtomicServer.PreparedTransactions (fromIntegral (V.length txs) * 103 + 8) txs)
        upstream notifications primarySource =
          EnvPeer
            (feedingLeiosNotifyServer node notifications)
            Nothing
            (bodyServing (provider primarySource) id)
            Nothing
        waitRequests count = atomically $ do
          seen <- readTVar requests
          check (length seen >= count)
          pure seen
    withNodeVsEnvConnection node (PeerAddr 1) (upstream offersA True) $ \_ ->
      withNodeVsEnvConnection node (PeerAddr 2) (upstream offersB False) $ \_ -> do
        atomically $ mapM_ (writeTQueue offersA) (announcingAndOffering fetchedEb announcingFetched)
        awaitAcquired next (AcquiredBody fetchedPoint)
        atomically $ readTVar primaryStarted >>= check
        atomically $ mapM_ (writeTQueue offersB) (announcingAndOffering fetchedEb announcingFetched)
        cooling <- atomically $ case nodeAtomicFetch node of
          Nothing -> retry
          Just runtime -> do
            snapshots <- readNodeClosureSnapshots runtime
            case Map.lookup fetchedPoint snapshots of
              Just snapshot
                | Just cooldownAt <- Retry.snapshotCooldownUntil (ClosurePolicy.closureRetry snapshot) ->
                    pure cooldownAt
              _ -> retry
        atCooldown <- getMonotonicTime
        ensure
          (abs (diffTime cooling atCooldown - 1) < 0.001)
          "cooldown did not start for one second at the fifth failure"
        seen <- take 6 <$> waitRequests 6
        let gaps = zipWith (\(_, a) (_, b) -> diffTime b a) seen (tail seen)
        ensure
          (and (zipWith (\a b -> abs (a - b) < 0.001) gaps [1, 1.5, 2, 2, 3]))
          ("unexpected retry timing: " ++ show gaps)
        ensure
          (map fst seen == [[0], [1], [2], [0], [1], [2]])
          "failed rescue opportunities did not rotate FIFO"
        before <- retrySnapshotOf node fetchedPoint
        ensure
          ( Retry.snapshotFailuresInGroup before == 0
              && Retry.snapshotCooldownUntil before == Nothing
              && Retry.snapshotNextCooldown before == 2
          )
          "cooldown expiry reset its grouped history"
        let near actual expected = abs ((fromRational actual :: Double) - expected) < 1e-10
        ensure
          (near (Retry.snapshotRetryFactor before) (1.5 ^ (5 :: Int)))
          "future rescue pressure lost a failure"
        atomically $ writeTVar releaseSixth True
        void (waitRequests 7)
        recovered <- retrySnapshotOf node fetchedPoint
        ensure
          (near (Retry.snapshotRetryFactor recovered) (1.5 ^ (5 :: Int) * 0.7))
          "useful wire TX did not recover pressure by its actual bytes"
        ensure
          (Retry.snapshotNextCooldown recovered == 2 && Retry.snapshotFailuresInGroup recovered == 0)
          "useful service reset cooldown history"
        atomically $ writeTVar releaseSeventh True
        awaitAcquired next (AcquiredClosure fetchedPoint)
        checkStored node fetchedPoint fetchedEb fetchedTxs
        observation <- atomicObservation node
        ensure
          ( observedClosureFailures observation == 5
              && observedClosureStalls observation == 1
              && observedPeakClosureCopies observation == 2
          )
          (show observation)
        pure True

-- Local test knobs shorten the failure group and lengthen its first cooldown.
-- The other EB uses ordinary body/TX requests and must finish inside that wait.
closureCooldownIndependent :: forall s. IOSim s Bool
closureCooldownIndependent = do
  db <- newLeiosDBInMemory
  let policy = either (error . show) id (Retry.mkRetryConfig 16 1.5 0.7 1 4 4)
      config = atomicNodeConfig{nodeClosureRetryConfig = policy}
  withAtomicNodeOver config db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offers <- mapM (const newTQueueIO) [1 .. 3 :: Int]
    started <- newTVarIO False
    let otherPoint = MkLeiosPoint (blockSlot announcingHonest) (endorserBlockHash honestEb)
        provider :: Int -> AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s)
        provider _source = AtomicServer.AtomicRequestHandler $ \case
          AtomicServer.ServeBody _ point -> do
            let body = if point == fetchedPoint then fetchedEb else honestEb
            pure (AtomicServer.PreparedBody (encodeEndorserBlockSize body) body)
          AtomicServer.ServeTransactions _ point bitmaps -> do
            if point == fetchedPoint
              then atomically (writeTVar started True) >> atomically retry
              else do
                let txs = V.fromList [sharedTx | _ <- bitmapOffsets (NE.toList bitmaps)]
                pure (AtomicServer.PreparedTransactions (fromIntegral (V.length txs) * 53 + 8) txs)
        connect [] action = action
        connect ((n, notifications) : rest) action =
          let upstream =
                EnvPeer (feedingLeiosNotifyServer node notifications) Nothing (bodyServing (provider n) id) Nothing
           in withNodeVsEnvConnection node (PeerAddr n) upstream (const (connect rest action))
    threadDelay 4
    connect (zip [1 .. 3] offers) $ do
      atomically $ mapM_ (writeTQueue (head offers)) (announcingAndOffering fetchedEb announcingFetched)
      atomically $ readTVar started >>= check
      atomically $ mapM_ (writeTQueue (offers !! 1)) (announcingAndOffering fetchedEb announcingFetched)
      cooldownAt <- atomically $ case nodeAtomicFetch node of
        Nothing -> retry
        Just runtime -> do
          snapshots <- readNodeClosureSnapshots runtime
          case Map.lookup fetchedPoint snapshots of
            Just snapshot | Just at <- Retry.snapshotCooldownUntil (ClosurePolicy.closureRetry snapshot) -> pure at
            _ -> retry
      atomically $ mapM_ (writeTQueue (offers !! 2)) (announcingAndOffering honestEb announcingHonest)
      awaitAcquired next (AcquiredClosure otherPoint)
      now <- getMonotonicTime
      ensure (now < cooldownAt) "another EB's ordinary work was held behind retry cooldown"
      stillCooling <- retrySnapshotOf node fetchedPoint
      ensure
        (Retry.snapshotCooldownUntil stillCooling == Just cooldownAt)
        "unrelated EB service shortened or renewed cooldown"
      observation <- atomicObservation node
      ensure
        (observedBodyRequests observation == 2 && observedClosureFailures observation == 1)
        (show observation)
      pure True

-- The response is released 50 ms after its request. Atomic assignment must react
-- to durability promptly; the same stimulus must leave legacy cadence intact.
coordinatorAssignment :: forall s. Bool -> IOSim s Bool
coordinatorAssignment atomic = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offers <- newTQueueIO
    bodyAt <- newEmptyTMVarIO
    txAt <- newEmptyTMVarIO
    release <- newTVarIO False
    let bodyReady = do
          now <- getMonotonicTime
          atomically $ putTMVar bodyAt now
          atomically $ readTVar release >>= check
        txReady = do
          now <- getMonotonicTime
          atomically $ void (tryPutTMVar txAt now)
        newProvider :: AtomicServer.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (IOSim s)
        newProvider = AtomicServer.AtomicRequestHandler $ \case
          AtomicServer.ServeBody _ _ -> bodyReady >> pure (AtomicServer.PreparedBody (encodeEndorserBlockSize fetchedEb) fetchedEb)
          AtomicServer.ServeTransactions _ _ bitmaps -> do
            txReady
            let txs = V.fromList [fetchedTxs V.! off | off <- bitmapOffsets (NE.toList bitmaps)]
            pure (AtomicServer.PreparedTransactions (fromIntegral (V.length txs) * 103 + 8) txs)
        oldProvider = MkLeiosFetchRequestHandler $ \request -> do
          (case request of MsgLeiosBlockRequest{} -> bodyReady; MsgLeiosBlockTxsRequest{} -> txReady) ::
            IOSim s ()
          case serveEb fetchedEb fetchedTxs of { MkLeiosFetchRequestHandler serve -> serve request }
        oldServer channel =
          void $
            runAnnotatedPeer nullTracer (envLeiosFetchCodec node) channel (leiosFetchServerPeer oldProvider)
        peer =
          EnvPeer
            (feedingLeiosNotifyServer node offers)
            Nothing
            (if atomic then bodyServing newProvider id else oldServer)
            Nothing
        connect = if atomic then withNodeVsEnvConnection else withNodeVsEnvConnectionVersion NodeToNodeV_16
    connect node (PeerAddr 1) peer $ \_ -> do
      atomically $ mapM_ (writeTQueue offers) (announcingAndOffering fetchedEb announcingFetched)
      started <- atomically (readTMVar bodyAt)
      threadDelay 0.05
      atomically $ writeTVar release True
      awaitAcquired next (AcquiredClosure fetchedPoint)
      requested <- atomically (readTMVar txAt)
      let elapsed = diffTime requested started
      ensure
        (if atomic then elapsed < 0.2 else elapsed >= 0.5 && elapsed < 0.7)
        ("unexpected assignment latency: " ++ show elapsed)
      checkStored node fetchedPoint fetchedEb fetchedTxs
      pure True

coordinatorReadinessFlood :: IOSim s Bool
coordinatorReadinessFlood = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    a <- newTQueueIO
    b <- newTQueueIO
    logVar <- newTVarIO []
    let upstream queue addr delay =
          EnvPeer
            (feedingLeiosNotifyServer node queue)
            Nothing
            (bodyServing (bodyProvider logVar addr delay) id)
            Nothing
        ready = lfcReady (lfkContext (getLeiosFetchKernel (nodeKernel node)))
    withNodeVsEnvConnection node (PeerAddr 1) (upstream a (PeerAddr 1) Nothing) $ \_ ->
      withNodeVsEnvConnection node (PeerAddr 2) (upstream b (PeerAddr 2) (Just 0)) $ \_ -> do
        bodyOffer node a
        seen <- waitBodyRequests 1 logVar
        bodyOffer node b
        withAsync (replicateM_ 150 (threadDelay 0.01 >> atomically (FetchWait.notifyReadiness ready))) $ \_ -> do
          awaitAcquired next (AcquiredBody fetchedPoint)
          now <- getMonotonicTime
          ensure (diffTime now (snd (head seen)) <= 1.05) "readiness traffic postponed the body deadline"
          observation <- atomicObservation node
          pure (observedBodyTriggers observation == 1 && observedBodyHedges observation == 1)

coordinatorClientCleanup :: forall s. Bool -> IOSim s Bool
coordinatorClientCleanup oldLegacy = do
  db <- newLeiosDBInMemory
  withAtomicNodeOver atomicNodeConfig db $ \node -> case nodeAtomicFetch node of
    Nothing -> throwIO (userError "atomic runtime missing")
    Just runtime -> do
      let kernel = getLeiosFetchKernel (nodeKernel node)
          conn = ConnectionId (PeerAddr 0) (PeerAddr 99)
      withLeiosFetchPeer kernel conn IsNotBigLedgerPeer $ \lifetime _ -> do
        entered <- newEmptyTMVarIO
        release <- newEmptyTMVarIO
        let oldAction = atomically (putTMVar entered ()) >> atomically (readTMVar release)
            oldScope =
              if oldLegacy
                then withNodeLegacyClient runtime lifetime oldAction
                else withNodeAtomicClient runtime (PeerAddr 99) lifetime 64 Nothing (const oldAction)
        withAsync oldScope $ \old -> do
          atomically (readTMVar entered)
          duplicate <- try (withNodeAtomicClient runtime (PeerAddr 99) lifetime 64 Nothing (const (pure ())))
          ensure
            (case duplicate of Left FetchClientAlreadyRegistered -> True; _ -> False)
            "overlapping client was not rejected"
          atomically $ do
            extension <- IOLike.readTVar (lfkExtension kernel)
            maybe (throwSTM (userError "extension missing")) (\e -> extensionRemovePeer e lifetime) extension
          withNodeAtomicClient runtime (PeerAddr 99) lifetime 64 Nothing $ \fresh -> do
            atomically $ putTMVar release ()
            wait old
            state <- atomically (AtomicClient.readClientSnapshot fresh)
            ensure (not (AtomicClient.snapshotClientClosed state)) "old finalizer closed its replacement"
            usage <- atomically (readNodeClientUsage runtime)
            ensure (Map.size usage == 1) "old finalizer removed the replacement registration"
            pure True

coordinatorFocusRefocus :: IOSim s Bool
coordinatorFocusRefocus = do
  db <- newLeiosDBInMemory
  let config =
        atomicNodeConfig
          { nodeStorageLimits = (nodeStorageLimits atomicNodeConfig){Handoff.storageMaxAcquisitions = 1}
          }
  withAtomicNodeOver config db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    a <- newTQueueIO
    b <- newTQueueIO
    logVar <- newTVarIO []
    sends <- newTVarIO 0
    releaseBody <- newTVarIO False
    releaseAck <- newTVarIO True
    let kernel = getLeiosFetchKernel (nodeKernel node)
        context = lfkContext kernel
        upstream queue addr adjust =
          EnvPeer
            (feedingLeiosNotifyServer node queue)
            Nothing
            (bodyServing (bodyProvider logVar addr (Just 0)) adjust)
            Nothing
    withNodeVsEnvConnection
      node
      (PeerAddr 1)
      (upstream a (PeerAddr 1) (committedBodyChannel sends releaseBody releaseAck))
      $ \_ ->
        withNodeVsEnvConnection node (PeerAddr 2) (upstream b (PeerAddr 2) id) $ \_ -> do
          bodyOffer node a
          void (waitBodyRequests 1 logVar)
          atomically $ readTVar sends >>= check . (>= 1)
          original <- bodySnapshotOf node
          focused <- IOLike.modifyMVar (lfcOutstanding context) $ \state -> do
            atomically $ FetchWait.notifyReadiness (lfcReady context)
            pure (state{elFocus = Map.empty}, elFocus state)
          atomically $ case nodeAtomicFetch node of
            Nothing -> retry
            Just runtime -> readNodeBodySnapshots runtime >>= check . Map.notMember fetchedPoint
          IOLike.modifyMVar_ (lfcOutstanding context) $ \state -> do
            atomically $ FetchWait.notifyReadiness (lfcReady context)
            pure state{elFocus = focused}
          bodyOffer node b
          awaitAcquired next (AcquiredBody fetchedPoint)
          replacement <- bodySnapshotOf node
          ensure
            (BodyPolicy.bodyGeneration replacement /= BodyPolicy.bodyGeneration original)
            "refocus reused the old acquisition generation"
          atomically $ writeTVar releaseBody True
          atomically $ case nodeAtomicFetch node of
            Nothing -> retry
            Just runtime -> do
              usages <- readNodeClientUsage runtime
              check
                ( all
                    (\u -> case u of Right x -> Admission.usedData x == 0 && Admission.usedExchanges x == 0; _ -> False)
                    (Map.elems usages)
                )
          duplicate <- timeout 0.1 (atomically next)
          ensure
            (case duplicate of Nothing -> True; _ -> False)
            "old generation emitted a duplicate acquisition"
          final <- bodySnapshotOf node
          pure
            ( BodyPolicy.bodyGeneration final == BodyPolicy.bodyGeneration replacement
                && BodyPolicy.bodyFulfilled final
            )

-- Exercise the real immutable-tip pruner. Before pruning the earlier point
-- occupies the only arena, so the later point must wait for its retirement.
coordinatorPrunes :: IOSim s Bool
coordinatorPrunes = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  fillLeiosDb serverDb refocusedPoint fetchedEb fetchedTxs
  let config =
        atomicNodeConfig
          { nodeStorageLimits = (nodeStorageLimits atomicNodeConfig){Handoff.storageMaxAcquisitions = 1}
          }
  withAtomicNodeOver config db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offers <- newTQueueIO
    let upstream = EnvPeer (feedingLeiosNotifyServer node offers) Nothing (atomicServing serverDb) Nothing
    -- Let these chain blocks' slots pass so none is from the future.
    threadDelay 20
    withNodeVsEnvConnection node (PeerAddr 1) upstream $ \_ -> do
      atomically $
        mapM_
          (writeTQueue offers . MsgLeiosBlockAnnouncement . ltbHeader)
          [announcingFetched, reannouncingFetched]
      atomically $ case nodeAtomicFetch node of
        Nothing -> retry
        Just runtime -> do
          bodies <- readNodeBodySnapshots runtime
          check (Map.member fetchedPoint bodies && Map.notMember refocusedPoint bodies)
      addBlocks node $ announcingFetched : take 11 (iterate successorLeiosBlock reannouncingFetched)
      awaitPrunedPast node (blockSlot announcingFetched)
      atomically $
        mapM_
          (writeTQueue offers)
          [ MsgLeiosBlockOffer refocusedPoint (encodeEndorserBlockSize fetchedEb)
          , MsgLeiosBlockTxsOffer refocusedPoint
          ]
      awaitAcquired next (AcquiredBody refocusedPoint)
      awaitAcquired next (AcquiredClosure refocusedPoint)
      checkStored node refocusedPoint fetchedEb fetchedTxs
      pure True

-- Hold a real writer receipt after submission. The raw DB may already contain
-- the bytes, but possession/notification and the reservation await this gate.
holdBodyReceipt ::
  LeiosDbHandle (IOSim s) ->
  StrictTVar (IOSim s) Bool ->
  StrictTVar (IOSim s) Bool ->
  LeiosDbHandle (IOSim s)
holdBodyReceipt db entered release =
  db
    { openWriter = do
        writer <- openWriter db
        pure
          writer
            { writeEbBody = \point body -> do
                promise <- writeEbBody writer point body
                atomically $ writeTVar entered True
                pure (Promise (atomically (readTVar release >>= check) >> await promise))
            }
    }

coordinatorReconnectStorage :: IOSim s Bool
coordinatorReconnectStorage = do
  db <- newLeiosDBInMemory
  submitted <- newTVarIO False
  release <- newTVarIO False
  withAtomicNodeOver atomicNodeConfig (holdBodyReceipt db submitted release) $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    serverDb <- newLeiosDBInMemory
    fillLeiosDb serverDb fetchedPoint fetchedEb fetchedTxs
    a <- newTQueueIO
    b <- newTQueueIO
    let upstream queue = EnvPeer (feedingLeiosNotifyServer node queue) Nothing (atomicServing serverDb) Nothing
        snapshot = case nodeAtomicFetch node of
          Nothing -> throwSTM (userError "runtime missing")
          Just runtime -> readNodeStorageSnapshot runtime
    old <- withNodeVsEnvConnection node (PeerAddr 1) (upstream a) $ \_ -> do
      bodyOffer node a
      atomically $ readTVar submitted >>= check
      atomically $ case nodeAtomicFetch node of
        Nothing -> retry
        Just runtime -> Map.keys <$> readNodeClientUsage runtime
    drained <- atomically snapshot
    ensure
      (Handoff.snapshotDrainingSessions drained == 1 && Handoff.snapshotClaims drained == 1)
      "disconnect refunded a writing body"
    withNodeVsEnvConnection node (PeerAddr 1) (upstream b) $ \_ -> do
      bodyOffer node b
      fresh <- atomically $ case nodeAtomicFetch node of
        Nothing -> retry
        Just runtime -> do
          usage <- readNodeClientUsage runtime
          check (not (Map.null usage))
          pure (Map.keys usage)
      ensure (old /= fresh) "same-address reconnect reused the peer lifetime"
      both <- atomically snapshot
      ensure
        (Handoff.snapshotSessions both == 2 && Handoff.snapshotDrainingSessions both == 1)
        "old storage debt was not retained beside the new session"
      atomically $ writeTVar release True
      awaitAcquired next (AcquiredBody fetchedPoint)
      atomically $ writeTQueue b (MsgLeiosBlockTxsOffer fetchedPoint)
      awaitAcquired next (AcquiredClosure fetchedPoint)
      checkStored node fetchedPoint fetchedEb fetchedTxs
      settled <- atomically snapshot
      ensure (Handoff.snapshotDrainingSessions settled == 0) "settled old session remained charged"
      observation <- atomicObservation node
      pure (observedAtomicConnections observation == 2 && observedBodyRequests observation == 1)

coordinatorShutdownStorage :: IOSim s Bool
coordinatorShutdownStorage = do
  db <- newLeiosDBInMemory
  submitted <- newTVarIO False
  release <- newTVarIO False
  scopeEnding <- newEmptyTMVarIO
  withAsync
    ( withAtomicNodeOver atomicNodeConfig (holdBodyReceipt db submitted release) $ \node -> do
        offers <- newTQueueIO
        logVar <- newTVarIO []
        let upstream =
              EnvPeer
                (feedingLeiosNotifyServer node offers)
                Nothing
                (bodyServing (bodyProvider logVar (PeerAddr 1) (Just 0)) id)
                Nothing
        withNodeVsEnvConnection node (PeerAddr 1) upstream $ \_ -> do
          bodyOffer node offers
          atomically $ readTVar submitted >>= check
        atomically $ putTMVar scopeEnding ()
    )
    $ \task -> do
      atomically (readTMVar scopeEnding)
      early <- timeout 0.05 (wait task)
      ensure (case early of Nothing -> True; _ -> False) "shutdown abandoned an accepted storage write"
      atomically $ writeTVar release True
      wait task
      withReader db $ \reader -> do
        body <- lookupEbBody reader fetchedPoint
        ensure (body == Just (endorserBlockBytes fetchedEb)) "drained body was not durable"
      pure True

-- Use the production assembly helper, defaults and normal supported map; the
-- fixture does not add version 17 through the old experimental-map adapter.
atomicDefaultActivation :: Bool -> IOSim s Bool
atomicDefaultActivation larger = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  fillLeiosDb serverDb fetchedPoint fetchedEb fetchedTxs
  let config = defaultAtomicNodeConfig{nodeResponseBytes = if larger then 8388608 else 1048576}
      limitOf version dataValue apps =
        case withHot (NTN.initiator mempty defaultMiniProtocolParameters version dataValue apps) of
          WithHot protocols -> case [ maximumIngressQueue (miniProtocolLimits mini)
                                    | mini <- protocols
                                    , miniProtocolNum mini == leiosFetchMiniProtocolNum
                                    ] of
            [found] -> found
            _ -> error "fetch mini-protocol missing"
  withAtomicNodeOver config db $ \node -> do
    let apps = nodeApps node ()
        expected = if larger then 8462336 else 4263936
        normal = nodeVersionData NodeToNodeV_17
    ensure (limitOf NodeToNodeV_17 normal apps == expected) "atomic mux ignored the response budget"
    ensure
      ( limitOf NodeToNodeV_16 (nodeVersionData NodeToNodeV_16) apps
          == maximumIngressQueue FetchAssembly.leiosFetchProtocolLimits
      )
      "legacy ingress budget changed"
    let smallD = normal{leiosFetchRequestLimit = mkLeiosFetchRequestLimit 4}
        expectedSmall =
          either
            (error . show)
            FetchTransport.atomicMuxIngressBytes
            (FetchTransport.atomicIngressLimits (nodeResponseBytes config) 4)
    ensure (limitOf NodeToNodeV_17 smallD apps == expectedSmall) "mux ignored the negotiated count"
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offers <- newTQueueIO
    let upstream = EnvPeer (feedingLeiosNotifyServer node offers) Nothing (atomicServing serverDb) Nothing
    withNodeVsEnvConnection node (PeerAddr 1) upstream $ \connection -> do
      ensure (connVersion connection == NodeToNodeV_17) "normal map did not negotiate version 17"
      atomically $ mapM_ (writeTQueue offers) (announcingAndOffering fetchedEb announcingFetched)
      awaitAcquired next (AcquiredClosure fetchedPoint)
      checkStored node fetchedPoint fetchedEb fetchedTxs
      observation <- atomicObservation node
      pure
        ( observedAtomicConnections observation == 1
            && observedBodyRequests observation == 1
            && observedCheckpoints observation == 1
        )

-- Keep the first announcement focused and unpruned, but make only the later
-- election fetchable. One acquisition slot must suffice without a lost wakeup.
atomicReclaimsEmpty :: IOSim s Bool
atomicReclaimsEmpty = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  fillLeiosDb serverDb refocusedPoint fetchedEb fetchedTxs
  let config =
        defaultAtomicNodeConfig
          { nodeStorageLimits = (nodeStorageLimits defaultAtomicNodeConfig){Handoff.storageMaxAcquisitions = 1}
          }
  withAtomicNodeOver config db $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    offers <- newTQueueIO
    let upstream = EnvPeer (feedingLeiosNotifyServer node offers) Nothing (atomicServing serverDb) Nothing
    threadDelay 2
    withNodeVsEnvConnection node (PeerAddr 1) upstream $ \_ -> do
      atomically $ writeTQueue offers (MsgLeiosBlockAnnouncement (ltbHeader announcingFetched))
      atomically $ case nodeAtomicFetch node of
        Just runtime -> readNodeBodySnapshots runtime >>= check . Map.member fetchedPoint
        Nothing -> retry
      atomically $ mapM_ (writeTQueue offers) (announcingAndOffering fetchedEb reannouncingFetched)
      awaitAcquired next (AcquiredClosure refocusedPoint)
      checkStored node refocusedPoint fetchedEb fetchedTxs
      outstanding <- readOutstanding node
      ensure
        (outstandingPrunedSlot outstanding <= blockSlot announcingFetched)
        "fixture pruned away the empty arena"
      pure (Map.member fetchedPoint (ebState outstanding))

-- Resource reclamation must not become a second stall/preemption policy.
atomicPreservesWanted :: IOSim s Bool
atomicPreservesWanted = atomicPreservesActive False

atomicPreservesAccepted :: IOSim s Bool
atomicPreservesAccepted = atomicPreservesActive True

atomicPreservesActive :: Bool -> IOSim s Bool
atomicPreservesActive waitingStorage = do
  db <- newLeiosDBInMemory
  serverDb <- newLeiosDBInMemory
  fillLeiosDb serverDb fetchedPoint fetchedEb fetchedTxs
  fillLeiosDb serverDb refocusedPoint fetchedEb fetchedTxs
  submitted <- newTVarIO False
  release <- newTVarIO False
  let config =
        defaultAtomicNodeConfig
          { nodeStorageLimits = (nodeStorageLimits defaultAtomicNodeConfig){Handoff.storageMaxAcquisitions = 1}
          }
      localDb = if waitingStorage then holdBodyReceipt db submitted release else db
  withAtomicNodeOver config localDb $ \node -> do
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    a <- newTQueueIO
    b <- newTQueueIO
    sends <- newTVarIO 0
    releaseAck <- newTVarIO True
    let upstream queue adjust = EnvPeer (feedingLeiosNotifyServer node queue) Nothing (atomicServing serverDb . adjust) Nothing
    threadDelay 2
    withNodeVsEnvConnection
      node
      (PeerAddr 1)
      (upstream a (if waitingStorage then id else committedBodyChannel sends release releaseAck))
      $ \_ ->
        withNodeVsEnvConnection node (PeerAddr 2) (upstream b id) $ \_ -> do
          atomically $ mapM_ (writeTQueue a) (announcingAndOffering fetchedEb announcingFetched)
          if waitingStorage
            then atomically (readTVar submitted >>= check)
            else atomically (readTVar sends >>= check . (>= 1))
          original <- bodySnapshotOf node
          atomically $ mapM_ (writeTQueue b) (announcingAndOffering fetchedEb reannouncingFetched)
          threadDelay 0.1
          atomically $ case nodeAtomicFetch node of
            Nothing -> retry
            Just runtime -> do
              bodies <- readNodeBodySnapshots runtime
              let retained = BodyPolicy.bodyGeneration <$> Map.lookup fetchedPoint bodies
              unless
                (retained == Just (BodyPolicy.bodyGeneration original) && Map.notMember refocusedPoint bodies)
                (throwSTM (userError "arena pressure evicted active body service"))
          atomically $ writeTVar release True
          awaitAcquired next (AcquiredClosure fetchedPoint)
          awaitAcquired next (AcquiredClosure refocusedPoint)
          checkStored node refocusedPoint fetchedEb fetchedTxs
          pure True
