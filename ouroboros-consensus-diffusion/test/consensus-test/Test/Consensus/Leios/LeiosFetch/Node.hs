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
import Control.Monad (forM_, replicateM, unless, void)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.Except (runExcept)
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
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Maybe.Strict (StrictMaybe (SNothing))
import qualified Data.Vector.Strict as V
import Data.Void (Void)
import Network.TypedProtocol.Codec (PeerRole (..))
import Network.TypedProtocol.Core (IsPipelined (..), ReflRelativeAgency (..))
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.Block
  ( Header
  , Point
  , Proxy (..)
  , SlotNo (..)
  , WithOrigin (NotOrigin)
  , blockHash
  , blockPoint
  , blockSlot
  , withOrigin
  , pattern GenesisPoint
  )
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Leios.Acquisitions (LeiosAcquisitions (..), LeiosEbNotification (..))
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
  , hashLeiosTx
  , mkEndorserBlock
  , toRankingBlockHash
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( ExnLeiosFetchInvalidReply (..)
  , LeiosFetchClientContext (..)
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosOutstanding (..)
  , PeerOffer (..)
  , WhetherTxsClosureOffered (..)
  , isFocusedEb
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
  ( ExnLeiosTwoCertificationClaims (..)
  , LeiosFetchKernel (..)
  , LeiosFetchPeer (..)
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
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (ancElectionId)
  , mkAnnouncingHeader
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( ExnLeiosNotifyInvalidOffer (..)
  , LeiosNotifyPeerState (..)
  , checkLeiosBlockOffer
  , checkLeiosClosureOffer
  , emptyLeiosNotifyPeerState
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
  ( LeiosTestBlock (..)
  , firstLeiosBlock
  , forkLeiosBlock
  , setAnnouncement
  , setLeiosCert
  , successorLeiosBlock
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ChainDB (emptyNodeDBs)

tests :: TestTree
tests =
  testGroup
    "node vs environment"
    [ testCase "the node's server ends when the peer sends MsgDone" $
        runSimFinishing (nodeServerAgainst doneClient) >>= (@?= ServerEnded)
    , testCase "the node serves an endorser block it holds" $
        runSimFinishing (nodeAnswers (requestBlock heldPoint))
          >>= (@?= (Just (ReplyBlock heldEb), ServerEnded))
    , testCase "the node serves the transactions it holds" $
        runSimFinishing (nodeAnswers (requestTxs heldPoint (bitmapsNaming [0, 63, 64])))
          >>= (@?= (Just (ReplyTxs (map heldTx [0, 63, 64])), ServerEnded))
    , testCase "the node disconnects a client that requests an endorser block it does not hold" $
        runSimFinishing (nodeAnswers (requestBlock unknownPoint))
          >>= (@?= (Nothing, ServerFailed (show (ExnLeiosUnknownBlockRequested unknownPoint))))
    , testCase "the node disconnects a client that requests transactions it does not hold" $
        -- The first is held, the second is not, and the third is past the end
        -- of the endorser block.
        runSimFinishing (nodeAnswers (requestTxs heldPoint (bitmapsNaming [0, 69, 100])))
          >>= (@?= (Nothing, ServerFailed (show (ExnLeiosUnknownTxsRequested heldPoint [69, 100]))))
    , testCase
        "the node fetches an endorser block and its closure that its peer offers, and offers them on"
        $ runSimFinishing nodeFetchesOffered
          >>= ( @?=
                  ( [AcquiredBody fetchedPoint, AcquiredClosure fetchedPoint]
                  ,
                    [ RelayedAnnouncement
                    , RelayedBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
                    , RelayedBlockTxsOffer fetchedPoint
                    ]
                  )
              )
    , testCase
        "the node announces its acquisition again, and offers it there, when a later election focuses an endorser block it holds"
        $ runSimFinishing nodeReacquiresHeld
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
                    , RelayedBlockOffer refocusedPoint (encodeEndorserBlockSize fetchedEb)
                    , RelayedBlockTxsOffer refocusedPoint
                    ]
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
    , testCase "the node records no offer older than the slot it pruned its peers' offers to" $
        runSimFinishing nodeIgnoresPrunedOffer >>= (@?= [SlotNo 5])
    , testCase
        "the node disconnects a peer that rolls forward certificates of two endorser blocks for one election"
        $ runSimFinishing nodeDisconnectsDoubleCertifier
          >>= (@?= Just (endorserBlockHash fetchedEb, otherEbHash))
    , testCase
        "the node selects a block certifying an endorser block it acquired at an earlier election"
        $ runSimFinishing (nodeSelectsReannounced NothingPruned)
          >>= (@?= lastPoint (reannouncedChain NothingPruned))
    , testCase
        "the node selects a block certifying an endorser block it acquired, pruned, and saw announced again"
        $ runSimFinishing (nodeSelectsReannounced PruneThenReannounce)
          >>= (@?= lastPoint (reannouncedChain PruneThenReannounce))
    , testCase
        "the node selects a block certifying an endorser block it acquired and saw announced again, once it prunes the first announcement"
        $ runSimFinishing (nodeSelectsReannounced ReannounceThenPrune)
          >>= (@?= lastPoint (reannouncedChain ReannounceThenPrune))
    , testCase
        "the node keeps an endorser block that a certificate refocuses an election onto through garbage collection and a restart"
        $ runSimFinishingWithin 200 nodeKeepsRefocusedThroughGc
          >>= (@?= lastPoint gcRestartChain)
    , testCase
        "the node fetches an endorser block a later election announced, once it prunes the earlier announcement"
        $ runSimFinishing nodeFetchesAfterPruningEarlier
          >>= (@?= [AcquiredBody refocusedPoint, AcquiredClosure refocusedPoint])
    , testCase
        "the node selects a block certifying an endorser block though a rival announcement took the focus first"
        $ runSimFinishing (nodeSelectsCertifiedRival CertifyingAfterAnnouncing)
          >>= (@?= (True, blockPoint certifyingFetched))
    , testCase
        "the node selects a block certifying an endorser block though a rival announcement took the focus first and the certifying block arrived first"
        $ runSimFinishing (nodeSelectsCertifiedRival CertifyingBeforeAnnouncing)
          >>= (@?= (True, blockPoint certifyingFetched))
    , testCase
        "the node fetches an endorser block at the size its peer offers, though the announcement states another"
        $ runSimFinishing nodeFetchesAtOfferedSize
          >>= (@?= [AcquiredBody fetchedPoint, AcquiredClosure fetchedPoint])
    , testCase
        "the node selects a block certifying an endorser block that another election announced at another size"
        $ runSimFinishing nodeSelectsDespiteMisstatedSize
          >>= (@?= blockPoint certifyingFetched)
    , testCase
        "the node disconnects a peer that serves an endorser block referencing more transaction bytes than the ledger allows"
        $ runSimFinishing (nodeFetchClientFails (oneTxEb (maxEbBodySize + 1)))
          >>= ( @?=
                  Just
                    ( show
                        ( ExnLeiosClosureTooBig
                            (oneTxEbPoint (maxEbBodySize + 1))
                            (fromIntegral maxEbBodySize + 1)
                            maxEbBodySize
                        )
                    )
              )
    , testCase
        "the node fetches an endorser block referencing as many transaction bytes as the ledger allows"
        $ runSimFinishing (nodeFetchesOneTxEb (oneTxEb maxEbBodySize))
          >>= ( @?=
                  [ AcquiredBody (oneTxEbPoint maxEbBodySize)
                  , AcquiredClosure (oneTxEbPoint maxEbBodySize)
                  ]
              )
    , testCase
        "the node does not acquire an endorser block larger than the ledger allows, though it references few transaction bytes"
        $ runSimFinishing nodeRefusesOversizedEb
          >>= ( @?=
                  ( Just
                      ( show
                          ( ExnLeiosBlockWrongSize
                              oversizedPoint
                              maxEbBodySize
                              (encodeEndorserBlockSize (fst oversizedEb))
                          )
                      )
                  , Nothing
                  )
              )
    , testCase
        "the node selects a block certifying an endorser block once the block arrives again after a restart forgot it"
        $ runSimFinishing nodeReselectsForgotten
          >>= (@?= ((1, True), (True, 0, lastPoint forgottenChain)))
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
        "the node does not offer the closure of an endorser block that misstates the size of a transaction the node already holds"
        $ runSimFinishing (nodeOffersLyingEb TxBeforeBody) >>= (@?= (True, False))
    , testCase
        "the node does not offer the closure of an endorser block that misstates the size of a transaction the node fetches afterwards"
        $ runSimFinishing (nodeOffersLyingEb BodyBeforeTx) >>= (@?= (True, False))
    , testCase "the node runs LeiosFetch from NodeToNodeV_16 on" $
        runSimFinishing (nodeRunsLeiosFetch NodeToNodeV_16) >>= (@?= (True, True))
    , testCase "the node does not run LeiosFetch before NodeToNodeV_16" $
        runSimFinishing (nodeRunsLeiosFetch NodeToNodeV_15) >>= (@?= (False, False))
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
  | -- | Something a receiving node would disconnect the node for, and why.
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

-- | The peer announces 'fetchedEb', offers its body and its closure, and
-- serves both. The node's acquisitions, as it announces them, and the first
-- three notifications it relays to the peer.
nodeFetchesOffered :: IOSim s ([Acquired], [Relayed])
nodeFetchesOffered = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  relayed <- newTVarIO []
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
          , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut relayed)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    acquired <- replicateM 2 $ acquiredOf <$> atomically next
    relays <- atomically $ readTVar relayed >>= \rs -> take 3 rs <$ check (length rs >= 3)
    pure (acquired, relays)

-- | 'nodeFetchesOffered', after which the peer announces 'fetchedEb' again, in
-- 'reannouncingFetched', and offers nothing more. The node's acquisitions, as it
-- announces them, and the first six notifications it relays to the peer.
nodeReacquiresHeld :: IOSim s ([Acquired], [Relayed])
nodeReacquiresHeld = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  relayed <- newTVarIO []
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
          , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut relayed)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    acquired <- replicateM 2 $ acquiredOf <$> atomically next
    atomically $
      writeTQueue notifications (MsgLeiosBlockAnnouncement (ltbHeader reannouncingFetched))
    reacquired <- replicateM 2 $ acquiredOf <$> atomically next
    relays <- atomically $ readTVar relayed >>= \rs -> take 6 rs <$ check (length rs >= 6)
    pure (acquired <> reacquired, relays)

-- | 'nodeFetchesOffered', after which the peer announces 'otherEbHash' and then
-- 'fetchedEb' in a later election, so that the former takes its focus, and the
-- node's ChainDB then verifies a certificate of the latter. The node's
-- acquisitions, as it announces them, and the first seven notifications it
-- relays to the peer.
nodeReacquiresCertified :: IOSim s ([Acquired], [Relayed])
nodeReacquiresCertified = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  relayed <- newTVarIO []
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
          , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut relayed)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    acquired <- replicateM 2 $ acquiredOf <$> atomically next
    atomically $
      mapM_
        (writeTQueue notifications . MsgLeiosBlockAnnouncement . ltbHeader)
        [reannouncingOther, reannouncingFetched]
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar relayed >>= check . (>= 5) . length
    addBlocks nut [announcingFetched, reannouncingFetched, certifying reannouncingFetched]
    reacquired <- replicateM 2 $ acquiredOf <$> atomically next
    relays <- atomically $ readTVar relayed >>= \rs -> take 7 rs <$ check (length rs >= 7)
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
-- node's acquisitions, as it announces them, and the first three notifications
-- it relays to the peer.
nodeFetchesRolledForward :: IOSim s ([Acquired], [Relayed])
nodeFetchesRolledForward = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  relayed <- newTVarIO []
  let envPeer =
        EnvPeer
          { epChainSyncServer =
              scriptedChainSyncServer $
                SendRollForward . ltbHeader <$> [announcingFetched, certifyingFetched]
          , epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut relayed)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    acquired <- replicateM 2 $ acquiredOf <$> atomically next
    relays <- atomically $ readTVar relayed >>= \rs -> take 3 rs <$ check (length rs >= 3)
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
  IOLike.atomically $ IOLike.writeTVar (lfkPrunedSlot kernel) (SlotNo 5)
  withLeiosFetchPeer kernel (ConnectionId (PeerAddr 0) (PeerAddr 1)) IsNotBigLedgerPeer $ \peerVars -> do
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

-- | The peer announces 'fetchedEb' in 'announcingFetched', offers it, and
-- serves it. Once the node has acquired it, the node adds 'gcRestartChain',
-- whose certificate moves the second announcement's election onto
-- 'fetchedEb', though nothing announced it to the node. Then the node waits
-- out a garbage collection of its LeiosDB to its immutable tip, which is past
-- 'announcingFetched', and restarts. The tip of its selection after the
-- restart, once that is the chain's last block or ten seconds have passed.
nodeKeepsRefocusedThroughGc :: IOSim s (Point LeiosTestBlock)
nodeKeepsRefocusedThroughGc = do
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
      addBlocks nut gcRestartChain
      -- The acquisition announced again, at the certified announcement.
      _ <- replicateM 2 $ atomically next
      _ <- awaitTip nut (lastPoint gcRestartChain)
      -- Past the ChainDB's delay before it collects garbage, and its interval.
      threadDelay 80
  withNodeOverDBs nodeDBs leiosDb $ \nut -> awaitTip nut (lastPoint gcRestartChain)

-- | The peer announces 'fetchedEb' in 'announcingFetched' and again in
-- 'reannouncingFetched', and offers nothing. The node adds a chain of twelve
-- blocks from 'announcingFetched', which puts its immutable tip at
-- 'reannouncingFetched', and prunes. Only then does the peer offer 'fetchedEb',
-- at the second announcement, and serve it. The node's acquisitions, as it
-- announces them.
nodeFetchesAfterPruningEarlier :: IOSim s [Acquired]
nodeFetchesAfterPruningEarlier = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  relayed <- newTVarIO []
  notifications <- newTQueueIO
  atomically $
    mapM_
      (writeTQueue notifications . MsgLeiosBlockAnnouncement . ltbHeader)
      [announcingFetched, reannouncingFetched]
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut relayed)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar relayed >>= check . (>= 2) . length
    addBlocks nut $ announcingFetched : take 11 (iterate successorLeiosBlock reannouncingFetched)
    awaitPrunedPast nut (blockSlot announcingFetched)
    atomically $
      mapM_
        (writeTQueue notifications)
        [ MsgLeiosBlockOffer refocusedPoint (encodeEndorserBlockSize fetchedEb)
        , MsgLeiosBlockTxsOffer refocusedPoint
        ]
    replicateM 2 $ acquiredOf <$> atomically next

-- | Which of 'announcingFetched' and 'certifyingFetched' the node adds first.
data CertifyingArrival = CertifyingAfterAnnouncing | CertifyingBeforeAnnouncing

-- | The peer announces 'otherEbHash' in 'announcingOther', and then 'fetchedEb'
-- in 'announcingFetched', for the same election, so that the former takes its
-- focus. It offers 'fetchedEb' and serves it. The node then adds
-- 'announcingFetched' and 'certifyingFetched'. Whether 'otherEbHash' had the
-- focus until then, and the tip of the node's selection, once that is
-- 'certifyingFetched' or ten seconds have passed.
nodeSelectsCertifiedRival :: CertifyingArrival -> IOSim s (Bool, Point LeiosTestBlock)
nodeSelectsCertifiedRival arrival = withNode $ \nut -> do
  relayed <- newTVarIO []
  notifications <- newTQueueIO
  atomically $
    mapM_ (writeTQueue notifications) $
      MsgLeiosBlockAnnouncement (ltbHeader announcingOther)
        : announcingAndOffering fetchedEb announcingFetched
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut relayed)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
  -- Let the slots of the chain's blocks pass, so none is from the future.
  threadDelay 20
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar relayed >>= check . (>= 2) . length
    rivalFocused <- isFocusedEb otherEbHash <$> readOutstanding nut
    case arrival of
      CertifyingAfterAnnouncing -> addBlocks nut [announcingFetched, certifyingFetched]
      CertifyingBeforeAnnouncing -> do
        addBlocksAfter nut (NotOrigin announcingFetched) [certifyingFetched]
        addBlocks nut [announcingFetched]
    (,) rivalFocused <$> awaitTip nut (blockPoint certifyingFetched)

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

-- | An endorser block of one transaction of this many bytes, and that
-- transaction.
oneTxEb :: BytesSize -> (EndorserBlock, V.Vector LeiosTx)
oneTxEb size = (mkEndorserBlock [(hashLeiosTx tx, size)], V.singleton tx)
 where
  tx = MkLeiosTx (BS.replicate (fromIntegral size) 7)

-- | Where 'oneTxEbPeer' announces the endorser block of 'oneTxEb'.
oneTxEbPoint :: BytesSize -> LeiosPoint
oneTxEbPoint size = MkLeiosPoint 1 (endorserBlockHash (fst (oneTxEb size)))

-- | A peer that announces the endorser block, in the slot of 'oneTxEbPoint',
-- offers it and its closure, and serves both.
oneTxEbPeer ::
  NodeUnderTest (IOSim s) -> (EndorserBlock, V.Vector LeiosTx) -> IOSim s (EnvPeer (IOSim s))
oneTxEbPeer nut (eb, txs) = do
  notifications <- newTQueueIO
  atomically $
    mapM_ (writeTQueue notifications) $
      announcingAndOffering eb $
        setAnnouncement (endorserBlockHash eb) (encodeEndorserBlockSize eb) (firstLeiosBlock 0)
  pure
    EnvPeer
      { epChainSyncServer = quietChainSyncServer
      , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
      , epLeiosNotifyClient = Nothing
      , epLeiosFetchServer = serving eb txs nut
      , epLeiosFetchClient = Nothing
      }

-- | The node's acquisitions, as it announces them, from 'oneTxEbPeer'.
nodeFetchesOneTxEb :: (EndorserBlock, V.Vector LeiosTx) -> IOSim s [Acquired]
nodeFetchesOneTxEb ebAndTxs = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  envPeer <- oneTxEbPeer nut ebAndTxs
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn ->
    replicateM 2 $ acquiredOf <$> atomically next

-- | Why the node's LeiosFetch client for 'oneTxEbPeer' failed, if it did.
nodeFetchClientFails :: (EndorserBlock, V.Vector LeiosTx) -> IOSim s (Maybe String)
nodeFetchClientFails ebAndTxs = withNode $ \nut -> do
  envPeer <- oneTxEbPeer nut ebAndTxs
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    client <-
      maybe (throwIO $ userError "the node does not run a LeiosFetch client") pure $
        Map.lookup leiosFetchMiniProtocolNum (connNodeInitiators conn)
    atomically client <&> \case
      Left e -> Just (show e)
      Right _ -> Nothing

-- | An endorser block larger than the ledger allows, referencing only one byte
-- per transaction, and those transactions.
oversizedEb :: (EndorserBlock, V.Vector LeiosTx)
oversizedEb = (mkEndorserBlock [(hashLeiosTx tx, 1) | tx <- V.toList txs], txs)
 where
  txs = V.fromList [MkLeiosTx (BS.singleton w) | w <- [0 .. 199]]

oversizedPoint :: LeiosPoint
oversizedPoint = MkLeiosPoint 1 (endorserBlockHash (fst oversizedEb))

-- | Why the node's LeiosFetch client failed for a peer that announces
-- 'oversizedEb' and offers it at the most the ledger allows, and what the node
-- announces acquiring, if anything, by a second later.
nodeRefusesOversizedEb :: IOSim s (Maybe String, Maybe Acquired)
nodeRefusesOversizedEb = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  notifications <- newTQueueIO
  atomically $
    mapM_
      (writeTQueue notifications)
      [ MsgLeiosBlockAnnouncement (ltbHeader announcing)
      , MsgLeiosBlockOffer oversizedPoint maxEbBodySize
      , MsgLeiosBlockTxsOffer oversizedPoint
      ]
  let envPeer =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = serving eb txs nut
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    client <-
      maybe (throwIO $ userError "the node does not run a LeiosFetch client") pure $
        Map.lookup leiosFetchMiniProtocolNum (connNodeInitiators conn)
    failure <- atomically client <&> either (Just . show) (const Nothing)
    -- The client settles a write on another thread.
    threadDelay 1
    acquired <- atomically $ (Just . acquiredOf <$> next) `orElse` pure Nothing
    pure (failure, acquired)
 where
  (eb, txs) = oversizedEb
  announcing = setAnnouncement (endorserBlockHash eb) maxEbBodySize (firstLeiosBlock 0)

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
-- certified, and the tip of the node's selection, once that is the last block
-- of 'forgottenChain' or ten seconds have passed.
nodeReselectsForgotten :: IOSim s ((Int, Bool), (Bool, Int, Point LeiosTestBlock))
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
    heardByRival <- newTVarIO []
    rivalNotifications <- newTQueueIO
    holderNotifications <- newTQueueIO
    atomically $ do
      writeTQueue rivalNotifications (MsgLeiosBlockAnnouncement (ltbHeader announcingOther))
      mapM_ (writeTQueue holderNotifications) (announcingAndOffering fetchedEb announcingFetched)
    let rival =
          EnvPeer
            { epChainSyncServer = quietChainSyncServer
            , epLeiosNotifyServer = feedingLeiosNotifyServer nut rivalNotifications
            , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut heardByRival)
            , epLeiosFetchServer = silentLeiosFetchServer nut
            , epLeiosFetchClient = Nothing
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
      atomically $ readTVar heardByRival >>= check . not . null
      withNodeVsEnvConnection nut (PeerAddr 2) holder $ \_holderConn -> do
        addBlocks nut forgottenChain
        (,,) forgotten certifiedAtStart <$> awaitTip nut (lastPoint forgottenChain)
  pure (beforeRestart, afterRestart)
 where
  certifiedCount nut = Map.size <$> atomically (ChainDB.getLeiosCertifiedEbs (nutChainDB nut))
  holds nut blk = atomically $ ($ blockPoint blk) <$> ChainDB.getIsFetched (nutChainDB nut)

-- | The holder announces 'fetchedEb', and the node relays the announcement,
-- before the listener connects. Only then does the holder offer 'fetchedEb' and
-- serve it. The first three notifications the node relays to the listener.
nodeAnnouncesToLateListener :: IOSim s [Relayed]
nodeAnnouncesToLateListener = withNode $ \nut -> do
  heardByHolder <- newTVarIO []
  heardByListener <- newTVarIO []
  notifications <- newTQueueIO
  atomically $
    writeTQueue notifications (MsgLeiosBlockAnnouncement (ltbHeader announcingFetched))
  let holder =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = feedingLeiosNotifyServer nut notifications
          , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut heardByHolder)
          , epLeiosFetchServer = servingFetched nut
          , epLeiosFetchClient = Nothing
          }
      listener =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut heardByListener)
          , epLeiosFetchServer = silentLeiosFetchServer nut
          , epLeiosFetchClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) holder $ \_holderConn -> do
    -- The node relays an announcement only once it has taken it in.
    atomically $ readTVar heardByHolder >>= check . not . null
    withNodeVsEnvConnection nut (PeerAddr 2) listener $ \_listenerConn -> do
      atomically $
        mapM_
          (writeTQueue notifications)
          [ MsgLeiosBlockOffer fetchedPoint (encodeEndorserBlockSize fetchedEb)
          , MsgLeiosBlockTxsOffer fetchedPoint
          ]
      atomically $ readTVar heardByListener >>= \rs -> take 3 rs <$ check (length rs >= 3)

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

-- | A transaction that 'honestEb' and 'lyingEb' both name.
sharedTx :: LeiosTx
sharedTx = MkLeiosTx (BS.replicate 50 4)

-- | Names 'sharedTx' at its size.
honestEb :: EndorserBlock
honestEb = mkEndorserBlock [(hashLeiosTx sharedTx, 50)]

-- | Names 'sharedTx' at one byte.
lyingEb :: EndorserBlock
lyingEb = mkEndorserBlock [(hashLeiosTx sharedTx, 1)]

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
-- Whether the node offered the listener 'lyingEb''s body, and its closure.
nodeOffersLyingEb :: LieArrival -> IOSim s (Bool, Bool)
nodeOffersLyingEb arrival = withNode $ \nut -> do
  next <- subscribeLeiosAcquisitions (nutLeiosAcquisitions nut)
  heard <- newTVarIO []
  holderNotifications <- newTQueueIO
  liarNotifications <- newTQueueIO
  let lyingPoint = MkLeiosPoint (blockSlot announcingLying) (endorserBlockHash lyingEb)
      listener =
        EnvPeer
          { epChainSyncServer = quietChainSyncServer
          , epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Just (recordingLeiosNotifyClient nut heard)
          , epLeiosFetchServer = silentLeiosFetchServer nut
          , epLeiosFetchClient = Nothing
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
        threadDelay 5
        relays <- readTVarIO heard
        pure
          ( RelayedBlockOffer lyingPoint (encodeEndorserBlockSize lyingEb) `elem` relays
          , RelayedBlockTxsOffer lyingPoint `elem` relays
          )

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
readOutstanding :: NodeUnderTest (IOSim s) -> IOSim s (LeiosOutstanding (ConnectionId PeerAddr))
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
  AcquiredEb point _size -> AcquiredBody point
  AcquiredEbTxs point -> AcquiredClosure point

-- | A LeiosNotify client that records what the node relays.
--
-- It holds each announcement and offer to what a receiving node checks, with
-- the node's own immutable tip standing in for the receiver's, and records one
-- that fails as 'RelayedDishonestly'. It does not check an announcement's
-- validity, only that it counts towards its election as an honest one would.
--
-- TODO It does not fetch what the node offers, so a test using it does not
-- establish that a peer could.
recordingLeiosNotifyClient ::
  NodeUnderTest (IOSim s) ->
  StrictTVar (IOSim s) [Relayed] ->
  Channel (IOSim s) ByteString ->
  IOSim s ()
recordingLeiosNotifyClient nut relayed chan = do
  stVar <- newTVarIO emptyLeiosNotifyPeerState
  fst
    <$> runPipelinedPeerWithLimits
      nullTracer
      (envLeiosNotifyCodec nut)
      (byteLimitsLeiosNotify (fromIntegral . BSL.length))
      timeLimitsLeiosNotify
      chan
      ( toLeiosNotifyClientPeerPipelined $
          leiosNotifyClientPeerPipelined (pure (Right 1)) (pure (record stVar))
      )
 where
  cfg = nutTopLevelConfig nut

  record stVar msg = atomically $ do
    immLedger <- ChainDB.getImmutableLedger (nutChainDB nut)
    st <-
      pruneLeiosNotifyPeerState (withOrigin 0 id (getTipSlot (ledgerState immLedger)))
        <$> readTVar stVar
    let checked = case msg of
          MsgLeiosBlockAnnouncement hdr -> case mkAnnouncingHeader cfg hdr of
            Nothing -> Left $ "not an announcement: " <> show hdr
            Just anc ->
              either (Left . show) (\anns -> Right st{lnpsAnnouncements = anns}) $
                Announcements.insert @_ @Void (ancElectionId anc) anc (lnpsAnnouncements st)
          MsgLeiosBlockOffer point size ->
            first show $ runExcept $ checkLeiosBlockOffer cfg immLedger point size st
          MsgLeiosBlockTxsOffer point ->
            first show $ runExcept $ checkLeiosClosureOffer point st
          _ -> Right st
    case checked of
      Left why -> modifyTVar relayed (<> [RelayedDishonestly why])
      Right st' -> writeTVar stVar st' >> modifyTVar relayed (<> [relayedOf msg])

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
  case [peer | (connId, peer) <- Map.toList peers, remoteAddress connId == addr] of
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

-- | The node's reply to the client's one request, if it replied, and how its
-- server ended.
nodeAnswers ::
  (StrictTVar (IOSim s) (Maybe Reply) -> EnvClient s) -> IOSim s (Maybe Reply, ServerOutcome)
nodeAnswers client = do
  replyVar <- newTVarIO Nothing
  outcome <- nodeServerAgainst (client replyVar)
  reply <- readTVarIO replyVar
  pure (reply, outcome)

-- | A client that requests an endorser block, keeps the reply, and sends
-- 'MsgDone'.
requestBlock :: LeiosPoint -> StrictTVar (IOSim s) (Maybe Reply) -> EnvClient s
requestBlock point replyVar =
  Yield ReflClientAgency (MsgLeiosBlockRequest point) $
    Await ReflServerAgency $ \case
      MsgLeiosBlock eb -> Effect $ doneClient <$ atomically (writeTVar replyVar (Just (ReplyBlock eb)))

-- | A client that requests transactions, keeps the reply, and sends 'MsgDone'.
requestTxs :: LeiosPoint -> TxBitmaps -> StrictTVar (IOSim s) (Maybe Reply) -> EnvClient s
requestTxs point bitmaps replyVar =
  Yield ReflClientAgency (MsgLeiosBlockTxsRequest point bitmaps) $
    Await ReflServerAgency $ \case
      MsgLeiosBlockTxs txs ->
        Effect $ doneClient <$ atomically (writeTVar replyVar (Just (ReplyTxs (V.toList txs))))

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

-- | Write 'heldEb' and all its transactions but the last.
holdHeldEb :: NodeUnderTest (IOSim s) -> IOSim s ()
holdHeldEb nut = withWriter (getLeiosDB (nutKernel nut)) $ \writer -> do
  await =<< writeEbPoint writer heldPoint (encodeEndorserBlockSize heldEb)
  _ <- await =<< writeEbBody writer heldPoint heldEb []
  _ <- await =<< writeTxs writer heldPoint [(i, heldTxBytes i) | i <- [0 .. 68]]
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
