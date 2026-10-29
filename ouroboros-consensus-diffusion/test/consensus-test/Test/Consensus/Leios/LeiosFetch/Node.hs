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
import Control.Monad (replicateM, void)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.Bits ((.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec (PeerRole (..))
import Network.TypedProtocol.Core (IsPipelined (..), ReflRelativeAgency (..))
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.Block
  ( Header
  , Proxy (..)
  , blockHash
  , blockSlot
  , pattern GenesisPoint
  )
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
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel (ExnLeiosTwoCertificationClaims (..))
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
import Ouroboros.Consensus.Network.LeiosNotify (runLookaheadFixedSenderPeerWithLimits)
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , Promise (..)
  , withWriter
  )
import Ouroboros.Network.Block (Tip (..))
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.Driver.Limits (runPipelinedPeerWithLimits)
import Ouroboros.Network.Driver.Simple (runAnnotatedPeer)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , TemperatureBundle (..)
  , WithProtocolTemperature (..)
  )
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
    , testCase
        "the node disconnects a peer that rolls forward certificates of two endorser blocks for one election"
        $ runSimFinishing nodeDisconnectsDoubleCertifier
          >>= (@?= Just (endorserBlockHash fetchedEb, otherEbHash))
    , testCase "the node runs LeiosFetch from NodeToNodeV_16 on" $
        runSimFinishing (nodeRunsLeiosFetch NodeToNodeV_16) >>= (@?= (True, True))
    , testCase "the node does not run LeiosFetch before NodeToNodeV_16" $
        runSimFinishing (nodeRunsLeiosFetch NodeToNodeV_15) >>= (@?= (False, False))
    ]

-- | Run the simulation, and fail the test if it has not finished after 100
-- simulated seconds.
runSimFinishing :: (forall s. IOSim s a) -> IO a
runSimFinishing sim = case runSimOrThrow (timeout 100 sim) of
  Nothing -> assertFailure "the simulation did not finish within 100 seconds"
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

acquiredOf :: LeiosEbNotification -> Acquired
acquiredOf = \case
  AcquiredEb point _size -> AcquiredBody point
  AcquiredEbTxs point -> AcquiredClosure point

-- | A LeiosNotify client that records what the node relays.
recordingLeiosNotifyClient ::
  NodeUnderTest (IOSim s) ->
  StrictTVar (IOSim s) [Relayed] ->
  Channel (IOSim s) ByteString ->
  IOSim s ()
recordingLeiosNotifyClient nut relayed chan =
  fst
    <$> runPipelinedPeerWithLimits
      nullTracer
      (envLeiosNotifyCodec nut)
      (byteLimitsLeiosNotify (fromIntegral . BSL.length))
      timeLimitsLeiosNotify
      chan
      ( toLeiosNotifyClientPeerPipelined $
          leiosNotifyClientPeerPipelined
            (pure (Right 1))
            (pure (\msg -> atomically $ modifyTVar relayed (<> [relayedOf msg])))
      )

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
servingFetched nut chan =
  fst
    <$> runAnnotatedPeer nullTracer (envLeiosFetchCodec nut) chan (leiosFetchServerPeer serveFetched)

relayedOf ::
  Message (LeiosNotify LeiosPoint (Header LeiosTestBlock) LeiosVote) LN.StBusy LN.StIdle -> Relayed
relayedOf = \case
  MsgLeiosBlockAnnouncement _hdr -> RelayedAnnouncement
  MsgLeiosBlockOffer point size -> RelayedBlockOffer point size
  MsgLeiosBlockTxsOffer point -> RelayedBlockTxsOffer point
  MsgLeiosVotes _votes -> RelayedVotes
  MsgCanceled -> RelayedVotes

-- | Serve 'fetchedEb' and its transactions.
serveFetched :: Monad m => LeiosFetchRequestHandler LeiosPoint EndorserBlock LeiosTx m
serveFetched = MkLeiosFetchRequestHandler $ \case
  MsgLeiosBlockRequest _point -> pure (MsgLeiosBlock fetchedEb)
  MsgLeiosBlockTxsRequest _point bitmaps ->
    pure $ MsgLeiosBlockTxs $ V.fromList $ mapMaybe (fetchedTxs V.!?) (bitmapOffsets bitmaps)

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
