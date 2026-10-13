{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests of LeiosNotify's client and server. Some run the two against each
-- other over connected channels with a real codec. The others run the node's
-- client and server, over real connections, against peers run by the Leios
-- node-vs-environment tests.
module Test.Consensus.Leios.LeiosNotify (tests) where

import Cardano.Binary.FixedSizeCodec (encodeFixedSized)
import Cardano.Crypto.DSIGN.Class (genKeyDSIGN, signDSIGN)
import Cardano.Crypto.Leios (LeiosSeatId (..), leiosSignContext)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.BaseTypes (Milliseconds32 (..), knownNonZeroBounded)
import Cardano.Ledger.State (emptyLeiosCommittee)
import Cardano.Network.NodeToNode (defaultMiniProtocolParameters)
import Cardano.Network.NodeToNode.Version (NodeToNodeVersion (..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft)
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word16, Word8)
import Network.TypedProtocol.Codec
  ( AnyMessage (..)
  , Codec
  , CodecF (..)
  , SomeMessage (..)
  , runDecoder
  )
import Ouroboros.Consensus.Block (ConvertRawHash (..), SlotNo (..), blockHash, blockSlot)
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Config (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash (..)
  , LeiosPoint (..)
  , LeiosVote (..)
  , RbHash (..)
  , decodeLeiosPoint
  , decodeLeiosVote
  , encodeEbHash
  , encodeLeiosPoint
  , encodeLeiosVote
  , encodeRbHash
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Validate (maxAnnouncementAgeRecv)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Server
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import Ouroboros.Consensus.Network.LeiosNotify
  ( leiosNotifyMiniProtocolNum
  , leiosNotifyPipelineDepth
  , runLookaheadFixedSenderPeerWithLimits
  )
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Node.ExitPolicy (NodeToNodeInitiatorResult)
import Ouroboros.Consensus.NodeKernel (getLeiosNotifyClientStates)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment (noPunishment)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import Ouroboros.Network.Channel (Channel, createConnectedBufferedChannels)
import Ouroboros.Network.Driver.Limits
  ( ProtocolLimitFailure (..)
  , runPipelinedPeerWithLimits
  )
import Ouroboros.Network.Driver.Simple (DecoderFailure)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , TemperatureBundle (..)
  , WithProtocolTemperature (..)
  )
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits)
import Ouroboros.Network.Util.ShowProxy (ShowProxy)
import Test.Consensus.Leios.Environment
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Util.ChainDB (emptyNodeDBs)
import Test.Util.Serialisation.CDDL (cddlTest, isCDDLCDisabled)

tests :: TestTree
tests =
  testGroup
    "LeiosNotify"
    [ testGroup "client and server" $
        [ testCase "the server cancels every outstanding request when the client quits" $
            runSimOrThrow (outstandingCanceled 3) @?= OutstandingOutcome 3 0
        , testCase "the server disconnects a client that exceeds its pipelining bound" $
            runSimOrThrow excessive @?= ExcessiveOutcome Disconnected
        , testCase "the codec rejects an empty MsgLeiosVotes" $
            assertBool "decoded" $
              isLeft $
                runSimOrThrow decodeEmptyVotes
        ]
          <> [ testProperty "the node's codec encodes each message as the CDDL specifies" $
                 prop_nodeCodecMatchesCDDL
             | not isCDDLCDisabled
             ]
    , testGroup
        "node vs environment"
        [ testCase "the node's client keeps its pipeline full, and quits when the node demotes the peer" $
            runSimOrThrow (timeout 100 nodeClientKeepsPipelineFull)
              @?= Just (NodeClientOutcome leiosNotifyPipelineDepth)
        , testCase "the node's client keeps requesting as replies arrive" $
            runSimOrThrow (timeout 100 nodeClientRefillsPipeline)
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 6))
        , testCase "the node's client disconnects a peer that cancels a request unprompted" $
            runSimOrThrow (timeout 100 nodeClientUnpromptedCancel)
              @?= Just (NodeClientDisconnected UnexpectedCancel)
        , testCase "the node's client sends no request until its immutable tip can forecast to now" $
            runSimOrThrow (timeout 100 nodeClientGated) @?= Just 0
        , testCase "the node's client accepts a valid announcement" $
            runSimOrThrow (timeout 100 (nodeClientAgainstAnnouncements [ann 1 (firstLeiosBlock 0)]))
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 1))
        , testCase "the node's client accepts two announcements for one election" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstAnnouncements
                    [ann 1 (firstLeiosBlock 0), ann 2 (forkLeiosBlock (firstLeiosBlock 0))]
              )
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 2))
        , testCase "the node's client accepts an announcement whose counter is below its immutable tip's" $
            runSimOrThrow
              (timeout 100 (nodeClientAgainstAnnouncements [setCounterBelowImmTips (ann 1 (firstLeiosBlock 0))]))
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 1))
        , testCase
            "the node's client counts an announcement whose counter is below its immutable tip's toward its election"
            $ runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstAnnouncements
                    [ setCounterBelowImmTips (ann 1 (firstLeiosBlock 0))
                    , ann 2 (forkLeiosBlock (firstLeiosBlock 0))
                    , ann 3 (forkLeiosBlock (forkLeiosBlock (firstLeiosBlock 0)))
                    ]
              )
              @?= Just (NodeClientDisconnected AnnouncementThird)
        , testCase "the node's client disconnects a peer that repeats an announcement" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstAnnouncements [ann 1 (firstLeiosBlock 0), ann 1 (firstLeiosBlock 0)]
              )
              @?= Just (NodeClientDisconnected AnnouncementRepeated)
        , testCase "the node's client disconnects a peer that announces a third time in one election" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstAnnouncements
                    [ ann 1 (firstLeiosBlock 0)
                    , ann 2 (forkLeiosBlock (firstLeiosBlock 0))
                    , ann 3 (forkLeiosBlock (forkLeiosBlock (firstLeiosBlock 0)))
                    ]
              )
              @?= Just (NodeClientDisconnected AnnouncementThird)
        , testCase "the node's client disconnects a peer that relays a header without an announcement" $
            runSimOrThrow (timeout 100 (nodeClientAgainstAnnouncements [firstLeiosBlock 0]))
              @?= Just (NodeClientDisconnected AnnouncementMissing)
        , testCase "the node's client disconnects a peer that relays an invalid announcing header" $
            runSimOrThrow
              (timeout 100 (nodeClientAgainstAnnouncements [setInvalid (ann 1 (firstLeiosBlock 0))]))
              @?= Just (NodeClientDisconnected AnnouncementInvalid)
        , testCase "the node's client disconnects a peer that relays a far-future announcement" $
            runSimOrThrow
              (timeout 100 (nodeClientAgainstAnnouncements [ann 1 (setSlot 1000 (firstLeiosBlock 0))]))
              @?= Just (NodeClientDisconnected AnnouncementInFuture)
        , testCase "the node's client disconnects a peer that relays a too-old announcement" $
            runSimOrThrow (timeout 1000 nodeClientTooOld)
              @?= Just (NodeClientDisconnected AnnouncementTooOld)
        , testCase "the node prunes its clients' state as its immutable tip advances" $
            runSimOrThrow (timeout 1000 nodePrunesClientState) @?= Just ((1, 1, 1, 1), (0, 0, 0, 0))
        , testCase "the node's client accepts an offer of an endorser block its peer announced" $
            runSimOrThrow (timeout 100 (nodeClientAgainstOffers [MsgLeiosBlockOffer someOfferedPoint 1000]))
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 2))
        , testCase "the node's client disconnects a peer that offers an endorser block it never announced" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstOffers
                    [MsgLeiosBlockOffer (MkLeiosPoint (SlotNo 1) (MkEbHash (BS.replicate 32 2))) 1000]
              )
              @?= Just (NodeClientDisconnected OfferWithoutAnnouncement)
        , testCase "the node's client disconnects a peer that repeats a body offer" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstOffers
                    [MsgLeiosBlockOffer someOfferedPoint 1000, MsgLeiosBlockOffer someOfferedPoint 1000]
              )
              @?= Just (NodeClientDisconnected OfferRepeated)
        , testCase "the node's client disconnects a peer that offers an empty endorser block" $
            runSimOrThrow (timeout 100 (nodeClientAgainstOffers [MsgLeiosBlockOffer someOfferedPoint 0]))
              @?= Just (NodeClientDisconnected OfferBadSize)
        , testCase
            "the node's client disconnects a peer that offers an endorser block bigger than the ledger allows"
            $ runSimOrThrow
              (timeout 100 (nodeClientAgainstOffers [MsgLeiosBlockOffer someOfferedPoint (maxEbBodySize + 1)]))
              @?= Just (NodeClientDisconnected OfferBadSize)
        , testCase
            "the node's client disconnects a peer that offers an endorser block older than its immutable tip"
            $ runSimOrThrow (timeout 1000 (nodeClientOfferTooOld (MsgLeiosBlockOffer someOfferedPoint 1000)))
              @?= Just (NodeClientDisconnected OfferTooOld)
        , testCase "the node's client accepts a closure offer of an endorser block its peer announced" $
            runSimOrThrow (timeout 100 (nodeClientAgainstOffers [MsgLeiosBlockTxsOffer someOfferedPoint]))
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 2))
        , testCase "the node's client accepts a body offer and a closure offer of one endorser block" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstOffers
                    [MsgLeiosBlockTxsOffer someOfferedPoint, MsgLeiosBlockOffer someOfferedPoint 1000]
              )
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 3))
        , testCase
            "the node's client disconnects a peer that closure-offers an endorser block it never announced"
            $ runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstOffers
                    [MsgLeiosBlockTxsOffer (MkLeiosPoint (SlotNo 1) (MkEbHash (BS.replicate 32 2)))]
              )
              @?= Just (NodeClientDisconnected ClosureOfferWithoutAnnouncement)
        , testCase "the node's client disconnects a peer that repeats a closure offer" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstOffers
                    [MsgLeiosBlockTxsOffer someOfferedPoint, MsgLeiosBlockTxsOffer someOfferedPoint]
              )
              @?= Just (NodeClientDisconnected ClosureOfferRepeated)
        , testCase
            "the node's client disconnects a peer that closure-offers an endorser block older than its immutable tip"
            $ runSimOrThrow (timeout 1000 (nodeClientOfferTooOld (MsgLeiosBlockTxsOffer someOfferedPoint)))
              @?= Just (NodeClientDisconnected OfferTooOld)
        , testCase "the node's client accepts a vote within its window" $
            runSimOrThrow
              ( timeout
                  100
                  (nodeClientAgainstSchedule [(5, MsgLeiosVotes (NE.fromList [voteFor someAnnouncingBlock 0]))])
              )
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 2))
        , testCase "the node's client accepts votes from different seats in one slot" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstSchedule
                    [(5, MsgLeiosVotes (NE.fromList [voteFor someAnnouncingBlock 0, voteFor someAnnouncingBlock 1]))]
              )
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 2))
        , testCase "the node's client disconnects a peer that votes for an announcement it never sent" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstSchedule
                    [(5, MsgLeiosVotes (NE.fromList [voteFor (ann 2 (successorLeiosBlock (firstLeiosBlock 0))) 0]))]
              )
              @?= Just (NodeClientDisconnected VoteWithoutAnnouncement)
        , testCase "the node's client disconnects a peer whose vote names the wrong slot" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstSchedule
                    [
                      ( 5
                      , MsgLeiosVotes
                          (NE.fromList [(voteFor someAnnouncingBlock 0){voteSlot = blockSlot someAnnouncingBlock + 1}])
                      )
                    ]
              )
              @?= Just (NodeClientDisconnected VoteWithoutAnnouncement)
        , testCase "the node's client disconnects a peer that relays two votes from one seat in one slot" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstSchedule
                    [ (0, MsgLeiosBlockAnnouncement (ltbHeader equivocation))
                    , (5, MsgLeiosVotes (NE.fromList [voteFor someAnnouncingBlock 0]))
                    , (5, MsgLeiosVotes (NE.fromList [voteFor equivocation 0]))
                    ]
              )
              @?= Just (NodeClientDisconnected VoteRepeated)
        , testCase "the node's client disconnects a peer that relays a vote too early" $
            runSimOrThrow
              ( timeout
                  100
                  (nodeClientAgainstSchedule [(1.5, MsgLeiosVotes (NE.fromList [voteFor someAnnouncingBlock 0]))])
              )
              @?= Just (NodeClientDisconnected VoteTooEarly)
        , testCase "the node's client disconnects a peer that relays a vote too late" $
            runSimOrThrow
              ( timeout
                  200
                  (nodeClientAgainstSchedule [(100, MsgLeiosVotes (NE.fromList [voteFor someAnnouncingBlock 0]))])
              )
              @?= Just (NodeClientDisconnected VoteTooLate)
        , testCase "the node's client disconnects a peer whose votes message holds more than the most votes" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstSchedule
                    [(5, MsgLeiosVotes (NE.fromList [voteFor someAnnouncingBlock seat | seat <- [0 .. 16]]))]
              )
              @?= Just (NodeClientDisconnected Undecodable)
        , testCase "the node's client accepts a votes message holding the most votes" $
            runSimOrThrow
              ( timeout 100 $
                  nodeClientAgainstSchedule
                    [(5, MsgLeiosVotes (NE.fromList [voteFor someAnnouncingBlock seat | seat <- [0 .. 15]]))]
              )
              @?= Just (NodeClientOutcome (leiosNotifyPipelineDepth + 2))
        , testCase "the node's server cancels a peer's outstanding requests when the peer quits" $
            runSimOrThrow (timeout 100 nodeServerCancels) @?= Just (NodeServerOutcome 0)
        , testCase "the node's server disconnects a peer that exceeds the pipelining bound" $
            runSimOrThrow (timeout 100 nodeServerExcessive) @?= Just (ExcessiveOutcome Disconnected)
        , testCase "the node runs LeiosNotify from NodeToNodeV_16 on" $
            runSimOrThrow (timeout 100 (nodeRunsLeiosNotify NodeToNodeV_16)) @?= Just (True, True)
        , testCase "the node does not run LeiosNotify before NodeToNodeV_16" $
            runSimOrThrow (timeout 100 (nodeRunsLeiosNotify NodeToNodeV_15)) @?= Just (False, False)
        ]
    , testGroup
        "the codec accepts either CBOR list encoding"
        [ testCase "definite message, indefinite votes" $
            runSimOrThrow (decodeVotes (definite [votesKey, indefinite twoVotes])) @?= Just [1, 2]
        , testCase "indefinite message, definite votes" $
            runSimOrThrow (decodeVotes (indefinite [votesKey, definite twoVotes])) @?= Just [1, 2]
        , testCase "indefinite message, indefinite votes" $
            runSimOrThrow (decodeVotes (indefinite [votesKey, indefinite twoVotes])) @?= Just [1, 2]
        , testCase "rejects indefinite votes past the bound" $
            runSimOrThrow
              (decodeVotes (definite [votesKey, indefinite (map CBOR.encodeInt [1 .. 17])]))
              @?= Nothing
        , testCase "rejects an indefinite message with an extra field" $
            runSimOrThrow (decodeVotes (indefinite [votesKey, definite twoVotes, CBOR.encodeInt 0]))
              @?= Nothing
        , testCase "rejects empty indefinite votes" $
            runSimOrThrow (decodeVotes (definite [votesKey, indefinite []])) @?= Nothing
        , testCase "an indefinite point" $
            let p = MkLeiosPoint (SlotNo 3) (MkEbHash (BS.replicate 32 7))
             in decodeWith
                  decodeLeiosPoint
                  (indefinite [CBOR.encodeWord64 3, encodeEbHash (MkEbHash (BS.replicate 32 7))])
                  @?= Just p
        , testCase "an indefinite vote" $
            let v = voteFor someAnnouncingBlock 0
             in decodeWith decodeLeiosVote (indefiniteVote v) @?= Just v
        ]
    ]

{-------------------------------------------------------------------------------
  Client and server on their own
-------------------------------------------------------------------------------}

type P = LeiosNotify Int Int Int

codec :: Codec P CBOR.DeserialiseFailure (IOSim s) BSL.ByteString
codec =
  codecLeiosNotify
    CBOR.encodeInt
    CBOR.decodeInt
    CBOR.encodeInt
    CBOR.decodeInt
    CBOR.encodeInt
    CBOR.decodeInt

-- | Decode a 'MsgLeiosVotes' that holds no votes.
decodeEmptyVotes :: IOSim s (Either CBOR.DeserialiseFailure (SomeMessage (StBusy :: P)))
decodeEmptyVotes = do
  step <- decode codec SingBusy
  runDecoder
    [CBOR.toLazyByteString (CBOR.encodeListLen 2 <> CBOR.encodeWord 7 <> CBOR.encodeListLen 0)]
    step

-- | The node's codec, except that an announcement is any CBOR bytes rather
-- than a header, as the CDDL leaves it unspecified.
nodeCodec ::
  Codec (LeiosNotify LeiosPoint BS.ByteString LeiosVote) CBOR.DeserialiseFailure IO BSL.ByteString
nodeCodec =
  codecLeiosNotify
    encodeLeiosPoint
    decodeLeiosPoint
    CBOR.encodeBytes
    CBOR.decodeBytes
    encodeLeiosVote
    decodeLeiosVote

prop_nodeCodecMatchesCDDL :: Property
prop_nodeCodecMatchesCDDL =
  forAll genMessage $ \(AnyMessage msg) ->
    ioProperty $ do
      result <-
        cddlTest
          (pure $ BSL.toStrict $ encode nodeCodec msg)
          "ouroboros-consensus-cardano/cddl/node-to-node/leiosnotify/leiosNotify.cddl"
          (T.pack "leiosNotifyMessage")
      pure $ either (`counterexample` False) (const $ property True) result
 where
  genMessage =
    oneof
      [ pure $ AnyMessage MsgLeiosNotificationRequestNext
      , AnyMessage . MsgLeiosBlockAnnouncement . BS.pack <$> arbitrary
      , fmap AnyMessage $ MsgLeiosBlockOffer <$> genPoint <*> arbitrary
      , AnyMessage . MsgLeiosBlockTxsOffer <$> genPoint
      , do
          n <- choose (1, maxLeiosVotesPerMessage)
          AnyMessage . MsgLeiosVotes . NE.fromList <$> vectorOf n genVote
      , pure $ AnyMessage MsgDone
      , pure $ AnyMessage MsgQuit
      , pure $ AnyMessage MsgCanceled
      ]

  genPoint = MkLeiosPoint <$> (SlotNo <$> arbitrary) <*> (MkEbHash <$> genHash)

  genVote = signedVote <$> (SlotNo <$> arbitrary) <*> (MkRbHash <$> genHash) <*> arbitrary

  genHash = BS.pack <$> vectorOf 32 arbitrary

-- | Decode a 'MsgLeiosVotes'.
decodeVotes :: CBOR.Encoding -> IOSim s (Maybe [Int])
decodeVotes enc = do
  step <- decode codec SingBusy
  runDecoder [CBOR.toLazyByteString enc] step >>= \case
    Right (SomeMessage (MsgLeiosVotes vs)) -> pure (Just (NE.toList vs))
    _ -> pure Nothing

decodeWith :: (forall s. CBOR.Decoder s a) -> CBOR.Encoding -> Maybe a
decodeWith dec enc =
  either (const Nothing) (Just . snd) $
    CBOR.deserialiseFromBytes dec (CBOR.toLazyByteString enc)

indefiniteVote :: LeiosVote -> CBOR.Encoding
indefiniteVote (MkLeiosVote slot rbHash seat signature) =
  indefinite
    [ CBOR.encodeWord64 (unSlotNo slot)
    , encodeRbHash rbHash
    , CBOR.encodeWord16 (leiosSeatIndex seat)
    , encodeFixedSized signature
    ]

definite :: [CBOR.Encoding] -> CBOR.Encoding
definite xs = CBOR.encodeListLen (fromIntegral $ length xs) <> mconcat xs

indefinite :: [CBOR.Encoding] -> CBOR.Encoding
indefinite xs = CBOR.encodeListLenIndef <> mconcat xs <> CBOR.encodeBreak

votesKey :: CBOR.Encoding
votesKey = CBOR.encodeWord 7

twoVotes :: [CBOR.Encoding]
twoVotes = map CBOR.encodeInt [1, 2]

sizeLimits :: ProtocolSizeLimits (LeiosNotify point announcement vote) BSL.ByteString
sizeLimits = byteLimitsLeiosNotify (fromIntegral . BSL.length)

-- | Run a server that never replies, counting requests and refusing more than
-- @bound@.
runServer ::
  (ShowProxy point, ShowProxy announcement, ShowProxy vote) =>
  Codec (LeiosNotify point announcement vote) CBOR.DeserialiseFailure (IOSim s) BSL.ByteString ->
  StrictTVar (IOSim s) Int ->
  Int ->
  Channel (IOSim s) BSL.ByteString ->
  IOSim s ()
runServer c requests bound chan = do
  peer <-
    leiosNotifyServerPeerLookahead
      ( atomically $ do
          n <- readTVar requests
          if n >= bound
            then pure ExcessiveRequests
            else do
              writeTVar requests (n + 1)
              pure NotExcessiveRequests
      )
      retry
  fst
    <$> runLookaheadFixedSenderPeerWithLimits nullTracer c sizeLimits timeLimitsLeiosNotify chan peer

-- | Run the pipelined client at the given depth until @stop@ is filled,
-- counting the replies it handles.
runPipelinedClient ::
  (ShowProxy point, ShowProxy announcement, ShowProxy vote) =>
  Codec (LeiosNotify point announcement vote) CBOR.DeserialiseFailure (IOSim s) BSL.ByteString ->
  StrictTVar (IOSim s) Int ->
  Int ->
  StrictTMVar (IOSim s) () ->
  Channel (IOSim s) BSL.ByteString ->
  IOSim s ()
runPipelinedClient c replies depth stop chan =
  fst
    <$> runPipelinedPeerWithLimits
      nullTracer
      c
      sizeLimits
      timeLimitsLeiosNotify
      chan
      ( toLeiosNotifyClientPeerPipelined $
          leiosNotifyClientPeerPipelined
            ((Left <$> readTMVar stop) `orElse` pure (Right depth))
            (pure $ \_msg -> atomically $ modifyTVar replies (+ 1))
      )

data OutstandingOutcome = OutstandingOutcome Int Int
  deriving (Eq, Show)

-- | Fill the pipeline, then quit: the server sees every request, and the
-- client handles none of the cancellations, since it is already quitting.
outstandingCanceled :: Int -> IOSim s OutstandingOutcome
outstandingCanceled depth = do
  (clientChan, serverChan) <- createConnectedBufferedChannels 10
  requests <- newTVarIO 0
  replies <- newTVarIO 0
  stop <- newEmptyTMVarIO
  withAsync (runServer codec requests depth serverChan) $ \server ->
    withAsync (runPipelinedClient codec replies depth stop clientChan) $ \client -> do
      atomically $ readTVar requests >>= check . (== depth)
      atomically $ putTMVar stop ()
      wait client
      wait server
      OutstandingOutcome <$> readTVarIO requests <*> readTVarIO replies

data WhetherDisconnected = Disconnected | NotDisconnected
  deriving (Eq, Show)

newtype ExcessiveOutcome = ExcessiveOutcome WhetherDisconnected
  deriving (Eq, Show)

-- | A client pipelining one request more than the server allows.
excessive :: IOSim s ExcessiveOutcome
excessive = do
  (clientChan, serverChan) <- createConnectedBufferedChannels 10
  requests <- newTVarIO 0
  replies <- newTVarIO 0
  stop <- newEmptyTMVarIO
  withAsync (runPipelinedClient codec replies 3 stop clientChan) $ \_client ->
    disconnectedBy $ runServer codec requests 2 serverChan

disconnectedBy :: IOSim s () -> IOSim s ExcessiveOutcome
disconnectedBy server =
  try server >>= \case
    Left ExnLeiosNotifyExcessiveRequests -> pure $ ExcessiveOutcome Disconnected
    Right () -> pure $ ExcessiveOutcome NotDisconnected

{-------------------------------------------------------------------------------
  The node, connected to peers run by the test
-------------------------------------------------------------------------------}

withNode :: (NodeUnderTest (IOSim s) -> IOSim s a) -> IOSim s a
withNode = withNodeForecasting 100000

-- | A node whose ledger view forecasts this many slots past its immutable tip.
withNodeForecasting :: SlotNo -> (NodeUnderTest (IOSim s) -> IOSim s a) -> IOSim s a
withNodeForecasting range body = do
  nodeDBs <- emptyNodeDBs
  withNodeUnderTest cfg nodeDBs body
 where
  k = SecurityParam $ knownNonZeroBounded @10
  cfg =
    defaultNodeUnderTestConfig
      k
      LeiosTestLedgerConfig
        { ltlcHardForkParams =
            HardFork.defaultEraParams k (slotLengthFromSec 1) HardFork.NoPerasEnabled
        , ltlcForecastRange = range
        , ltlcLeios =
            MkLeiosLedgerView
              { llvCommittee = emptyLeiosCommittee
              , llvQuorumStakeThreshold = maxBound
              , llvAnnouncementPeriod = Milliseconds32 1000
              , llvVotePeriod = Milliseconds32 4000
              , llvDiffusionPeriod = Milliseconds32 1000
              , llvMaxEbBodySize = maxEbBodySize
              , llvMaxEbTxsSize = maxEbBodySize
              }
        }

-- | The node's LeiosNotify client on the connection.
nodeLeiosNotifyClient ::
  NodeVsEnvConnection (IOSim s) ->
  IOSim s (STM (IOSim s) (Either SomeException NodeToNodeInitiatorResult))
nodeLeiosNotifyClient conn =
  maybe (throwIO $ userError "the node does not run a LeiosNotify client") pure $
    Map.lookup leiosNotifyMiniProtocolNum (connNodeInitiators conn)

-- | The node's LeiosNotify server on the connection.
nodeLeiosNotifyServer ::
  NodeVsEnvConnection (IOSim s) -> IOSim s (STM (IOSim s) (Either SomeException ()))
nodeLeiosNotifyServer conn =
  maybe (throwIO $ userError "the node does not run a LeiosNotify server") pure $
    Map.lookup leiosNotifyMiniProtocolNum (connNodeResponders conn)

data NodeClientOutcome
  = -- | how many requests the peer received
    NodeClientOutcome Int
  | NodeClientDisconnected DisconnectReason
  deriving (Eq, Show)

data DisconnectReason
  = UnexpectedCancel
  | ExceededSize
  | ExceededTime
  | Undecodable
  | AnnouncementMissing
  | AnnouncementRepeated
  | AnnouncementThird
  | AnnouncementInvalid
  | AnnouncementTooOld
  | AnnouncementInFuture
  | OfferWithoutAnnouncement
  | OfferBadSize
  | OfferRepeated
  | ClosureOfferWithoutAnnouncement
  | ClosureOfferRepeated
  | OfferTooOld
  | VoteWithoutAnnouncement
  | VoteRepeated
  | VoteTooEarly
  | VoteTooLate
  | VoteOutsideHorizon
  | OfferOutsideHorizon
  deriving (Eq, Show)

-- | Why the node's client disconnected its peer, if it did so on purpose.
disconnectReason :: SomeException -> Maybe DisconnectReason
disconnectReason e
  | Just ExnLeiosNotifyUnexpectedCancel <- fromException e = Just UnexpectedCancel
  | Just (_ :: DecoderFailure) <- fromException e = Just Undecodable
  | Just (lf :: ProtocolLimitFailure) <- fromException e = Just $ case lf of
      ExceededSizeLimit{} -> ExceededSize
      ExceededTimeLimit{} -> ExceededTime
  | Just (exn :: ExnLeiosNotifyInvalidAnnouncement Blk) <- fromException e = Just $ case exn of
      ExnLeiosNotifyAnnouncementMissing -> AnnouncementMissing
      ExnLeiosNotifyInvalidAnnouncement err -> case err of
        Announcements.ErrRepeat -> AnnouncementRepeated
        Announcements.ErrThird -> AnnouncementThird
        Announcements.ErrInvalid{} -> AnnouncementInvalid
        Announcements.ErrTooOld -> AnnouncementTooOld
  | Just (_ :: InFutureCheck.HeaderArrivalException) <- fromException e = Just AnnouncementInFuture
  | Just (exn :: ExnLeiosNotifyInvalidOffer) <- fromException e = Just $ case exn of
      ExnLeiosBlockOfferWithoutAnnouncement{} -> OfferWithoutAnnouncement
      ExnLeiosBlockOfferBadSize{} -> OfferBadSize
      ExnLeiosRepeatedBlockOffer{} -> OfferRepeated
      ExnLeiosClosureOfferWithoutAnnouncement{} -> ClosureOfferWithoutAnnouncement
      ExnLeiosRepeatedClosureOffer{} -> ClosureOfferRepeated
      ExnLeiosOfferTooOld{} -> OfferTooOld
      ExnLeiosBlockOfferOutsideHorizon{} -> OfferOutsideHorizon
  | Just (exn :: ExnLeiosNotifyInvalidVotes) <- fromException e = Just $ case exn of
      ExnLeiosVoteWithoutAnnouncement{} -> VoteWithoutAnnouncement
      ExnLeiosRepeatedVote{} -> VoteRepeated
      ExnLeiosVoteTooEarly{} -> VoteTooEarly
      ExnLeiosVoteTooLate{} -> VoteTooLate
      ExnLeiosVoteOutsideHorizon{} -> VoteOutsideHorizon
  | otherwise = Nothing

-- | Run a peer's server that answers each request with whatever @next@ yields
-- next, counting requests.
runReplyingServer ::
  LeiosNotifyCodec s ->
  StrictTVar (IOSim s) Int ->
  STM (IOSim s) (Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle) ->
  Channel (IOSim s) BSL.ByteString ->
  IOSim s ()
runReplyingServer c requests next chan = do
  peer <-
    leiosNotifyServerPeerLookahead
      (NotExcessiveRequests <$ atomically (modifyTVar requests (+ 1)))
      next
  fst
    <$> runLookaheadFixedSenderPeerWithLimits nullTracer c sizeLimits timeLimitsLeiosNotify chan peer

-- | The node's client, connected to a peer whose server answers with @next@;
-- the node demotes the peer once the peer has received @target@ requests.
runNodeClientAgainst ::
  NodeUnderTest (IOSim s) ->
  STM (IOSim s) (Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle) ->
  Int ->
  IOSim s NodeClientOutcome
runNodeClientAgainst nut next target = do
  requests <- newTVarIO 0
  let envPeer =
        EnvPeer
          { epLeiosNotifyServer = runReplyingServer (envLeiosNotifyCodec nut) requests next
          , epLeiosNotifyClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    client <- nodeLeiosNotifyClient conn
    early <-
      atomically $
        (Just <$> client)
          `orElse` (Nothing <$ (readTVar requests >>= check . (>= target)))
    result <- case early of
      Just result -> pure result
      Nothing -> do
        -- told to stop, the client drains its pipeline and ends
        connTerminate conn
        atomically client
    case result of
      Right _ -> NodeClientOutcome <$> readTVarIO requests
      Left e -> maybe (throwIO e) (pure . NodeClientDisconnected) (disconnectReason e)

-- | A peer that never has a notification: the node keeps exactly its
-- pipelining bound of requests outstanding.
nodeClientKeepsPipelineFull :: IOSim s NodeClientOutcome
nodeClientKeepsPipelineFull = withNode $ \nut ->
  runNodeClientAgainst nut retry leiosNotifyPipelineDepth

-- | Six replies, announcements from six elections, free six slots in the
-- pipeline, which the node fills again.
nodeClientRefillsPipeline :: IOSim s NodeClientOutcome
nodeClientRefillsPipeline = withNode $ \nut -> do
  queue <- newTQueueIO
  atomically $
    mapM_ (writeTQueue queue) $
      [ MsgLeiosBlockAnnouncement $ ltbHeader $ setIssuer issuer $ ann 1 $ firstLeiosBlock 0
      | issuer <- [0 .. 5]
      ]
  runNodeClientAgainst nut (readTQueue queue) (leiosNotifyPipelineDepth + 6)

nodeClientUnpromptedCancel :: IOSim s NodeClientOutcome
nodeClientUnpromptedCancel = withNode $ \nut ->
  runNodeClientAgainst nut (pure MsgCanceled) maxBound

-- | Announce an endorser block whose hash repeats the given byte 32 times.
ann :: Word8 -> LeiosTestBlock -> LeiosTestBlock
ann n = setAnnouncement (MkEbHash (BS.replicate 32 n)) 1000

-- | A peer that relays these headers, one per request.
nodeClientAgainstAnnouncements :: [LeiosTestBlock] -> IOSim s NodeClientOutcome
nodeClientAgainstAnnouncements blks = withNode $ \nut -> do
  queue <- newTQueueIO
  atomically $ mapM_ (writeTQueue queue . MsgLeiosBlockAnnouncement . ltbHeader) blks
  runNodeClientAgainst nut (readTQueue queue) (leiosNotifyPipelineDepth + length blks)

-- | The announcement arrives long after its slot: the peer connects only then.
nodeClientTooOld :: IOSim s NodeClientOutcome
nodeClientTooOld = withNode $ \nut -> do
  threadDelay (realToFrac maxAnnouncementAgeRecv + 100)
  queue <- newTQueueIO
  atomically $ writeTQueue queue $ MsgLeiosBlockAnnouncement $ ltbHeader $ ann 1 (firstLeiosBlock 0)
  runNodeClientAgainst nut (readTQueue queue) (leiosNotifyPipelineDepth + 1)

-- | By the time the node connects, the current slot is past what its
-- immutable tip, still at genesis, can forecast to.
nodeClientGated :: IOSim s Int
nodeClientGated = withNodeForecasting 5 $ \nut -> do
  threadDelay 10
  requests <- newTVarIO 0
  let envPeer =
        EnvPeer
          { epLeiosNotifyServer = runReplyingServer (envLeiosNotifyCodec nut) requests retry
          , epLeiosNotifyClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    threadDelay 10
    n <- readTVarIO requests
    connTerminate conn
    client <- nodeLeiosNotifyClient conn
    atomically client >>= either throwIO (\_ -> pure n)

-- | How many elections, body offers, closure offers and votes the client's state
-- holds once one of each is in, and once the immutable tip has passed them.
nodePrunesClientState :: IOSim s ((Int, Int, Int, Int), (Int, Int, Int, Int))
nodePrunesClientState = withNode $ \nut -> do
  requests <- newTVarIO 0
  queue <- newTQueueIO
  let envPeer =
        EnvPeer
          { epLeiosNotifyServer = runReplyingServer (envLeiosNotifyCodec nut) requests (readTQueue queue)
          , epLeiosNotifyClient = Nothing
          }
  withAsync
    ( feedAt
        queue
        [ (0, someAnnouncement)
        , (0, MsgLeiosBlockOffer someOfferedPoint 1000)
        , (0, MsgLeiosBlockTxsOffer someOfferedPoint)
        , -- within the vote's window
          (5, MsgLeiosVotes (NE.fromList [voteFor someAnnouncingBlock 0]))
        ]
    )
    $ \_feeder ->
      withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \_conn -> do
        atomically $ readTVar requests >>= check . (> leiosNotifyPipelineDepth + 3)
        beforePruning <- clientStateSize nut
        advanceImmutableTip nut
        afterPruning <- clientStateSize nut
        pure (beforePruning, afterPruning)

-- | How many elections, body offers, closure offers and votes the node's
-- clients remember.
clientStateSize :: NodeUnderTest (IOSim s) -> IOSim s (Int, Int, Int, Int)
clientStateSize nut = atomically $ do
  states <- IOLike.readTVar (getLeiosNotifyClientStates (nutKernel nut))
  sizes <- mapM (fmap size . IOLike.readTVar) (Map.elems states)
  pure
    ( sum [e | (e, _, _, _) <- sizes]
    , sum [b | (_, b, _, _) <- sizes]
    , sum [c | (_, _, c, _) <- sizes]
    , sum [v | (_, _, _, v) <- sizes]
    )
 where
  size st =
    ( Map.size (Announcements.live (lnpsAnnouncements st))
    , Set.size (lnpsBodyOffers st)
    , Set.size (lnpsClosureOffers st)
    , Set.size (lnpsVotes st)
    )

-- | Put each reply in the queue at its time.
feedAt ::
  StrictTQueue (IOSim s) (Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle) ->
  [(DiffTime, Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle)] ->
  IOSim s ()
feedAt queue = go 0
 where
  go _now [] = pure ()
  go now ((at, msg) : rest) = do
    threadDelay (max 0 (at - now))
    atomically $ writeTQueue queue msg
    go (max now at) rest

-- | A peer that sends each reply at its time, the announcement of
-- 'someAnnouncingBlock' first.
nodeClientAgainstSchedule ::
  [(DiffTime, Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle)] ->
  IOSim s NodeClientOutcome
nodeClientAgainstSchedule schedule = withNode $ \nut -> do
  queue <- newTQueueIO
  let msgs = (0, someAnnouncement) : schedule
  withAsync (feedAt queue msgs) $ \_feeder ->
    runNodeClientAgainst nut (readTQueue queue) (leiosNotifyPipelineDepth + length msgs)

-- | A vote from this seat for the announcement this block carries.
voteFor :: LeiosTestBlock -> Word16 -> LeiosVote
voteFor blk seat = signedVote (blockSlot blk) (MkRbHash (toRawHash (Proxy @LeiosTestBlock) (blockHash blk))) seat

-- | A vote from this seat for this announcing block. The node's LeiosNotify
-- client does not check a vote's signature, so any key will do.
signedVote :: SlotNo -> RbHash -> Word16 -> LeiosVote
signedVote slot rbHash seat =
  MkLeiosVote slot rbHash (LeiosSeatId seat) $
    signDSIGN leiosSignContext rbHash (genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 0)))

someAnnouncingBlock :: LeiosTestBlock
someAnnouncingBlock = ann 1 (firstLeiosBlock 0)

-- | Another announcement in the election of 'someAnnouncingBlock'.
equivocation :: LeiosTestBlock
equivocation = ann 2 (forkLeiosBlock (firstLeiosBlock 0))

-- | Make the first two slots immutable: k is 10, so twelve blocks do it.
advanceImmutableTip :: NodeUnderTest (IOSim s) -> IOSim s ()
advanceImmutableTip nut = do
  threadDelay 20
  mapM_ (ChainDB.addBlock_ (nutChainDB nut) noPunishment) $
    take 12 $
      iterate successorLeiosBlock (firstLeiosBlock 1)
  threadDelay 1

-- | The peer sends this offer of an endorser block it announced, but only once
-- the immutable tip has passed it.
nodeClientOfferTooOld ::
  Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle -> IOSim s NodeClientOutcome
nodeClientOfferTooOld offer = withNode $ \nut -> do
  queue <- newTQueueIO
  atomically $ writeTQueue queue someAnnouncement
  announced <- newEmptyTMVarIO
  let feed = do
        atomically $ readTMVar announced
        advanceImmutableTip nut
        atomically $ writeTQueue queue offer
      next = do
        msg <- readTQueue queue
        _ <- tryPutTMVar announced ()
        pure msg
  withAsync feed $ \_feeder -> runNodeClientAgainst nut next maxBound

-- | A peer that announces 'someOfferedPoint', then sends these.
nodeClientAgainstOffers ::
  [Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle] -> IOSim s NodeClientOutcome
nodeClientAgainstOffers msgs = withNode $ \nut -> do
  queue <- newTQueueIO
  atomically $ mapM_ (writeTQueue queue) (someAnnouncement : msgs)
  runNodeClientAgainst nut (readTQueue queue) (leiosNotifyPipelineDepth + 1 + length msgs)

someAnnouncement :: Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle
someAnnouncement = MsgLeiosBlockAnnouncement $ ltbHeader $ ann 1 (firstLeiosBlock 0)

-- | The endorser block 'someAnnouncement' announces.
someOfferedPoint :: LeiosPoint
someOfferedPoint = MkLeiosPoint (SlotNo 1) (MkEbHash (BS.replicate 32 1))

maxEbBodySize :: BytesSize
maxEbBodySize = 5000

newtype NodeServerOutcome = NodeServerOutcome Int
  deriving (Eq, Show)

-- | The node never has a notification, so the peer's requests are all
-- outstanding when it quits.
nodeServerCancels :: IOSim s NodeServerOutcome
nodeServerCancels = withNode $ \nut -> do
  requests <- newTVarIO 0
  replies <- newTVarIO 0
  stop <- newEmptyTMVarIO
  let codec' = envLeiosNotifyCodec nut
      envPeer =
        EnvPeer
          { epLeiosNotifyServer = runReplyingServer codec' requests retry
          , epLeiosNotifyClient = Just $ runPipelinedClient codec' replies 3 stop
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    threadDelay 10
    atomically $ putTMVar stop ()
    for_ (connPeerLeiosNotifyClient conn) $ \client -> atomically client >>= either throwIO pure
    server <- nodeLeiosNotifyServer conn
    atomically server >>= either throwIO pure
    NodeServerOutcome <$> readTVarIO replies

-- | A peer pipelining one request more than the node's server allows, so the
-- node disconnects it.
nodeServerExcessive :: IOSim s ExcessiveOutcome
nodeServerExcessive = withNode $ \nut -> do
  requests <- newTVarIO 0
  replies <- newTVarIO 0
  stop <- newEmptyTMVarIO
  let codec' = envLeiosNotifyCodec nut
      envPeer =
        EnvPeer
          { epLeiosNotifyServer = runReplyingServer codec' requests retry
          , epLeiosNotifyClient =
              Just $ runPipelinedClient codec' replies (leiosNotifyPipelineDepth + 1) stop
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    server <- nodeLeiosNotifyServer conn
    atomically server >>= \case
      Left e | Just ExnLeiosNotifyExcessiveRequests <- fromException e -> pure $ ExcessiveOutcome Disconnected
      _ -> pure $ ExcessiveOutcome NotDisconnected

-- | Whether the node's initiator bundle, and its initiator-and-responder bundle,
-- include LeiosNotify among their hot mini-protocols for this version.
nodeRunsLeiosNotify :: NodeToNodeVersion -> IOSim s (Bool, Bool)
nodeRunsLeiosNotify ntnVersion = withNode $ \nut ->
  pure
    ( runsLeiosNotify $
        NTN.initiator mempty defaultMiniProtocolParameters ntnVersion versionData (nutApps nut ())
    , runsLeiosNotify $
        NTN.initiatorAndResponder
          mempty
          defaultMiniProtocolParameters
          ntnVersion
          versionData
          (nutApps nut ())
    )
 where
  versionData = nodeVersionData ntnVersion
  runsLeiosNotify bundle = case withHot bundle of
    WithHot protocols -> leiosNotifyMiniProtocolNum `elem` map miniProtocolNum protocols
