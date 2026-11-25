{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Tests of the node's pipelined LeiosFetch client, against a scripted server,
-- in simulated time.
module Test.Consensus.Leios.LeiosFetch.Client (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync (withAsync)
import Control.Monad.Class.MonadThrow (finally)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (mkTracer)
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec (AnyMessage (..), PeerRole (..))
import Network.TypedProtocol.Core (IsPipelined (..), ReflRelativeAgency (..))
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
  ( LeiosFetchJobActions (..)
  , SomeLeiosFetchJob (..)
  , leiosFetchClientPeerPipelined
  , toLeiosFetchClientPeerPipelined
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( LeiosFetch (..)
  , Message (..)
  , codecLeiosFetchId
  )
import Ouroboros.Network.Channel (createConnectedBufferedChannels)
import Ouroboros.Network.Driver.Simple
  ( Role (..)
  , TraceSendRecv (..)
  , runConnectedPeersPipelined
  )
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "pipelined client"
    [ testCase "the client sends a request as soon as it has one, even while a reply is outstanding" $
        sentRequests (runSimOrThrow (runClient 10 (\i -> if i == 1 then 10 else 0) sendSecondLater))
          @?= [(Time 0, 1), (Time 1, 2)]
    , testCase "the client keeps at most its depth of requests outstanding" $
        sentRequests (runSimOrThrow (runClient 2 (const 10) sendThree))
          @?= [(Time 0, 1), (Time 0, 2), (Time 10, 3)]
    , testCase
        "the client, once told to stop, ignores the replies still outstanding, then sends MsgDone"
        $ let run = runSimOrThrow (runClient 10 (const 10) stopWhileTwoOutstanding)
           in (sentRequests run, handled run, cleaned run, sentDone run)
                @?= ([(Time 0, 1), (Time 0, 2)], [], [1, 2], [Time 20])
    , testCase "stop wins over an available job in the same transaction" $
        let run = runSimOrThrow (runClient 10 (const 0) stopWithWork)
         in (sentRequests run, handled run, cleaned run) @?= ([], [], [])
    ]
 where
  stopWithWork queue stop = atomically $ do
    writeTVar queue [1]
    writeTVar stop True
  sendSecondLater queue stop = do
    atomically $ writeTVar queue [1]
    threadDelay 1
    atomically $ modifyTVar queue (<> [2])
    threadDelay 29
    atomically $ writeTVar stop True
  sendThree queue stop = do
    atomically $ writeTVar queue [1, 2, 3]
    threadDelay 100
    atomically $ writeTVar stop True
  stopWhileTwoOutstanding queue stop = do
    atomically $ writeTVar queue [1, 2]
    threadDelay 5
    atomically $ writeTVar stop True

-- | What the protocol carries in these tests: the number of a request, which
-- its reply echoes.
newtype N = N Int
  deriving (Eq, Show)

instance ShowProxy N where showProxy _ = "N"

-- | What the client did over a run.
data Run = MkRun
  { sentRequests :: [(Time, Int)]
  -- ^ when it sent each block request
  , sentDone :: [Time]
  -- ^ when it sent 'MsgDone'
  , cleaned :: [Int]
  -- ^ requests whose handler cleanup ran, including ignored drain replies
  , handled :: [Int]
  -- ^ the replies it handed to their jobs
  }

-- | Run the client, leaving at most this many requests outstanding, against a
-- server that delays its reply to each block request by the given time, while
-- the script feeds the client's request queue and finally tells it to stop.
runClient ::
  Int ->
  (Int -> DiffTime) ->
  (StrictTVar (IOSim s) [Int] -> StrictTVar (IOSim s) Bool -> IOSim s ()) ->
  IOSim s Run
runClient depth delayFor script = do
  queue <- newTVarIO []
  stop <- newTVarIO False
  handledVar <- newTVarIO []
  cleanedVar <- newTVarIO []
  events <- newTVarIO []
  let tracer = mkTracer $ \(role, event) -> do
        now <- getMonotonicTime
        atomically $ modifyTVar events ((now, role, event) :)
      job i =
        MkSomeLeiosFetchJob
          (LeiosFetchJobActions (pure True) (pure ()) (\action -> action `finally` atomically (modifyTVar cleanedVar (i :))))
          (MsgLeiosBlockRequest (N i))
          (\(MsgLeiosBlock (N j)) -> atomically $ modifyTVar handledVar (j :))
      awaitStop = readTVar stop >>= check
      awaitJob =
        readTVar queue >>= \case
          [] -> retry
          i : is -> job i <$ writeTVar queue is
  _ <-
    withAsync (script queue stop) $ \_ ->
      runConnectedPeersPipelined
        (createConnectedBufferedChannels 100)
        tracer
        codecLeiosFetchId
        (toLeiosFetchClientPeerPipelined (leiosFetchClientPeerPipelined depth awaitStop awaitJob))
        (server delayFor)
  traced <- reverse <$> readTVarIO events
  replies <- reverse <$> readTVarIO handledVar
  cleanups <- reverse <$> readTVarIO cleanedVar
  pure
    MkRun
      { sentRequests =
          [(t, i) | (t, Client, TraceSendMsg (AnyMessage (MsgLeiosBlockRequest (N i)))) <- traced]
      , sentDone = [t | (t, Client, TraceSendMsg (AnyMessage MsgDone)) <- traced]
      , handled = replies
      , cleaned = cleanups
      }

-- | Replies to each block request after the given delay, one at a time.
server :: (Int -> DiffTime) -> Peer (LeiosFetch N N N) AsServer NonPipelined StIdle (IOSim s) ()
server delayFor = Await ReflClientAgency $ \case
  MsgLeiosBlockRequest (N i) -> Effect $ do
    threadDelay (delayFor i)
    pure $ Yield ReflServerAgency (MsgLeiosBlock (N i)) (server delayFor)
  MsgLeiosBlockTxsRequest _point _bitmaps ->
    Yield ReflServerAgency (MsgLeiosBlockTxs V.empty) (server delayFor)
  MsgDone -> Done ReflNobodyAgency ()
