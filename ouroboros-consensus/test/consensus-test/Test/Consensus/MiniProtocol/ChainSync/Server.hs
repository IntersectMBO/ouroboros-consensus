{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The node-to-client ChainSync server never sends a CertRB without its EB's
-- transactions.
module Test.Consensus.MiniProtocol.ChainSync.Server (tests) where

import Cardano.Binary (DecoderError)
import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (foldM, forM_, unless, (>=>))
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.ResourceRegistry (ResourceRegistry, withRegistry)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import LeiosDemoTypes
  ( BytesSize
  , LeiosClosureError (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash
  )
import Network.TypedProtocol.Proofs (connect)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (SecurityParam (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( CertRbWithoutEbTxs (..)
  , CertRbWithoutEbTxsReason (..)
  , chainSyncBlockServerFollower
  , chainSyncBlocksServer
  , inlineCertifiedEbTxs
  )
import Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (GetRawBlock), ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDBImpl
import Ouroboros.Consensus.Storage.LedgerDB (ResolveLeiosBlock (..))
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , newLeiosDBInMemory
  , withLeiosDBSQLite
  , withReader
  )
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , StrictTVar
  , atomically
  , modifyTVar
  , readTVar
  , uncheckedNewTVarM
  , writeTVar
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (Serialised (..), Tip, getTipPoint)
import qualified Ouroboros.Network.Block as Network
import Ouroboros.Network.Protocol.ChainSync.Client
import Ouroboros.Network.Protocol.ChainSync.Server
  ( ChainSyncServer (..)
  , ServerStIdle (..)
  , ServerStIntersect (..)
  , ServerStNext (..)
  , chainSyncServerPeer
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.LeiosTestNode
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.ChainDB
import Test.Util.LeiosTestBlock
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "ChainSync server (CertRBs)"
    [ testGroup
        "one block"
        [ testCase "a block that is not a CertRB is sent as stored" test_notCertRb
        , testCase "a CertRB is sent with its EB's transactions" test_certRb
        , testCase "throws if the block before announced no EB" test_noAnnouncement
        , testCase "throws if the stored CertRB does not decode" test_undecodable
        , testCase "throws if the LeiosDB lacks the EB's transactions" test_closureMissing
        ]
    , testCase
        "a client syncing from genesis after the LeiosDB is garbage-collected gets every CertRB with its EB's transactions"
        test_syncAfterGc
    , testCase
        "a client that intersects at an RB that announced an EB gets the next CertRB with the EB's transactions"
        test_intersectAtAnnouncer
    , testProperty
        "for any chain and any follower updates, the client follows the node's chain"
        prop_followsTheNodesChain
    ]

type Blk = LeiosTestBlock

{-------------------------------------------------------------------------------
  The chain

  rb1 and rb2 announce eb1 and eb2, which cert1 and cert2 certify. p1
  announces eb3, which is never certified.
-------------------------------------------------------------------------------}

rb1, cert1, rb2, cert2, p1, p2, p3 :: Blk
rb1 = announcing eb1Point eb1Size (firstLeiosBlock 0)
cert1 = certifying (certFor rb1) (successorLeiosBlock rb1)
rb2 = announcing eb2Point eb2Size (successorLeiosBlock cert1)
cert2 = certifying (certFor rb2) (successorLeiosBlock rb2)
p1 = announcing eb3Point eb3Size (successorLeiosBlock cert2)
p2 = successorLeiosBlock p1
p3 = successorLeiosBlock p2

chain :: [Blk]
chain = [rb1, cert1, rb2, cert2, p1, p2, p3]

eb1Txs, eb2Txs, eb3Txs :: [LeiosTestTx]
eb1Txs = [tx 1, tx 2]
eb2Txs = [tx 3]
eb3Txs = [tx 4]

eb1, eb2, eb3 :: LeiosEb
eb1Closure, eb2Closure, eb3Closure :: [(TxHash, BS.ByteString)]
eb1Size, eb2Size, eb3Size :: BytesSize
(eb1, eb1Closure, eb1Size) = mkLeiosTestEb eb1Txs
(eb2, eb2Closure, eb2Size) = mkLeiosTestEb eb2Txs
(eb3, eb3Closure, eb3Size) = mkLeiosTestEb eb3Txs

-- | Each EB is announced in the slot after the block before its announcer.
eb1Point, eb2Point, eb3Point :: LeiosPoint
eb1Point = leiosTestEbPoint (blockSlot (firstLeiosBlock 0)) eb1
eb2Point = leiosTestEbPoint (blockSlot (successorLeiosBlock cert1)) eb2
eb3Point = leiosTestEbPoint (blockSlot (successorLeiosBlock cert2)) eb3

tx :: Int -> LeiosTestTx
tx k = LeiosTestTx{ltxAsserts = IntMap.empty, ltxWrites = IntMap.singleton k 'a'}

{-------------------------------------------------------------------------------
  One block
-------------------------------------------------------------------------------}

test_notCertRb :: Assertion
test_notCertRb = do
  db <- newLeiosDBInMemory
  sent <- withReader db $ \reader -> send reader Nothing rb1 (encodeBlock rb1)
  sent @?= encodeBlock rb1

test_certRb :: Assertion
test_certRb = do
  db <- newLeiosDBInMemory
  storeEb db eb1Point eb1 eb1Size (map snd eb1Closure)
  sent <- withReader db $ \reader -> send reader (Just eb1Point) cert1 (encodeBlock cert1)
  blockTxs (decodeBlock sent) @?= eb1Txs

test_noAnnouncement :: Assertion
test_noAnnouncement =
  throwsBecause (\case NoAnnouncement -> True; _ -> False) $ do
    db <- newLeiosDBInMemory
    storeEb db eb1Point eb1 eb1Size (map snd eb1Closure)
    withReader db $ \reader -> send reader Nothing cert1 (encodeBlock cert1)

test_undecodable :: Assertion
test_undecodable =
  throwsBecause (\case Undecodable _ -> True; _ -> False) $ do
    db <- newLeiosDBInMemory
    storeEb db eb1Point eb1 eb1Size (map snd eb1Closure)
    withReader db $ \reader -> send reader (Just eb1Point) cert1 (Serialised (Lazy.pack [0xff]))

test_closureMissing :: Assertion
test_closureMissing =
  throwsBecause (\case EbTxsUnavailable LeiosClosureMissing{} -> True; _ -> False) $ do
    db <- newLeiosDBInMemory
    withReader db $ \reader -> send reader (Just eb1Point) cert1 (encodeBlock cert1)

-- | What the server sends for this block, given the EB the block before it
-- announced.
send :: LeiosDbReader IO -> Maybe LeiosPoint -> Blk -> Serialised Blk -> IO (Serialised Blk)
send reader prevAnn blk =
  inlineCertifiedEbTxs LeiosTestCodecConfig reader prevAnn (blockPoint blk) (getHeader blk)

-- | The action throws 'CertRbWithoutEbTxs' for 'cert1', for this reason.
throwsBecause :: (CertRbWithoutEbTxsReason -> Bool) -> IO a -> Assertion
throwsBecause expected act =
  try act >>= \case
    Left (CertRbWithoutEbTxs pt reason) -> do
      pt @?= blockPoint cert1
      assertBool ("unexpected reason: " <> show reason) (expected reason)
    Right _ -> assertFailure "expected CertRbWithoutEbTxs, but the block was sent"

{-------------------------------------------------------------------------------
  Syncing from genesis after garbage collection
-------------------------------------------------------------------------------}

-- | After garbage collection, a client syncing from genesis still gets every
-- CertRB with its EB's transactions. Uses SQLite, the only LeiosDB that
-- collects. The volatile part ends up empty, so the transactions come from the
-- immutable part.
test_syncAfterGc :: Assertion
test_syncAfterGc =
  withSystemTempDirectory "chainsync-server" $ \dir ->
    withLeiosDBSQLite nullTracer (dir </> "vol.db") (dir </> "imm.db") $ \db -> do
      -- Stored before the ChainDB opens, so that it adopts the CertRBs.
      storeEb db eb1Point eb1 eb1Size (map snd eb1Closure)
      storeEb db eb2Point eb2 eb2Size (map snd eb2Closure)
      storeEb db eb3Point eb3 eb3Size (map snd eb3Closure)
      nodeDBs <- emptyNodeDBs
      withNode securityParam nodeDBs db $ \node@Node{nChainDB = chainDB, nInternal = internal} -> do
        addChain node
        tip <- atomically (ChainDB.getTipPoint chainDB)
        tip @?= blockPoint p3
        -- With k = 2, rb1 to p1 become immutable.
        immTip <- ChainDBImpl.intCopyToImmutableDB internal
        immTip @?= NotOrigin (blockSlot p1)
        awaitIO "the certified EBs to be promoted" $
          (>= 2) . immutableEbs <$> leiosDbSampleStats db
        ChainDBImpl.intGarbageCollect internal (succ (blockSlot p1))
        awaitIO "the volatile part to be garbage-collected" $
          (== 0) . volatileEbs <$> leiosDbSampleStats db
        withReader db $ \reader -> do
          received <- syncFromGenesis chainDB reader
          map blockPoint received @?= map blockPoint chain
          map blockTxs received @?= [[], eb1Txs, [], eb2Txs, [], [], []]

-- | Everything a client gets from 'chainSyncBlocksServer', from genesis to the
-- tip.
syncFromGenesis :: ChainDB IO Blk -> LeiosDbReader IO -> IO [Blk]
syncFromGenesis chainDB reader = withRegistry $ \registry -> do
  follower <- chainSyncBlockServerFollower chainDB registry
  let server = chainSyncBlocksServer nullTracer chainDB LeiosTestCodecConfig reader follower
  (received, (), _) <-
    connect (chainSyncClientPeer (client [])) (chainSyncServerPeer server)
  pure received
 where
  client :: [Blk] -> ChainSyncClient (Serialised Blk) (Point Blk) (Tip Blk) IO [Blk]
  client acc = ChainSyncClient $ pure $ SendMsgRequestNext (pure ()) next
   where
    next =
      ClientStNext
        { recvMsgRollForward = \sblk tip ->
            let blk = decodeBlock sblk
                acc' = acc <> [blk]
             in if blockPoint blk == getTipPoint tip
                  then ChainSyncClient (pure (SendMsgDone acc'))
                  else client acc'
        , recvMsgRollBackward = \_ _ -> client acc
        }

-- | Small enough that most of 'chain' becomes immutable, so that its CertRBs'
-- EBs are promoted and garbage collection can run.
securityParam :: SecurityParam
securityParam = SecurityParam $ knownNonZeroBounded @2

-- | Add 'chain' in order, as BlockFetch would.
addChain :: IOLike m => Node m -> m ()
addChain node =
  sequence_ $ addAtGenesis node rb1 : zipWith (addWithCommittee node) chain (drop 1 chain)

{-------------------------------------------------------------------------------
  Any chain, any follower updates
-------------------------------------------------------------------------------}

-- | What the node and one client do, in order.
data Scenario = Scenario
  { scEbs :: Map LeiosPoint [LeiosTestTx]
  -- ^ The transactions of every EB that a block announces. All are in the
  -- LeiosDB from the start, so the node adopts every CertRB.
  , scSteps :: [Step]
  }

data Step
  = -- | The node adds this block, as BlockFetch would. It may switch chains.
    AddBlock Blk
  | -- | The client asks for this many updates.
    RequestNext Int
  | -- | The client reconnects, offering its chain from this many blocks back
    -- from its tip.
    Reconnect Int

instance Show Scenario where
  show = unlines . map showStep . scSteps
   where
    -- Without the certificate, which is long and says nothing here.
    showStep = \case
      AddBlock blk ->
        unwords $
          ["AddBlock", show (blockPoint blk), show (length (blockTxs blk)), "txs"]
            <> ["certifies" | headerContainsLeiosCert (getHeader blk)]
            <> ["announces" | isJust (headerLeiosAnnouncement (getHeader blk))]
      RequestNext n -> "RequestNext " <> show n
      Reconnect n -> "Reconnect " <> show n

-- | A block tree with forks, added parents first, with client steps between
-- the blocks.
genScenario :: Gen Scenario
genScenario = do
  n <- choose (1, 20)
  (blocks, scEbs) <- genBlocks n
  scSteps <- concat <$> mapM (\blk -> (AddBlock blk :) <$> genClientSteps) blocks
  pure Scenario{scEbs, scSteps}
 where
  genClientSteps = do
    requests <- frequency [(2, pure []), (3, (: []) . RequestNext <$> choose (1, 3))]
    reconnects <- frequency [(5, pure []), (1, (: []) . Reconnect <$> choose (0, 4))]
    pure (requests <> reconnects)

-- | A tree of blocks with forks. A block after an EB announcement usually
-- certifies that EB. Transactions never conflict, so the node accepts every block.
genBlocks :: Int -> Gen ([Blk], Map LeiosPoint [LeiosTestTx])
genBlocks = go [] Map.empty Map.empty 0 0
 where
  -- added: the blocks so far, newest first
  -- ebs: the transactions of each announced EB
  -- children: how many children each block has, so siblings get different hashes
  -- key: the next unused transaction key
  -- roots: how many blocks start at genesis, so each gets a different hash
  -- n: how many blocks are left to make
  go added ebs _ _ _ 0 = pure (reverse added, ebs)
  go added ebs children key roots n = do
    parent <-
      frequency $
        (1, pure Nothing)
          : [(4, Just <$> elements (take 3 added)) | not (null added)]
            <> [(2, Just <$> elements added) | not (null added)]
    let base = case parent of
          Nothing -> firstLeiosBlock roots
          Just p ->
            iterate forkLeiosBlock (successorLeiosBlock p)
              !! Map.findWithDefault 0 (blockHash p) children
        children' = maybe id (\p -> Map.insertWith (+) (blockHash p) 1) parent children
        roots' = maybe (roots + 1) (const roots) parent
        parentAnnounces = any (\p -> hasAnnouncement (getHeader p)) parent
    certifies <- if parentAnnounces then frequency [(3, pure True), (1, pure False)] else pure False
    nOwnTxs <- if certifies then pure 0 else choose (0, 2)
    let (ownTxs, key') = freshTxs nOwnTxs key
        withBody = case parent of
          Just p | certifies -> certifying (certFor p) base
          _ -> withTxs ownTxs base
    announces <- arbitrary
    nEbTxs <- choose (1, 3)
    let (ebTxs, key'') = freshTxs nEbTxs key'
        (eb, _, size) = mkLeiosTestEb ebTxs
        point = leiosTestEbPoint (blockSlot base) eb
        (blk, ebs')
          | announces = (announcing point size withBody, Map.insert point ebTxs ebs)
          | otherwise = (withBody, ebs)
    go (blk : added) ebs' children' (if announces then key'' else key') roots' (n - 1)

  freshTxs m key = (map tx [key .. key + m - 1], key + m)

  hasAnnouncement = isJust . headerLeiosAnnouncement

-- | Run a random scenario and check that:
--
-- * each CertRB has exactly its EB's transactions,
-- * every other block is sent as stored,
-- * the server only sends what the ChainDB follower says, and
-- * the client ends with the node's chain.
prop_followsTheNodesChain :: Property
prop_followsTheNodesChain =
  forAll genScenario $ \scenario ->
    let Outcome{oFailures, oLabels, oClientChain, oNodeChain} = runScenario [] scenario
     in conjoin
          [ counterexample (unlines oFailures) (null oFailures)
          , counterexample "the client's chain is not the node's" $
              oClientChain === oNodeChain
          ]
          & tabulate "events" oLabels
          & cover 10 ("rollback" `elem` oLabels) "the client rolled back"
          & cover 10 ("CertRB" `elem` oLabels) "the client got a CertRB"
          & cover 5 ("reconnect" `elem` oLabels) "the client reconnected"

-- | A client that reconnects at 'rb1' still gets 'cert1' with eb1's
-- transactions, though this connection never sent 'rb1'.
test_intersectAtAnnouncer :: Assertion
test_intersectAtAnnouncer = do
  let ebs = Map.fromList [(eb1Point, eb1Txs), (eb2Point, eb2Txs), (eb3Point, eb3Txs)]
      Outcome{oFailures, oLabels, oClientChain, oNodeChain} =
        runScenario
          [blockPoint rb1]
          Scenario{scEbs = ebs, scSteps = map AddBlock chain <> [Reconnect 0]}
  assertBool (unlines oFailures) (null oFailures)
  assertBool
    ("expected cert1 right after intersecting at rb1, got: " <> show oLabels)
    ("CertRB right after intersecting at its announcer" `elem` oLabels)
  assertEqual "the client's chain is not the node's" oNodeChain oClientChain

data Outcome = Outcome
  { oFailures :: [String]
  , oLabels :: [String]
  , oClientChain :: [Point Blk]
  -- ^ Oldest first.
  , oNodeChain :: [Point Blk]
  -- ^ Oldest first.
  }

-- | Run the scenario for a client that starts with this chain (newest first),
-- then let the client catch up with the node.
runScenario :: [Point Blk] -> Scenario -> Outcome
runScenario clientChain Scenario{scEbs, scSteps} = runSimOrThrow $ do
  nodeDBs <- emptyNodeDBs
  db <- newLeiosDBInMemory
  forM_ (Map.toList scEbs) $ \(point, txs) ->
    let (eb, closure, size) = mkLeiosTestEb txs
     in storeEb db point eb size (map snd closure)
  withNode propSecurityParam nodeDBs db $ \eNode ->
    withReader db $ \eReader -> withRegistry $ \eRegistry -> do
      let env = Env{eNode, eReader, eRegistry, eBlocks, eEbs = scEbs}
      (conn, close, followed) <- openConn env
      let client0 =
            Client
              { cConn = conn
              , cClose = close
              , cFollowed = followed
              , cChain = clientChain
              , cIntersection = Nothing
              , cFailures = []
              , cLabels = []
              }
      client <- foldM (runStep env) client0 scSteps >>= catchUp env
      nodeChain <- selectedChain env
      pure
        Outcome
          { oFailures = reverse (cFailures client)
          , oLabels = reverse (cLabels client)
          , oClientChain = reverse (cChain client)
          , oNodeChain = nodeChain
          }
 where
  eBlocks = Map.fromList [(blockHash blk, blk) | AddBlock blk <- scSteps]

-- | k is larger than any chain here, so the node can switch to any fork.
propSecurityParam :: SecurityParam
propSecurityParam = SecurityParam $ knownNonZeroBounded @30

data Env s = Env
  { eNode :: Node (IOSim s)
  , eReader :: LeiosDbReader (IOSim s)
  , eRegistry :: ResourceRegistry (IOSim s)
  , eBlocks :: Map (HeaderHash Blk) Blk
  , eEbs :: Map LeiosPoint [LeiosTestTx]
  }

type Idle s = ServerStIdle (Serialised Blk) (Point Blk) (Tip Blk) (IOSim s) ()

type Next s = ServerStNext (Serialised Blk) (Point Blk) (Tip Blk) (IOSim s) ()

data Conn s
  = -- | Ready for a request.
    Idle (Idle s)
  | -- | The server is at the node's tip, and answers once the chain changes.
    Awaiting (IOSim s (Next s))

-- | An instruction the ChainDB follower gave the server.
data Followed = Forwarded (Point Blk) | RolledBack (Point Blk)
  deriving (Eq, Show)

data Client s = Client
  { cConn :: Conn s
  , cClose :: IOSim s ()
  , cFollowed :: StrictTVar (IOSim s) [Followed]
  -- ^ The follower's instructions the client has not had yet.
  , cChain :: [Point Blk]
  -- ^ Newest first.
  , cIntersection :: Maybe (Point Blk)
  -- ^ Where the client last intersected, until the next block arrives.
  , cFailures :: [String]
  , cLabels :: [String]
  }

-- | Connect to a new server, whose follower's instructions are recorded.
openConn :: Env s -> IOSim s (Conn s, IOSim s (), StrictTVar (IOSim s) [Followed])
openConn Env{eNode = Node{nChainDB}, eReader, eRegistry} = do
  -- Unchecked: test bookkeeping, read only by this thread.
  followed <- uncheckedNewTVarM []
  follower <- recording followed <$> chainSyncBlockServerFollower nChainDB eRegistry
  idle <-
    runChainSyncServer $
      chainSyncBlocksServer nullTracer nChainDB LeiosTestCodecConfig eReader follower
  pure (Idle idle, ChainDB.followerClose follower, followed)
 where
  recording var f =
    f
      { ChainDB.followerInstruction =
          ChainDB.followerInstruction f >>= \mu -> mapM_ (record var) mu >> pure mu
      , ChainDB.followerInstructionBlocking =
          ChainDB.followerInstructionBlocking f >>= \u -> record var u >> pure u
      }
  record var u = atomically $ modifyTVar var (<> [followedOf u])
  followedOf = \case
    Network.AddBlock wp -> Forwarded (ChainDB.point wp)
    Network.RollBack pt -> RolledBack pt

runStep :: Env s -> Client s -> Step -> IOSim s (Client s)
runStep env@Env{eNode, eBlocks} client = \case
  AddBlock blk -> do
    case blockPrevHash blk of
      GenesisHash -> addAtGenesis eNode blk
      BlockHash h -> addWithCommittee eNode (eBlocks Map.! h) blk
    pure client
  RequestNext n -> foldM (\c _ -> requestNext env c) client [1 .. n]
  Reconnect back -> reconnect env back client

-- | Ask for the next update. At the node's tip, wait a second for the chain to
-- change.
requestNext :: Env s -> Client s -> IOSim s (Client s)
requestNext env client = case cConn client of
  Idle idle ->
    recvMsgRequestNext idle >>= \case
      Left next -> receive env client next
      Right await -> pure client{cConn = Awaiting await}
  Awaiting await -> timeout 1 await >>= maybe (pure client) (receive env client)

-- | Keep asking until the client is at the node's tip.
catchUp :: Env s -> Client s -> IOSim s (Client s)
catchUp env client = case cConn client of
  Idle _ -> requestNext env client >>= catchUp env
  Awaiting await -> timeout 1 await >>= maybe (pure client) (receive env client >=> catchUp env)

-- | Reconnect, offering the client's chain from @back@ blocks behind its tip,
-- then genesis.
reconnect :: Env s -> Int -> Client s -> IOSim s (Client s)
reconnect env back client = do
  cClose client
  (conn, close, followed) <- openConn env
  case conn of
    Awaiting _ -> error "reconnect: a new connection is idle"
    Idle idle ->
      recvMsgFindIntersect idle (take 3 (drop back (cChain client)) <> [GenesisPoint]) >>= \case
        SendMsgIntersectFound point _ server -> do
          idle' <- runChainSyncServer server
          pure
            client
              { cConn = Idle idle'
              , cClose = close
              , cFollowed = followed
              , cChain = dropWhile (/= point) (cChain client)
              , cIntersection = Just point
              , cLabels = "reconnect" : cLabels client
              }
        SendMsgIntersectNotFound _ _ ->
          pure client{cFailures = "no intersection, not even genesis" : cFailures client}

-- | Check one update from the server, and apply it to the client's chain.
receive :: Env s -> Client s -> Next s -> IOSim s (Client s)
receive Env{eNode = Node{nChainDB}, eBlocks, eEbs} client = \case
  SendMsgRollForward sblk _ server -> do
    idle <- runChainSyncServer server
    let blk = decodeBlock sblk
        point = blockPoint blk
    unfollowed <- matchFollower (Forwarded point)
    (failures, labels) <- case Map.lookup (blockHash blk) eBlocks of
      Nothing -> pure (["sent a block the node never had: " <> show point], [])
      Just stored
        | headerContainsLeiosCert (getHeader stored) -> do
            let parent = case blockPrevHash stored of
                  BlockHash h -> Map.lookup h eBlocks
                  GenesisHash -> Nothing
                ebTxs = do
                  (ebPoint, _) <- headerLeiosAnnouncement . getHeader =<< parent
                  Map.lookup ebPoint eEbs
                atAnnouncer = cIntersection client == fmap blockPoint parent
            pure
              ( [ "the CertRB at " <> show point <> " has the wrong transactions"
                | Just (blockTxs blk) /= ebTxs
                ]
                  <> [ "the CertRB at " <> show point <> " lost its certificate"
                     | ltbCert (ltbBody blk) /= ltbCert (ltbBody stored)
                     ]
              , "CertRB" : ["CertRB right after intersecting at its announcer" | atAnnouncer]
              )
        | otherwise -> do
            raw <- ChainDB.getBlockComponent nChainDB GetRawBlock (blockRealPoint stored)
            pure
              ( [ "the block at " <> show point <> " is not sent as stored"
                | raw /= Just (unSerialised sblk)
                ]
              , []
              )
    pure
      client
        { cConn = Idle idle
        , cChain = point : cChain client
        , cIntersection = Nothing
        , cFailures = reverse (unfollowed <> failures) <> cFailures client
        , cLabels = reverse labels <> cLabels client
        }
  SendMsgRollBackward point _ server -> do
    idle <- runChainSyncServer server
    unfollowed <- matchFollower (RolledBack point)
    let (dropped, kept) = break (== point) (cChain client)
        failures =
          unfollowed
            <> [ "rolled back to " <> show point <> ", which the client does not have"
               | point /= GenesisPoint && null kept
               ]
    pure
      client
        { cConn = Idle idle
        , cChain = kept
        , cFailures = reverse failures <> cFailures client
        , cLabels = ["rollback" | not (null dropped)] <> cLabels client
        }
 where
  -- The update must be the follower's next instruction.
  matchFollower sent = atomically $ do
    pending <- readTVar (cFollowed client)
    case pending of
      next : rest | next == sent -> [] <$ writeTVar (cFollowed client) rest
      next : _ -> pure ["sent " <> show sent <> ", but the follower said " <> show next]
      [] -> pure ["sent " <> show sent <> ", which the follower never said"]

-- | The node's selected chain, oldest first. Nothing becomes immutable, since
-- 'withNode' runs no background tasks, so it starts at genesis.
selectedChain :: Env s -> IOSim s [Point Blk]
selectedChain Env{eNode = Node{nChainDB}} = do
  frag <- atomically (ChainDB.getCurrentChain nChainDB)
  unless (AF.anchorPoint frag == GenesisPoint) $
    error "selectedChain: part of the chain is immutable"
  pure (map headerPoint (AF.toOldestFirst frag))

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Poll until the condition holds, for the LeiosDB's background threads.
-- Fails after ten seconds.
awaitIO :: String -> IO Bool -> IO ()
awaitIO what cond = go (1000 :: Int)
 where
  go 0 = assertFailure $ "timed out waiting for " <> what
  go n = do
    done <- cond
    unless done $ threadDelay 10_000 >> go (n - 1)

blockTxs :: Blk -> [LeiosTestTx]
blockTxs = ltbTxs . ltbBody

-- | A block as the ChainDB stores it.
encodeBlock :: Blk -> Serialised Blk
encodeBlock = Serialised . toLazyByteString . encodeDisk LeiosTestCodecConfig

-- | A block as a client decodes it.
decodeBlock :: Serialised Blk -> Blk
decodeBlock (Serialised bs) =
  case deserialiseFromBytes decoder bs of
    Left e -> error $ "decodeBlock: " <> show e
    Right (_, f) -> either (error . ("decodeBlock: " <>) . show) id (f bs)
 where
  decoder :: Decoder s (Lazy.ByteString -> Either DecoderError Blk)
  decoder = decodeDisk LeiosTestCodecConfig
