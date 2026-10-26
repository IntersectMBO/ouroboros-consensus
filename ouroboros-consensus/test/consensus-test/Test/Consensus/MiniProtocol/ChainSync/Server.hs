{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad (unless)
import Control.ResourceRegistry (withRegistry)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.IntMap.Strict as IntMap
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
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDBImpl
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , newLeiosDBInMemory
  , withLeiosDBSQLite
  , withReader
  )
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))
import Ouroboros.Consensus.Util.IOLike (atomically)
import Ouroboros.Network.Block (Serialised (..), Tip, getTipPoint)
import Ouroboros.Network.Protocol.ChainSync.Client
import Ouroboros.Network.Protocol.ChainSync.Server (chainSyncServerPeer)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.LeiosTestNode
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ChainDB
import Test.Util.LeiosTestBlock

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
addChain :: Node IO -> IO ()
addChain node =
  sequence_ $ addAtGenesis node rb1 : zipWith (addWithCommittee node) chain (drop 1 chain)

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
