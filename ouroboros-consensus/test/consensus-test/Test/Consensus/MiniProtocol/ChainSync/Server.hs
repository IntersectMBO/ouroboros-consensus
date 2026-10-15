{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for 'inlineCertifiedEbsFollower': which blocks a node-to-client
-- ChainSync client gets when the chain has Leios blocks.
--
-- Each test gives the server a list of chain changes, in batches. After each
-- batch, the client reads everything the server has for it. Several batches
-- act like a client that is up to date, getting new blocks as they arrive. One
-- batch acts like a client that is behind and catching up.
module Test.Consensus.MiniProtocol.ChainSync.Server (tests) where

import Codec.Serialise (deserialise, serialise)
import Control.Monad (void)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import LeiosDemoTypes (BytesSize, LeiosCert, LeiosEb, LeiosPoint, RbHash (..), TxHash)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server (inlineCertifiedEbsFollower)
import Ouroboros.Consensus.Storage.ChainDB.API (Follower (..), WithPoint (..))
import qualified Ouroboros.Consensus.Storage.LeiosDB as LeiosDb
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block (ChainUpdate (..), Serialised (..))
import Test.Cardano.Crypto.Leios.Gen (TestCommittee, genCommittee)
import Test.LeiosValidClaims (mkCert)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.LeiosTestBlock
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "ChainSync.Server (inlining certified EBs)"
    [ testGroup "reading without waiting" (map (scenario NoWait) scenarios)
    , testGroup "reading and waiting, as a client at the tip does" (map (scenario Wait) scenarios)
    , testGroup
        "reading as the server does, with blocks arriving while it waits"
        (map (scenario AsServer) scenarios)
    ]
 where
  scenario how (name, batches, expected) = testCase name $ run how batches @?= expected

-- | A name, the chain changes in batches, and what the client must get.
scenarios :: [(String, [[ChainUpdate Blk Blk]], [ChainUpdate Blk Blk])]
scenarios =
  [
    ( "a client catching up gets RB⁺ directly, with no rollback"
    , [[add b0, add rb, add certRb, add b3]]
    , [add b0, add rbPlus, add certRb, add b3]
    )
  ,
    ( "a client at the tip gets RB, then a rollback, RB⁺ and the CertRB"
    , [[add b0, add rb], [add certRb, add b3]]
    , [add b0, add rb, RollBack (blockPoint b0), add rbPlus, add certRb, add b3]
    )
  ,
    ( "RB is sent plain when its EB is not certified (catching up)"
    , [[add b0, add rb, add uncertified]]
    , [add b0, add rb, add uncertified]
    )
  ,
    ( "RB is sent plain when its EB is not certified (at the tip)"
    , [[add b0, add rb], [add uncertified]]
    , [add b0, add rb, add uncertified]
    )
  ,
    ( "RB is sent plain when a rollback comes next"
    , [[add b0, add rb, RollBack (blockPoint b0)]]
    , [add b0, add rb, RollBack (blockPoint b0)]
    )
  ,
    ( "a rollback that keeps RB still leads to RB⁺ when the CertRB comes"
    , [[add b0, add rb], [RollBack (blockPoint rb), add certRb]]
    , [add b0, add rb, RollBack (blockPoint rb), RollBack (blockPoint b0), add rbPlus, add certRb]
    )
  ,
    ( "a rollback past RB forgets it, so a later CertRB is sent as it is"
    , [[add b0, add rb], [RollBack (blockPoint b0), add certRbOnFork]]
    , [add b0, add rb, RollBack (blockPoint b0), add certRbOnFork]
    )
  ,
    ( "a CertRB that announces an EB of its own is its RB⁺ (catching up)"
    , [[add b0, add rb, add certRbAnnouncing, add certRb2]]
    , [add b0, add rbPlus, add certRbAnnouncingPlus, add certRb2]
    )
  ,
    ( "a CertRB that announces an EB of its own is its RB⁺ (at the tip)"
    , [[add b0, add rb], [add certRbAnnouncing], [add certRb2]]
    ,
      [ add b0
      , add rb
      , RollBack (blockPoint b0)
      , add rbPlus
      , add certRbAnnouncing
      , RollBack (blockPoint rb)
      , add certRbAnnouncingPlus
      , add certRb2
      ]
    )
  ]

type Blk = LeiosTestBlock

add :: Blk -> ChainUpdate Blk Blk
add = AddBlock

{-------------------------------------------------------------------------------
  The chain
-------------------------------------------------------------------------------}

tx :: Int -> LeiosTestTx
tx k = LeiosTestTx{ltxAsserts = IntMap.empty, ltxWrites = IntMap.singleton k 'a'}

-- | An ordinary block with a transaction of its own.
b0 :: Blk
b0 = withTxs [tx 0] (firstLeiosBlock 0)

-- | RB: it has a transaction of its own, and it announces 'eb'.
rb :: Blk
rb = withTxs [tx 1] (announcing (ebPoint ebSlot eb) ebSize (successorLeiosBlock b0))
 where
  ebSlot = blockSlot (successorLeiosBlock b0)

-- | What RB⁺ must be: RB's own transaction, then the EB's.
rbPlus :: Blk
rbPlus = withTxs ([tx 1] <> ebTxs) rb

-- | The CertRB over 'rb'.
certRb :: Blk
certRb = certifying (certFor rb) (successorLeiosBlock rb)

-- | A block after 'rb' that does not certify its EB.
uncertified :: Blk
uncertified = withTxs [tx 9] (successorLeiosBlock rb)

b3 :: Blk
b3 = successorLeiosBlock certRb

-- | A CertRB on a fork after 'b0', where there is no 'rb'.
certRbOnFork :: Blk
certRbOnFork = certifying (certFor b0) (forkLeiosBlock (successorLeiosBlock b0))

-- | A CertRB over 'rb' that also announces 'eb2'.
certRbAnnouncing :: Blk
certRbAnnouncing =
  announcing (ebPoint ebSlot eb2) eb2Size $ certifying (certFor rb) (successorLeiosBlock rb)
 where
  ebSlot = blockSlot (successorLeiosBlock rb)

certRbAnnouncingPlus :: Blk
certRbAnnouncingPlus = withTxs eb2Txs certRbAnnouncing

-- | The CertRB over 'certRbAnnouncing'.
certRb2 :: Blk
certRb2 = certifying (certFor certRbAnnouncing) (successorLeiosBlock certRbAnnouncing)

{-------------------------------------------------------------------------------
  The endorser blocks
-------------------------------------------------------------------------------}

ebTxs, eb2Txs :: [LeiosTestTx]
ebTxs = [tx 2, tx 3]
eb2Txs = [tx 4]

eb, eb2 :: LeiosEb
ebSize, eb2Size :: BytesSize
ebClosure, eb2Closure :: [(TxHash, BS.ByteString)]
(eb, ebClosure, ebSize) = mkLeiosTestEb ebTxs
(eb2, eb2Closure, eb2Size) = mkLeiosTestEb eb2Txs

ebPoint :: SlotNo -> LeiosEb -> LeiosPoint
ebPoint = leiosTestEbPoint

{-------------------------------------------------------------------------------
  Certificates
-------------------------------------------------------------------------------}

-- | The server does not check certificates, so any will do.
certFor :: Blk -> LeiosCert
certFor announcer = mkCert testCommittee claim
 where
  claim :: RbHash
  claim = MkRbHash $ toRawHash (Proxy @Blk) (blockHash announcer)

testCommittee :: TestCommittee
testCommittee = unGen genCommittee (mkQCGen 42) 10

{-------------------------------------------------------------------------------
  Running the server
-------------------------------------------------------------------------------}

-- | How the client reads.
data Reading
  = -- | Each batch is added, then the client reads it with
    -- 'followerInstruction'.
    NoWait
  | -- | Each batch is added, then the client reads it with
    -- 'followerInstructionBlocking'.
    Wait
  | -- | As the ChainSync server does: 'followerInstruction', and when that has
    -- nothing, 'followerInstructionBlocking'. Each batch arrives while the
    -- client waits.
    AsServer

-- | Store the EBs, then play the batches. After each batch, the client reads
-- everything the server has for it. Returns all the client got.
run :: Reading -> [[ChainUpdate Blk Blk]] -> [ChainUpdate Blk Blk]
run how batches = runSimOrThrow go
 where
  go :: forall s. IOSim s [ChainUpdate Blk Blk]
  go = do
    db <- LeiosDb.newLeiosDBInMemory
    LeiosDb.withWriter db $ \w -> do
      storeEb w (ebPoint (blockSlot rb) eb) eb ebSize ebClosure
      storeEb w (ebPoint (blockSlot certRbAnnouncing) eb2) eb2 eb2Size eb2Closure
    LeiosDb.withReader db $ \reader -> do
      (scriptVar, inner) <- scriptedFollower
      outer <- inlineCertifiedEbsFollower LeiosTestCodecConfig reader inner
      let push batch = atomically $ modifyTVar scriptVar (<> map (fmap toInner) batch)
      case how of
        AsServer -> do
          void $ forkIO $ mapM_ (\batch -> threadDelay 1 >> push batch) batches
          drain outer
        _ -> concat <$> mapM (\batch -> push batch >> drain outer) batches

  storeEb w pt body size closure = do
    p1 <- LeiosDb.writeEbPoint w pt size
    p2 <- LeiosDb.writeEbBody w pt body []
    p3 <- LeiosDb.writeTxs w pt [(off, bs) | (off, (_, bs)) <- zip [0 ..] closure]
    void (LeiosDb.await p1) >> void (LeiosDb.await p2) >> void (LeiosDb.await p3)

  toInner :: Blk -> WithPoint Blk (Header Blk, Serialised Blk)
  toInner b = WithPoint (getHeader b, Serialised (serialise b)) (blockPoint b)

  drain ::
    forall s. Follower (IOSim s) Blk (WithPoint Blk (Serialised Blk)) -> IOSim s [ChainUpdate Blk Blk]
  drain outer =
    next >>= \case
      Nothing -> pure []
      Just u -> (fmap (\(WithPoint (Serialised bs) _) -> deserialise bs) u :) <$> drain outer
   where
    next = case how of
      NoWait -> followerInstruction outer
      Wait -> waitForNext
      AsServer -> followerInstruction outer >>= maybe waitForNext (pure . Just)

    -- Nothing comes for two seconds: everything has been read.
    waitForNext = either (const Nothing) Just <$> race (threadDelay 2) (followerInstructionBlocking outer)

-- | A follower that plays the updates in the returned variable, in order.
-- With none left, it is at the tip.
scriptedFollower :: IOLike m => m (StrictTVar m [ChainUpdate Blk b], Follower m Blk b)
scriptedFollower = do
  scriptVar <- uncheckedNewTVarM []
  let pop = do
        script <- readTVar scriptVar
        case script of
          [] -> pure Nothing
          u : us -> writeTVar scriptVar us >> pure (Just u)
  pure
    ( scriptVar
    , Follower
        { followerInstruction = atomically pop
        , followerInstructionBlocking = atomically $ pop >>= maybe retry pure
        , followerForward = \_ -> pure Nothing
        , followerClose = pure ()
        }
    )
