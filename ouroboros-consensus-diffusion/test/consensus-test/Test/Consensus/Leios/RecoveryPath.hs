{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A node that loses a CertRB to a restart, and gets it back.
--
-- One real node, and peers that are entirely this test's: it decides what each
-- peer holds and when, and the node does the rest by itself --- syncing
-- headers, fetching blocks, verifying the certificate, fetching the endorser
-- block, and selecting.
module Test.Consensus.Leios.RecoveryPath (tests) where

import Cardano.Crypto.DSIGN (signDSIGN)
import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import Control.Monad (unless)
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim
  ( Failure (FailureException)
  , IOSim
  , runSimTrace
  , selectTraceEventsSay'
  , traceResult
  )
import Control.ResourceRegistry (withRegistry)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified LeiosDemoDb as LeiosDb
import qualified LeiosDemoLogic as Leios
import LeiosDemoTypes
  ( BytesSize
  , LeiosCert
  , LeiosEb
  , LeiosPoint (..)
  , LeiosSeatId (..)
  , RbHash (MkRbHash)
  , TxHash
  , Weight
  , aggregateLeiosCert
  )
import LeiosDemoLogic.Announcements.ElBimap (ElId)
import LeiosDemoTypes (PeerId (..), offerings)
import LeiosValidClaims (isCertifiedEb, memberValidClaim, sizeValidClaims)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Config (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB (headerElId)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (forgetFingerprint)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import Test.Cardano.Crypto.Leios.Gen (TestCommittee (..), genCommittee)
import Test.Consensus.Leios.Environment
import Test.Consensus.Leios.NodeUnderTest
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ChainDB (emptyNodeDBs)
import Test.Util.LeiosTestBlock
import Test.Util.Orphans.IOLike ()
import Test.Util.Tracer (recordingTracerTVar)

tests :: TestTree
tests =
  testGroup
    "Leios"
    [ testCase
        "a forgotten CertRB is re-acquired and selected"
        test_forgottenCertRBIsSelectedAgain
    , testCase
        "a peer claiming two certified endorser blocks for one election is dropped"
        test_twoCertificationClaimsIsDropped
    ]

type Blk = LeiosTestBlock

{-------------------------------------------------------------------------------
  What exists in this little network
-------------------------------------------------------------------------------}

-- | The transaction the endorser block endorses. Its effect on the ledger is
-- how the test sees that the closure was applied.
endorsedTx :: LeiosTestTx
endorsedTx =
  LeiosTestTx
    { ltxAsserts = IntMap.singleton 0 Nothing
    , ltxWrites = IntMap.singleton 0 'a'
    }

endorserBlock :: LeiosEb
endorserClosure :: [(TxHash, BS8.ByteString)]
endorserSize :: BytesSize
(endorserBlock, endorserClosure, endorserSize) = mkLeiosTestEb [endorsedTx]

-- | The announcing block: it announces the endorser block, and carries no
-- certificate itself.
announcer :: Blk
announcer =
  announcing
    (leiosTestEbPoint 1 endorserBlock)
    endorserSize
    (firstLeiosBlock 0)

-- | The claim a CertRB extending 'announcer' makes.
announcerClaim :: RbHash
announcerClaim = MkRbHash $ toRawHash (Proxy @Blk) (blockHash announcer)

-- | The CertRB: it certifies the endorser block its predecessor announced.
certRB :: Blk
certRB =
  certifying
    (mkCert testCommittee announcerClaim)
    (successorLeiosBlock announcer)

-- | Two more blocks on the CertRB's chain, which make it the longest once the
-- test reveals them.
afterCertRB :: [Blk]
afterCertRB = take 2 $ iterate successorLeiosBlock $ successorLeiosBlock certRB

-- | A second endorser block for the same election as 'endorserBlock'. Nothing
-- ever certifies it, and the peer that announces it never holds it.
decoyEb :: LeiosEb
decoySize :: BytesSize
(decoyEb, _, decoySize) = mkLeiosTestEb [decoyTx]

-- | Writes where 'endorsedTx' does not, so this is a different endorser block.
decoyTx :: LeiosTestTx
decoyTx =
  LeiosTestTx
    { ltxAsserts = IntMap.singleton 1 Nothing
    , ltxWrites = IntMap.singleton 1 'b'
    }

-- | Announces 'decoyEb' for the election 'announcer' also announces in: the
-- same slot and the same issuer, a different endorser block.
decoyAnnouncer :: Blk
decoyAnnouncer =
  announcing
    (leiosTestEbPoint 1 decoyEb)
    decoySize
    (firstLeiosBlock 2)

-- | A header claiming to certify 'decoyEb', which is what makes the peer
-- serving it an offerer of that endorser block: rolling forward to a CertRB
-- says this peer has selected it, and a peer could only have selected it
-- holding the endorser block.
--
-- This one has not. It never hands the block over --- which no peer can be
-- caught at --- and the certificate on that block is the one from the other
-- chain, certifying an announcement this block does not follow, so the node
-- would reject it if it ever arrived.
decoyCertRB :: Blk
decoyCertRB =
  certifying
    (mkCert testCommittee announcerClaim)
    (successorLeiosBlock decoyAnnouncer)

-- | The chain the rival announcement is on. The node need only see it: it
-- never outranks what the node has already selected, so the node never asks
-- this peer for a block --- and would wait forever if it did, since the peer
-- serves nothing past 'decoyAnnouncer'.
decoyChain :: [Blk]
decoyChain = [decoyAnnouncer, decoyCertRB]

-- | A chain that shares nothing with the announcer and carries no
-- certificates, so the node can select it without needing any endorser block.
betterChain :: [Blk]
betterChain = take 3 $ iterate successorLeiosBlock $ firstLeiosBlock 1

testCommittee :: TestCommittee
testCommittee = unGen genCommittee (mkQCGen 42) 10

-- | A certificate the whole committee signed over this claim.
mkCert :: TestCommittee -> RbHash -> LeiosCert
mkCert TestCommittee{committee, allKeys} msg =
  case aggregateLeiosCert committee sigs of
    Right cert -> cert
    Left e -> error $ "mkCert: " <> show e
 where
  sigs =
    Map.fromList
      [ (LeiosSeatId (fromIntegral i), signDSIGN () msg sk)
      | (i, sk) <- zip [0 :: Int ..] allKeys
      ]

-- | Every seat of 'genCommittee' has weight /n@.
wholeCommittee :: Weight
wholeCommittee = 1 % 1

nodeConfig :: NodeUnderTestConfig
nodeConfig =
  defaultNodeUnderTestConfig
    ( leiosTestLedgerConfig
        (HardFork.defaultEraParams securityParam (slotLengthFromSec 1))
        100
        (const (Just (testCommittee.committee, wholeCommittee)))
    )
    securityParam

-- | Larger than every chain here, so nothing becomes immutable and every block
-- stays where forgetting can reach it.
securityParam :: SecurityParam
securityParam = SecurityParam $ knownNonZeroBounded @8

{-------------------------------------------------------------------------------
  The test
-------------------------------------------------------------------------------}

-- | The scenario of @valid_claims_startup.md@, end to end.
--
-- Before the restart the node learns the CertRB and verifies its certificate,
-- but cannot select it: the endorser block is nowhere to be had, so the CertRB
-- is parked. Then a longer certificate-free chain appears and the node selects
-- that instead, which leaves the CertRB off the selection.
--
-- The restart therefore forgets the CertRB, and the claim goes with it. When
-- the CertRB's chain then becomes the longest, the node has to re-fetch it,
-- re-verify its certificate, and only then --- because the claim is what
-- licenses the fetch --- acquire the endorser block and select the chain.
--
-- The first thing the restarted node hears about that election, though, is a
-- rival announcement, from a peer that also claims to hold a certificate for
-- it and then never hands that block over. So the node is offered a second
-- endorser block for this election on the strength of a lie it cannot check.
--
-- TODO Today's fetch gate ignores every endorser block that is not certified,
-- so the rival changes nothing yet. Once that gate is weakened to something
-- practical, the node will be going after the rival when the real certificate
-- arrives, and will have to move its attention for that election to the
-- announcement that was actually certified.
test_forgottenCertRBIsSelectedAgain :: Assertion
test_forgottenCertRBIsSelectedAgain = do
  assertEqual
    "the two announcements are for one election"
    theElection
    (headerElId (getHeader decoyAnnouncer))
  let simTrace = runSimTrace scenario
  case traceResult False simTrace of
    Left err ->
      assertFailure $
        unlines $
          show err : lastN 60 (selectTraceEventsSay' simTrace)
    Right (beforeRestart, afterRestart) -> checks beforeRestart afterRestart
 where
  scenario :: forall s. IOSim s ((Int, Bool), AfterRestart)
  scenario = do
    nodeDBs <- emptyNodeDBs
    leiosDb <- LeiosDb.newLeiosDBInMemory
    peerA <- newPeerEnv
    peerB <- newPeerEnv

    (chainDBTracer, getTraces) <- recordingTracerTVar
    let await = awaitWith getTraces
        awaitPolling = awaitPollingWith getTraces

    before <-
      withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
        -- The peers live inside the session: closing this registry
        -- disconnects them, which is what a restart does.
        withRegistry $ \registry -> do
          connectPeer nut registry (PeerAddr 0) peerA
          connectPeer nut registry (PeerAddr 1) peerB

          -- The CertRB's chain is all there is, so the node fetches it and
          -- verifies the certificate. It cannot select it: the endorser
          -- block is not on offer anywhere, so the CertRB is parked.
          serveChain peerA $ chainOf [announcer, certRB]
          await "the CertRB arrives" $ isFetchedSTM nut certRB
          await "its claim is established" $ claimEstablishedSTM nut

          -- Only now does the longer chain appear, so the node selects that
          -- and the CertRB is left off the selection.
          serveChain peerB $ chainOf betterChain
          await "the better chain is selected" $
            tipIsSTM nut (last betterChain)

          (,) <$> claimCount nut <*> isFetched nut certRB

    after' <-
      withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
        withRegistry $ \registry -> do
          forgotten <- not <$> isFetched nut certRB
          claimsAtStartUp <- claimCount nut

          -- What the node hears of this election first is the rival
          -- announcement, and the claim of a certificate for it.
          serveChainThrough peerB (blockPoint decoyAnnouncer) $
            chainOf decoyChain
          connectPeer nut registry (PeerAddr 1) peerB
          awaitPolling "the rival endorser block is on offer" $
            ebOfferedBy nut (PeerAddr 1) decoyPoint

          -- Only then can the endorser block be had, and only then does the
          -- CertRB's chain --- which certifies the other announcement of that
          -- same election --- become the longest.
          plantEb peerA endorserBlock endorserClosure
          serveChain peerA $ chainOf ([announcer, certRB] <> afterCertRB)
          connectPeer nut registry (PeerAddr 0) peerA

          await "the CertRB is re-acquired" $ isFetchedSTM nut certRB
          await "its claim is re-established" $ claimEstablishedSTM nut
          await "the endorser block is certified" $
            ebCertifiedSTM nut endorserPoint
          await "the CertRB's chain is selected" $
            tipIsSTM nut (last afterCertRB)

          arRivalCertified <- atomically $ ebCertifiedSTM nut decoyPoint
          arRivalCertRbHeld <- isFetched nut decoyCertRB
          pure
            AfterRestart
              { arForgotten = forgotten
              , arClaimsAtStartUp = claimsAtStartUp
              , arRivalCertified
              , arRivalCertRbHeld
              }

    pure (before, after')

  checks (claimsBefore, fetchedBefore) after' = do
    assertEqual "the claim was established before the restart" 1 claimsBefore
    assertBool "and the CertRB was held" fetchedBefore
    assertBool "the CertRB is forgotten at startup" (arForgotten after')
    assertEqual "and so is its claim" 0 (arClaimsAtStartUp after')
    assertBool
      "the block claiming to certify the rival never arrives"
      (not (arRivalCertRbHeld after'))
    assertBool "so the rival is never certified" (not (arRivalCertified after'))

-- | What the test measures of the restarted node.
data AfterRestart = AfterRestart
  { arForgotten :: Bool
  , arClaimsAtStartUp :: Int
  , arRivalCertified :: Bool
  , arRivalCertRbHeld :: Bool
  }

-- | A peer may claim a certificate for at most one endorser block per
-- election, and the second claim costs it the connection.
--
-- Nothing this peer says is ever checked: it serves the two headers that make
-- the claims and no blocks at all, which is what an adversary would do, since
-- a certificate lives in a block body and a body it never sends is a body we
-- cannot reject. What bounds it is the claim itself.
test_twoCertificationClaimsIsDropped :: Assertion
test_twoCertificationClaimsIsDropped = do
  let simTrace = runSimTrace scenario
  case traceResult False simTrace of
    Left (FailureException e) | isTwoClaims e -> pure ()
    outcome ->
      assertFailure $
        unlines $
          ("expected the peer to be dropped, but: " <> show outcome)
            : lastN 40 (selectTraceEventsSay' simTrace)
 where
  -- The peer's ChainSync client dies of it, and the thread is linked, so the
  -- exception arrives wrapped.
  isTwoClaims :: SomeException -> Bool
  isTwoClaims e
    | Just (ExceptionInLinkedThread _ inner) <- fromException e = isTwoClaims inner
    | Just Leios.ExnLeiosTwoCertificationClaims{} <- fromException e = True
    | otherwise = False

  scenario :: forall s. IOSim s ()
  scenario = do
    nodeDBs <- emptyNodeDBs
    leiosDb <- LeiosDb.newLeiosDBInMemory
    peer <- newPeerEnv

    (chainDBTracer, getTraces) <- recordingTracerTVar

    withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
      withRegistry $ \registry -> do
        -- Headers only, never a block.
        serveChainThrough peer GenesisPoint $ chainOf [announcer, certRB]
        connectPeer nut registry (PeerAddr 0) peer
        awaitPollingWith getTraces "the first claim is in" $
          ebOfferedBy nut (PeerAddr 0) endorserPoint

        -- The same election, a different endorser block, and again a header
        -- claiming a certificate for it.
        serveChainThrough peer GenesisPoint $ chainOf [decoyAnnouncer, decoyCertRB]
        threadDelay 30

{-------------------------------------------------------------------------------
  Watching the node
-------------------------------------------------------------------------------}

-- | Wait for the node to reach a state, or fail --- with what the ChainDB was
-- doing --- rather than hang.
awaitWith ::
  Show ev =>
  IOSim s [ev] ->
  String ->
  STM (IOSim s) Bool ->
  IOSim s ()
awaitWith getTraces what cond =
  awaitAction getTraces what $ atomically $ cond >>= \b -> unless b retry

-- | As 'awaitWith', for what the node keeps in an 'MVar' rather than in STM,
-- which has to be polled.
awaitPollingWith ::
  Show ev =>
  IOSim s [ev] ->
  String ->
  IOSim s Bool ->
  IOSim s ()
awaitPollingWith getTraces what cond = awaitAction getTraces what again
 where
  again = cond >>= \b -> unless b (threadDelay 0.1 >> again)

awaitAction :: Show ev => IOSim s [ev] -> String -> IOSim s () -> IOSim s ()
awaitAction getTraces what action = do
  result <- timeout 30 action
  case result of
    Just () -> pure ()
    Nothing -> do
      evs <- getTraces
      throwIO $
        userError $
          unlines $
            ("timed out waiting for: " <> what)
              : map show (lastN 40 evs)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

chainOf :: [Blk] -> Chain.Chain Blk
chainOf = Chain.fromOldestFirst

isFetchedSTM :: NodeUnderTest (IOSim s) -> Blk -> STM (IOSim s) Bool
isFetchedSTM nut blk =
  ($ blockPoint blk) <$> ChainDB.getIsFetched (nutChainDB nut)

isFetched :: NodeUnderTest (IOSim s) -> Blk -> IOSim s Bool
isFetched nut = atomically . isFetchedSTM nut

tipIsSTM :: NodeUnderTest (IOSim s) -> Blk -> STM (IOSim s) Bool
tipIsSTM nut blk =
  (== blockPoint blk) <$> ChainDB.getTipPoint (nutChainDB nut)

claimEstablishedSTM :: NodeUnderTest (IOSim s) -> STM (IOSim s) Bool
claimEstablishedSTM nut =
  memberValidClaim announcerClaim . forgetFingerprint
    <$> ChainDB.getLeiosValidClaims (nutChainDB nut)

-- | Whether a certificate has named this endorser block for the election both
-- announcements in this test are for.
ebCertifiedSTM :: NodeUnderTest (IOSim s) -> LeiosPoint -> STM (IOSim s) Bool
ebCertifiedSTM nut point =
  isCertifiedEb theElection (pointEbHash point) . forgetFingerprint
    <$> ChainDB.getLeiosValidClaims (nutChainDB nut)

theElection :: ElId
theElection = headerElId (getHeader announcer)

-- | Whether this peer has offered this endorser block. The header claiming to
-- certify it is what puts it here: an announcement alone only tells the node
-- the endorser block exists, and leaves it with nobody to ask for it.
ebOfferedBy :: NodeUnderTest (IOSim s) -> PeerAddr -> LeiosPoint -> IOSim s Bool
ebOfferedBy nut addr point = do
  peers <- LazySTM.readTVarIO (getLeiosPeersVars (nutKernel nut))
  case Map.lookup (MkPeerId (ConnectionId addr addr)) peers of
    Nothing -> pure False
    Just peerVars -> Map.member point <$> MVar.readMVar (offerings peerVars)

endorserPoint :: LeiosPoint
endorserPoint = leiosTestEbPoint 1 endorserBlock

decoyPoint :: LeiosPoint
decoyPoint = leiosTestEbPoint 1 decoyEb

claimCount :: NodeUnderTest (IOSim s) -> IOSim s Int
claimCount nut =
  sizeValidClaims . forgetFingerprint
    <$> atomically (ChainDB.getLeiosValidClaims (nutChainDB nut))
