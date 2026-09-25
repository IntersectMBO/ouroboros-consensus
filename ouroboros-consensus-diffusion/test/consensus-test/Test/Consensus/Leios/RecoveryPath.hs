{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A node that loses a CertRB to a restart, and gets it back.
--
-- One real node, and peers that are entirely this test's: it decides what each
-- peer holds and when, and the node does the rest by itself --- syncing
-- headers, fetching blocks, verifying the certificate, fetching the endorser
-- block, and selecting.
--
-- TODO The name no longer fits what is here. Only the first test is about the
-- Recovery Path; the rest are about what the node will say to a peer and what
-- it will believe from one. Either rename this module for the harness it
-- actually is, or float those tests out into their own.
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
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ratio ((%))
import qualified LeiosDemoDb as LeiosDb
import qualified LeiosDemoLogic as Leios
import LeiosDemoLogic.Announcements (ShouldRelay (..))
import LeiosDemoLogic.Announcements.ElBimap (ElId)
import LeiosDemoTypes
  ( BytesSize
  , LeiosCert
  , LeiosEb
  , LeiosPoint (..)
  , LeiosSeatId (..)
  , PeerId (..)
  , RbHash (MkRbHash)
  , TxHash
  , Weight
  , aggregateLeiosCert
  , offerings
  )
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
        "a certified endorser block is fetched though a rival took the focus first"
        test_certifiedRivalIsFetched
    , testCase
        "a peer claiming two certified endorser blocks for one election is dropped"
        test_twoCertificationClaimsIsDropped
    , testCase
        "a peer offering an endorser block it never announced is dropped"
        (test_unannouncedOfferIsDropped (\peer -> offerEb peer endorserPoint endorserSize))
    , testCase
        "a peer offering the closure of one it never announced is dropped"
        (test_offerSequence PeerDropped (`offerEbTxs` endorserPoint))
    , testCase
        "a peer offering one endorser block's body twice is dropped"
        ( test_offerSequence PeerDropped $ \peer -> do
            announceEb peer (getHeader announcer)
            offerEb peer endorserPoint endorserSize
            offerEb peer endorserPoint endorserSize
        )
    , testCase
        "a peer offering one endorser block's closure twice is dropped"
        ( test_offerSequence PeerDropped $ \peer -> do
            announceEb peer (getHeader announcer)
            offerEbTxs peer endorserPoint
            offerEbTxs peer endorserPoint
        )
    , testCase
        "a peer may offer a size no announcement claimed"
        ( test_offerSequence PeerKept $ \peer -> do
            announceEb peer (getHeader announcer)
            offerEb peer endorserPoint (endorserSize + 1)
        )
    , testCase
        "a peer may offer the closure without offering the body"
        ( test_offerSequence PeerKept $ \peer -> do
            announceEb peer (getHeader announcer)
            offerEbTxs peer endorserPoint
        )
    , testCase
        "a peer re-offering one the fetch logic already evicted is dropped"
        test_reofferAfterEviction
    , testCase
        "every offer the node makes a peer follows its announcement to that peer"
        test_offerFollowsAnnouncementPerPeer
    , testCase
        "an endorser block misstating a size is not offered onward (transaction first)"
        (test_lyingSizeIsNotOffered TxBeforeBody)
    , testCase
        "an endorser block misstating a size is not offered onward (body first)"
        (test_lyingSizeIsNotOffered BodyBeforeTx)
    , testCase
        "a request for an endorser block we do not have is refused"
        (test_requestIsRefused NothingHeld (`requestEb` honestPoint))
    , testCase
        "a request for an offset the endorser block does not have is refused"
        ( test_requestIsRefused ClosureHeld $ \peer ->
            requestEbTxs peer honestPoint (Leios.offsetsToBitmap (IntSet.fromList [0, 1]))
        )
    , testCase
        "a request for a transaction we do not hold is refused"
        ( test_requestIsRefused BodyOnlyHeld $ \peer ->
            requestEbTxs peer honestPoint (Leios.offsetsToBitmap (IntSet.singleton 0))
        )
    , testCase
        "an endorser block too near the immutable tip is not offered onward"
        (test_offeredOnlyAboveTheLead DoNotRelay)
    , testCase
        "one far enough above it is"
        (test_offeredOnlyAboveTheLead DoRelay)
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

-- | An endorser block announced in slot 2 rather than slot 1. Since nothing
-- in these tests becomes immutable, that is the difference between being
-- inside 'nutcMinOfferLead' of the immutable tip and being clear of it.
freshEb :: LeiosEb
freshClosure :: [(TxHash, BS8.ByteString)]
freshSize :: BytesSize
(freshEb, freshClosure, freshSize) = mkLeiosTestEb [freshTx]

-- | Writes where 'endorsedTx' does not, so this is a different endorser block.
freshTx :: LeiosTestTx
freshTx =
  LeiosTestTx
    { ltxAsserts = IntMap.singleton 2 Nothing
    , ltxWrites = IntMap.singleton 2 'c'
    }

freshPoint :: LeiosPoint
freshPoint = leiosTestEbPoint 2 freshEb

-- | 'freshEb' announced and then certified, on a fork of its own. The
-- announcing block is in slot 2, so it takes a block ahead of it to get
-- there.
freshChain :: [Blk]
freshChain = [freshRoot, freshAnnouncer, freshCertRB]
 where
  freshRoot = firstLeiosBlock 3
  freshAnnouncer =
    announcing freshPoint freshSize $
      successorLeiosBlock freshRoot
  freshCertRB =
    certifying
      (mkCert testCommittee (MkRbHash (toRawHash (Proxy @Blk) (blockHash freshAnnouncer))))
      (successorLeiosBlock freshAnnouncer)

-- | A transaction that two endorser blocks name: 'honestEb', which states its
-- size correctly, and 'lyingEb', which does not.
sharedTx :: LeiosTestTx
sharedTx =
  LeiosTestTx
    { ltxAsserts = IntMap.singleton 4 Nothing
    , ltxWrites = IntMap.singleton 4 'd'
    }

-- | Names 'sharedTx' at its true size. The node fetches this one's closure,
-- which is how 'sharedTx' comes to be in its LeiosDb at all.
honestEb :: LeiosEb
honestClosure :: [(TxHash, BS8.ByteString)]
honestSize :: BytesSize
(honestEb, honestClosure, honestSize) = mkLeiosTestEb [sharedTx]

honestPoint :: LeiosPoint
honestPoint = leiosTestEbPoint 2 honestEb

-- | The header that announces 'honestEb'. No peer ever serves it as part of a
-- chain: this test is entirely about LeiosNotify and LeiosFetch, so the only
-- thing said about it is the announcement itself.
honestAnnouncer :: Blk
honestAnnouncer =
  announcing honestPoint honestSize $
    successorLeiosBlock (firstLeiosBlock 4)

-- | Names the same transaction as 'honestEb', at one byte.
--
-- Nothing about this endorser block is detectably wrong on arrival: it is its
-- own block, for its own election, and its body is exactly the size its
-- announcement claims. The lie is inside the body, about a transaction the
-- node is not going to fetch.
lyingEb :: LeiosEb
lyingSize :: BytesSize
(lyingEb, _, lyingSize) = mkLeiosTestEbClaiming [(sharedTx, 1)]

lyingPoint :: LeiosPoint
lyingPoint = leiosTestEbPoint 4 lyingEb

-- | Announced in slot 4, so this is a different election from 'honestEb''s.
lyingAnnouncer :: Blk
lyingAnnouncer =
  announcing lyingPoint lyingSize $
    iterate successorLeiosBlock (firstLeiosBlock 5) !! 3

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

-- | Which of the lying endorser block's body and the transaction it misstates
-- reaches the node first. They complete its closure at different places ---
-- the body's own insert, or the arrival of the last transaction it waited on
-- --- so each has to be checked.
data LieArrival = TxBeforeBody | BodyBeforeTx
  deriving Show

-- | An endorser block may not be offered onward on the strength of a closure
-- the node never checked.
--
-- The node first acquires 'honestEb', whose body states 'sharedTx''s size
-- correctly, so the transaction lands in its LeiosDb at its true size. Then a
-- second peer announces 'lyingEb', which names that same transaction at one
-- byte. The node fetches that body --- it matches its announced hash and size,
-- so there is nothing to object to --- and the LeiosDb finds every transaction
-- it names already present, because presence is keyed by transaction hash
-- alone. The closure is therefore declared complete without a single byte being
-- fetched for it, and never having been compared against what the body claimed.
--
-- So the node tells its downstream peers it holds a closure matching a body
-- that lies about its own size. A peer that believes it, and asks for the
-- transactions, is served far more than the body led it to expect: here one
-- byte becomes the whole of 'sharedTx', and in general an endorser block whose
-- body is well under the size ceiling can name a closure of unbounded size.
--
-- Only the closure offer is at issue. Offering the body is honest --- the node
-- does hold that reference list, at the hash and size announced.
--
-- Both arrival orders are run, because the closure is completed at a different
-- place in each: by the body's own insert when the transaction is already
-- held, and by the arrival of the last transaction it was waiting on when it
-- is not. Both end in the same offer.
--
-- The node does own a check that would catch this --- 'checkJobSize' compares
-- arriving transactions against the sizes the body claimed --- but it only
-- runs on what the node fetches. No peer here offers the lying closure, so
-- there is nothing to fetch for it: the transaction arrives on the honest
-- endorser block's fetch, under the size that block states, and completes the
-- lying one as a side-effect.
--
-- TODO Both cases fail. A bugfix commit is imminent.
test_lyingSizeIsNotOffered :: LieArrival -> Assertion
test_lyingSizeIsNotOffered arrival = do
  let simTrace = runSimTrace scenario
  case traceResult False simTrace of
    Right (bodyOffers, closureOffers) -> do
      -- Without this the test could pass for the wrong reason: if the node
      -- never offered the listener anything about this endorser block, there
      -- was no closure offer to catch.
      assertBool
        "the node never offered this endorser block's body, so the test proves nothing"
        (lyingPoint `elem` bodyOffers)
      assertEqual
        "the node offered a closure for an endorser block that misstates a size"
        []
        (filter (== lyingPoint) closureOffers)
    outcome ->
      assertFailure $
        unlines $
          ("expected " <> show arrival <> " to be caught, but: " <> show outcome)
            : lastN 40 (selectTraceEventsSay' simTrace)
 where
  scenario :: forall s. IOSim s ([LeiosPoint], [LeiosPoint])
  scenario = do
    nodeDBs <- emptyNodeDBs
    leiosDb <- LeiosDb.newLeiosDBInMemory
    holder <- newPeerEnv
    listener <- newPeerEnv
    liar <- newPeerEnv

    (chainDBTracer, getTraces) <- recordingTracerTVar

    withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
      withRegistry $ \registry -> do
        -- The listener joins first, so the node relays it every announcement
        -- and its own send-side gate is never what keeps an offer away.
        connectPeer nut registry (PeerAddr 1) listener
        connectPeer nut registry (PeerAddr 0) holder
        connectPeer nut registry (PeerAddr 2) liar

        -- The liar's body is genuine and genuinely the size it announced;
        -- only the size inside it is a lie. It never offers the closure, so
        -- the only way the node can come to hold that transaction is the
        -- honest endorser block's own fetch.
        plantEb liar lyingEb []
        plantEb holder honestEb honestClosure

        -- Both announcing headers sit in slots this simulation starts before,
        -- and a header from the future is rejected as such. Nothing here turns
        -- on when they arrive, only on the order they arrive in.
        threadDelay 6

        let servesTheTransaction = do
              announceEb holder (getHeader honestAnnouncer)
              offerEb holder honestPoint honestSize
              offerEbTxs holder honestPoint
              awaitPollingWith getTraces "the shared transaction is in the LeiosDb" $
                LeiosDb.withReader leiosDb $ \r ->
                  isJust <$> LeiosDb.lookupTrustedEbClosure r (pointEbHash honestPoint)
            servesTheLie = do
              announceEb liar (getHeader lyingAnnouncer)
              offerEb liar lyingPoint lyingSize
              awaitPollingWith getTraces "the lying body is in the LeiosDb" $
                LeiosDb.withReader leiosDb $ \r ->
                  not . null <$> LeiosDb.lookupEbBody r (pointEbHash lyingPoint)

        case arrival of
          TxBeforeBody -> servesTheTransaction >> servesTheLie
          BodyBeforeTx -> servesTheLie >> servesTheTransaction
        threadDelay 5

        (,) <$> heardBodyOffers listener <*> heardClosureOffers listener

-- | How much of 'honestEb' the node has acquired before the request arrives.
data WhatIsHeld
  = -- | Nothing at all; no peer ever offers it.
    NothingHeld
  | -- | The body, from a peer that then withholds the transactions.
    BodyOnlyHeld
  | -- | The body and the whole closure.
    ClosureHeld

-- | The node refuses a LeiosFetch request it cannot answer, which costs the
-- asking peer its connection.
--
-- It does not check that it ever /offered/ what was asked for --- that would
-- need state the LeiosFetch server does not keep --- only that it can answer.
--
-- The empty-body case is the one that used to be silently wrong: the server
-- built an endorser block out of the nothing it found and sent that, a reply
-- the requester cannot tell from a real one until it hashes it.
test_requestIsRefused ::
  WhatIsHeld -> (forall s. PeerEnv (IOSim s) -> IOSim s ()) -> Assertion
test_requestIsRefused held asksIt = do
  let simTrace = runSimTrace scenario
  case traceResult False simTrace of
    Left (FailureException e) | isInvalidRequest e -> pure ()
    outcome ->
      assertFailure $
        unlines $
          ("expected the request to be refused, but: " <> show outcome)
            : lastN 40 (selectTraceEventsSay' simTrace)
 where
  isInvalidRequest :: SomeException -> Bool
  isInvalidRequest e
    | Just (ExceptionInLinkedThread _ inner) <- fromException e = isInvalidRequest inner
    | Just (_ :: Leios.ExnLeiosInvalidRequest) <- fromException e = True
    | otherwise = False

  scenario :: forall s. IOSim s ()
  scenario = do
    nodeDBs <- emptyNodeDBs
    leiosDb <- LeiosDb.newLeiosDBInMemory
    holder <- newPeerEnv
    greedy <- newPeerEnv

    (chainDBTracer, getTraces) <- recordingTracerTVar

    withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
      withRegistry $ \registry -> do
        connectPeer nut registry (PeerAddr 0) holder
        connectPeer nut registry (PeerAddr 1) greedy

        -- The announcing header is in slot 2, which this simulation starts
        -- before, and a header from the future is rejected as such.
        threadDelay 8

        case held of
          NothingHeld -> pure ()
          BodyOnlyHeld -> do
            -- An empty closure is a peer that has the body and never hands
            -- the transactions over, so the node is left holding one without
            -- the other.
            plantEb holder honestEb []
            announceEb holder (getHeader honestAnnouncer)
            offerEb holder honestPoint honestSize
            awaitPollingWith getTraces "the body is in the LeiosDb" $
              LeiosDb.withReader leiosDb $ \r ->
                not . null <$> LeiosDb.lookupEbBody r (pointEbHash honestPoint)
          ClosureHeld -> do
            plantEb holder honestEb honestClosure
            announceEb holder (getHeader honestAnnouncer)
            offerEb holder honestPoint honestSize
            offerEbTxs holder honestPoint
            awaitPollingWith getTraces "the closure is in the LeiosDb" $
              LeiosDb.withReader leiosDb $ \r ->
                isJust <$> LeiosDb.lookupTrustedEbClosure r (pointEbHash honestPoint)

        asksIt greedy
        threadDelay 30

-- | The node offers an endorser block to its downstream peers only once the
-- endorser block's slot is 'nutcMinOfferLead' above the node's own immutable
-- tip, so that a peer whose immutable tip is a little ahead of ours does not
-- disconnect us for offering below it.
--
-- Nothing here ever becomes immutable --- 'securityParam' sees to that --- so
-- the immutable tip stays at genesis and the lead alone decides. The endorser
-- block of 'announcer' is in slot 1, inside it; 'freshEb' is in slot 2, clear
-- of it. Either way the node acquires the endorser block, which is what makes
-- this a test of the relaying and not of the fetching.
test_offeredOnlyAboveTheLead :: ShouldRelay -> Assertion
test_offeredOnlyAboveTheLead shouldRelay = do
  let simTrace = runSimTrace scenario
  case traceResult False simTrace of
    Left err ->
      assertFailure $
        unlines $
          show err : lastN 60 (selectTraceEventsSay' simTrace)
    Right heard ->
      assertEqual
        ("the node relays " <> show point <> "?")
        (case shouldRelay of DoRelay -> True; DoNotRelay -> False)
        (point `elem` heard)
 where
  (point, eb, closure, chain) = case shouldRelay of
    DoNotRelay -> (endorserPoint, endorserBlock, endorserClosure, [announcer, certRB])
    DoRelay -> (freshPoint, freshEb, freshClosure, freshChain)

  scenario :: forall s. IOSim s [LeiosPoint]
  scenario = do
    nodeDBs <- emptyNodeDBs
    leiosDb <- LeiosDb.newLeiosDBInMemory
    holder <- newPeerEnv
    listener <- newPeerEnv

    (chainDBTracer, getTraces) <- recordingTracerTVar

    withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
      withRegistry $ \registry -> do
        connectPeer nut registry (PeerAddr 0) holder
        connectPeer nut registry (PeerAddr 1) listener

        -- The one peer holds everything, so the node acquires the endorser
        -- block and selects the chain that certifies it.
        plantEb holder eb closure
        serveChain holder $ chainOf chain
        awaitWith getTraces "the certified chain is selected" $
          tipIsSTM nut (last chain)

        -- Whatever the node was going to say to the other peer, it has had
        -- the chance to.
        threadDelay 5
        heardOffers listener

-- | What the test measures of the restarted node.
data AfterRestart = AfterRestart
  { arForgotten :: Bool
  , arClaimsAtStartUp :: Int
  , arRivalCertified :: Bool
  , arRivalCertRbHeld :: Bool
  }

-- | An election whose focus is already on one endorser block must still end up
-- fetching the one a verified certificate names.
--
-- The node sees the rival announcement first, so the election focuses on
-- 'decoyEb' and the later announcement of 'endorserBlock' --- same election ---
-- does not take the focus, and so does not list it either. Only the
-- certificate moves the focus, and by then something must already have listed
-- 'endorserBlock' as one to fetch, or the CertRB is parked forever.
--
-- Today the CertRB's own roll-forward is what lists it, on the strength of a
-- claim nothing has verified yet. That is due to change, since an unverified
-- claim should not make us track an endorser block --- at which point the
-- listing has to come from the focus moving instead, and this test is what
-- says the endorser block is still fetched either way.
test_certifiedRivalIsFetched :: Assertion
test_certifiedRivalIsFetched = do
  assertEqual
    "the two announcements are for one election"
    theElection
    (headerElId (getHeader decoyAnnouncer))
  let simTrace = runSimTrace scenario
  case traceResult False simTrace of
    Right () -> pure ()
    outcome ->
      assertFailure $
        unlines $
          ("expected the certified chain to be selected, but: " <> show outcome)
            : lastN 40 (selectTraceEventsSay' simTrace)
 where
  scenario :: forall s. IOSim s ()
  scenario = do
    nodeDBs <- emptyNodeDBs
    leiosDb <- LeiosDb.newLeiosDBInMemory
    holder <- newPeerEnv
    rival <- newPeerEnv

    (chainDBTracer, getTraces) <- recordingTracerTVar

    withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
      withRegistry $ \registry -> do
        connectPeer nut registry (PeerAddr 0) holder
        connectPeer nut registry (PeerAddr 1) rival

        -- The rival's announcement is the first this election sees, so it is
        -- the one the election focuses on. The rival never holds that
        -- endorser block, so nothing ever certifies it.
        serveChain rival $ chainOf [decoyAnnouncer]
        awaitWith getTraces "the rival announcement is selected" $
          tipIsSTM nut decoyAnnouncer

        -- Only then the chain that announces and certifies the other one.
        plantEb holder endorserBlock endorserClosure
        serveChain holder $ chainOf ([announcer, certRB] <> afterCertRB)
        awaitWith getTraces "the certified chain is selected" $
          tipIsSTM nut (last afterCertRB)

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

-- | A peer may offer over LeiosNotify only an endorser block it has itself
-- announced there. The size it names is its own business, and its body offer
-- and closure offer are independent, but neither may be sent for something it
-- never announced.
--
-- This peer serves no chain at all and says one thing: the offer. Since it
-- never announced the endorser block, that alone must cost it the connection.
test_unannouncedOfferIsDropped ::
  (forall s. PeerEnv (IOSim s) -> IOSim s ()) -> Assertion
test_unannouncedOfferIsDropped = test_offerSequence PeerDropped

-- | Requesting an endorser block's body evicts the offer that prompted it, so
-- the fetch logic's per-peer offers cannot say what a peer has already
-- offered us.
--
-- Which is why the peer's offers are remembered beside its announcements, in
-- the LeiosNotify client: an offer it repeats after that eviction still costs
-- it the connection. This test waits for the eviction before repeating it.
test_reofferAfterEviction :: Assertion
test_reofferAfterEviction = do
  let simTrace = runSimTrace scenario
  case traceResult False simTrace of
    Left (FailureException e) | isInvalidOffer e -> pure ()
    outcome ->
      assertFailure $
        unlines $
          ("expected the peer to be dropped, but: " <> show outcome)
            : lastN 40 (selectTraceEventsSay' simTrace)
 where
  isInvalidOffer :: SomeException -> Bool
  isInvalidOffer e
    | Just (ExceptionInLinkedThread _ inner) <- fromException e =
        isInvalidOffer inner
    | Just (_ :: Leios.ExnLeiosInvalidOffer) <- fromException e = True
    | otherwise = False

  scenario :: forall s. IOSim s ()
  scenario = do
    nodeDBs <- emptyNodeDBs
    leiosDb <- LeiosDb.newLeiosDBInMemory
    peer <- newPeerEnv

    (chainDBTracer, getTraces) <- recordingTracerTVar

    withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
      withRegistry $ \registry -> do
        connectPeer nut registry (PeerAddr 0) peer

        -- The announcement focuses the election, so the body offer is one the
        -- node acts on: it asks this peer for the body --- which this peer
        -- never plants, so the request stays outstanding --- and drops the
        -- offer that prompted it.
        announceEb peer (getHeader announcer)
        offerEb peer endorserPoint endorserSize
        awaitPollingWith getTraces "the body offer is recorded" $
          ebOfferedBy nut (PeerAddr 0) endorserPoint
        awaitPollingWith getTraces "and then evicted" $
          not <$> ebOfferedBy nut (PeerAddr 0) endorserPoint

        offerEb peer endorserPoint endorserSize
        threadDelay 30

-- | A peer that joins after we relayed an announcement must not then be sent
-- an offer of that endorser block: by the rule this node itself enforces, an
-- offer that its announcement did not precede costs the sender the connection.
--
-- The node relays the announcement while only the holder is connected, so the
-- listener misses it, and only then does the endorser block become available
-- to fetch --- which is what sets the offer going after the listener has
-- joined. It uses 'freshEb' rather than the slot-1 one, since only an endorser
-- block clear of 'nutcMinOfferLead' is offered onward at all.
test_offerFollowsAnnouncementPerPeer :: Assertion
test_offerFollowsAnnouncementPerPeer = do
  let simTrace = runSimTrace scenario
  case traceResult False simTrace of
    Right unannounced ->
      assertEqual "offers the node made without announcing first" [] unannounced
    outcome ->
      assertFailure $
        unlines $
          ("expected the run to finish, but: " <> show outcome)
            : lastN 40 (selectTraceEventsSay' simTrace)
 where
  scenario :: forall s. IOSim s [LeiosPoint]
  scenario = do
    nodeDBs <- emptyNodeDBs
    leiosDb <- LeiosDb.newLeiosDBInMemory
    holder <- newPeerEnv
    listener <- newPeerEnv

    (chainDBTracer, getTraces) <- recordingTracerTVar

    withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
      withRegistry $ \registry -> do
        -- The holder alone is connected while the node learns of, and relays,
        -- the announcement. It withholds the endorser block itself.
        connectPeer nut registry (PeerAddr 0) holder
        serveChain holder $ chainOf freshChain
        awaitPollingWith getTraces "the announcement has been processed" $
          ebOfferedBy nut (PeerAddr 0) freshPoint

        -- Only now does the listener join, so the announcement is behind it.
        connectPeer nut registry (PeerAddr 1) listener
        threadDelay 5

        -- And only now can the node acquire the body, which is what makes it
        -- /consider/ offering the endorser block onward.
        plantEb holder freshEb freshClosure
        awaitWith getTraces "the certified chain is selected" $
          tipIsSTM nut (last freshChain)
        threadDelay 5

        heardUnannouncedOffers listener

-- | Whether the peer survives what it said over LeiosNotify.
data OfferOutcome = PeerDropped | PeerKept
  deriving Show

-- | Run a peer that says only what the given action queues, and check whether
-- that costs it the connection.
--
-- The peer serves no chain at all, so everything the node knows of it came
-- over LeiosNotify.
test_offerSequence ::
  OfferOutcome -> (forall s. PeerEnv (IOSim s) -> IOSim s ()) -> Assertion
test_offerSequence expected saysIt = do
  let simTrace = runSimTrace scenario
  case (expected, traceResult False simTrace) of
    (PeerDropped, Left (FailureException e)) | isInvalidOffer e -> pure ()
    (PeerKept, Right ()) -> pure ()
    (_, outcome) ->
      assertFailure $
        unlines $
          ("expected " <> show expected <> ", but: " <> show outcome)
            : lastN 40 (selectTraceEventsSay' simTrace)
 where
  isInvalidOffer :: SomeException -> Bool
  isInvalidOffer e
    | Just (ExceptionInLinkedThread _ inner) <- fromException e =
        isInvalidOffer inner
    | Just (_ :: Leios.ExnLeiosInvalidOffer) <- fromException e = True
    | otherwise = False

  scenario :: forall s. IOSim s ()
  scenario = do
    nodeDBs <- emptyNodeDBs
    leiosDb <- LeiosDb.newLeiosDBInMemory
    peer <- newPeerEnv

    (chainDBTracer, _getTraces) <- recordingTracerTVar

    withNodeUnderTest nodeConfig nodeDBs leiosDb chainDBTracer $ \nut ->
      withRegistry $ \registry -> do
        connectPeer nut registry (PeerAddr 0) peer
        saysIt peer
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
