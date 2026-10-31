{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | ValidClaims as ChainSel drives it, against a real ChainDB over a mocked
-- filesystem.
--
-- The blocks are 'LeiosTestBlock's, so the certificates are real but the chains
-- are whatever the test says they are.
--
-- Each CertRB is judged against the ledger view the test passes to
-- 'ChainDB.addBlock'. In a node that value is not supplied but derived:
-- BlockFetch reads it off the matched header ('matchedBlockPredecessor').
-- These tests take it as given, so they cover what ChainSel does with the
-- view, not whether the view ChainSel is handed is the right one.
module Test.LeiosValidClaimsChainDB (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (isJust)
import LeiosDemoTypes
  ( LeiosPoint (..)
  , RbHash
  )
import LeiosValidClaims (memberValidClaim, sizeValidClaims)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (SecurityParam (..))
import Ouroboros.Consensus.Storage.ChainDB.API (Predecessor (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.LeiosDB as LeiosDb
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.BlockFetch.ConsensusInterface (WithFingerprint (..))
import Test.LeiosTestNode
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ChainDB
import Test.Util.LeiosHash (unsafeEbHashFromBytes)
import Test.Util.LeiosTestBlock
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "LeiosValidClaims (ChainDB)"
    [ testCase "one announcer, one verification" test_establishOncePerAnnouncer
    , testCase "an invalid certificate establishes nothing" test_invalidCert
    , testCase "a view with no committee establishes nothing" test_noCommittee
    , testCase "an established claim admits a bad certificate" test_knownClaimShortCircuits
    , testCase "an off-selection CertRB is forgotten and re-acquired" test_forgetAndReacquire
    , testCase "a CertRB with an unacquired EB is parked, then forgotten" test_unacquiredCertRBAtStartUp
    ]

type Blk = LeiosTestBlock

{-------------------------------------------------------------------------------
  The chains these tests use
-------------------------------------------------------------------------------}

-- The announced endorser block is never given to the node: nothing plants its
-- closure in the LeiosDB. So every CertRB below is parked as soon as it is
-- added and none is ever selected, since ChainSel skips a CertRB whose EB has
-- not been acquired.
--
-- The tests remain interesting for two reasons. A claim is verified on the
-- add path, before and regardless of whether the block making it can be
-- selected, so establishing a claim, reusing an established one, and rejecting
-- a bad certificate are all fully exercised. And the startup policy keys off
-- the /selection/, so a parked CertRB is exactly the off-selection CertRB the
-- policy exists for --- parking hands us that state rather than making us
-- contrive it.
--
-- What it does cost: nothing here shows a re-admitted CertRB going on to be
-- selected and used. That needs the closure planted in the LeiosDB.

-- | The announcer: slot 1, no certificate of its own, announces an EB.
announcer :: Blk
announcer =
  announcing (MkLeiosPoint 1 ebHash) 100 (firstLeiosBlock 0)
 where
  ebHash = unsafeEbHashFromBytes $ BS8.take 32 (BS8.pack "eb" <> BS8.replicate 32 '\0')

-- | The claim a CertRB extending 'announcer' makes.
announcerClaim :: RbHash
announcerClaim = claimOf announcer

-- | A CertRB over 'announcer', with a certificate that verifies.
certRB :: Blk
certRB = certifying (certFor announcer) (successorLeiosBlock announcer)

-- | A second CertRB over the same announcer, making the same claim. It carries
-- the same certificate, which is legitimate: the signed message is the
-- announcer's hash, not the certifying block's.
certRBSibling :: Blk
certRBSibling = forkLeiosBlock certRB

-- | A longer chain that shares nothing with 'announcer'. It carries no
-- certificate, so nothing parks it and it is the chain that actually gets
-- selected, which is what leaves 'certRB' off the selection at a restart.
better1, better2, better3 :: Blk
better1 = firstLeiosBlock 1
better2 = successorLeiosBlock better1
better3 = successorLeiosBlock better2

{-------------------------------------------------------------------------------
  The node under test
-------------------------------------------------------------------------------}

-- | Larger than every chain these tests build, so nothing becomes immutable and
-- every block stays in the VolatileDB, where forgetting can reach it.
securityParam :: SecurityParam
securityParam = SecurityParam $ knownNonZeroBounded @8

withFreshNode :: (Node (IOSim s) -> IOSim s a) -> IOSim s a
withFreshNode body = do
  nodeDBs <- emptyNodeDBs
  leiosDb <- LeiosDb.newLeiosDBInMemory
  withNode securityParam nodeDBs leiosDb body

claimCount :: Node (IOSim s) -> IOSim s Int
claimCount Node{nChainDB} =
  sizeValidClaims . forgetFingerprint
    <$> atomically (ChainDB.getLeiosValidClaims nChainDB)

claimEstablished :: Node (IOSim s) -> RbHash -> IOSim s Bool
claimEstablished Node{nChainDB} rbHash =
  memberValidClaim rbHash . forgetFingerprint
    <$> atomically (ChainDB.getLeiosValidClaims nChainDB)

-- | Whether ChainSel has rejected this block.
isInvalid :: Node (IOSim s) -> Blk -> IOSim s Bool
isInvalid Node{nChainDB} blk = do
  isInvalidBlock <- atomically (ChainDB.getIsInvalidBlock nChainDB)
  pure $ isJust $ forgetFingerprint isInvalidBlock (blockHash blk)

isFetched :: Node (IOSim s) -> Blk -> IOSim s Bool
isFetched Node{nChainDB} blk =
  ($ blockPoint blk) <$> atomically (ChainDB.getIsFetched nChainDB)

tipPoint :: Node (IOSim s) -> IOSim s (Point Blk)
tipPoint Node{nChainDB} = atomically $ ChainDB.getTipPoint nChainDB

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | The claim is established once, and the second CertRB making it rides on
-- that verdict rather than being verified again.
test_establishOncePerAnnouncer :: Assertion
test_establishOncePerAnnouncer = do
  let (afterCertRB, established, afterSibling) = runSimOrThrow $
        withFreshNode $ \node -> do
          addAtGenesis node announcer
          addWithCommittee node announcer certRB
          n1 <- claimCount node
          member <- claimEstablished node announcerClaim
          addWithCommittee node announcer certRBSibling
          n2 <- claimCount node
          pure (n1, member, n2)
  assertEqual "the CertRB establishes its claim" 1 afterCertRB
  assertBool "and it is the announcer's claim" established
  assertEqual "the sibling adds no claim" 1 afterSibling

-- | A certificate over another claim establishes nothing, and the block does
-- not get selected.
test_invalidCert :: Assertion
test_invalidCert = do
  let (claims, rejected) = runSimOrThrow $
        withFreshNode $ \node -> do
          addAtGenesis node announcer
          addWithCommittee node announcer poisoned
          (,) <$> claimCount node <*> isInvalid node poisoned
  assertEqual "no claim is established" 0 claims
  assertBool "the CertRB is rejected" rejected
 where
  -- Signed over the wrong message: the certifying block's own hash.
  poisoned = certifying (certFor certRB) (successorLeiosBlock announcer)

-- | A CertRB whose announcing view seats no committee establishes nothing.
test_noCommittee :: Assertion
test_noCommittee = do
  let (claims, tip) = runSimOrThrow $
        withFreshNode $ \node -> do
          addAtGenesis node announcer
          addBlockWith
            node
            (Predecessor (blockSlot announcer) (LeiosTestView Nothing maxBound))
            certRB
          (,) <$> claimCount node <*> tipPoint node
  assertEqual "no claim is established" 0 claims
  assertEqual "the CertRB is not selected" (blockPoint announcer) tip

-- | Once the claim is established, a CertRB making it is admitted without its
-- own certificate being checked. This is the only way the cache is observable
-- from outside: a verification that does happen and one that is skipped are
-- otherwise indistinguishable.
test_knownClaimShortCircuits :: Assertion
test_knownClaimShortCircuits = do
  let (established, rejected) = runSimOrThrow $
        withFreshNode $ \node -> do
          addAtGenesis node announcer
          addWithCommittee node announcer certRB
          addWithCommittee node announcer poisonedSibling
          (,)
            <$> claimEstablished node announcerClaim
            <*> isInvalid node poisonedSibling
  assertBool "the good CertRB establishes the claim" established
  assertBool "the bad one is not rejected" (not rejected)
 where
  -- Signed over the wrong message, as in 'test_invalidCert'.
  poisonedSibling = certifying (certFor certRB) (forkLeiosBlock certRBSibling)

-- | The policy of @valid_claims_startup.md@: a CertRB that is in the
-- VolatileDB but off the selection at startup is treated as a block we do not
-- have, so the ordinary add path can re-establish its claim.
test_forgetAndReacquire :: Assertion
test_forgetAndReacquire = do
  let (beforeRestart, afterRestart, afterThirdOpen) = runSimOrThrow $ do
        nodeDBs <- emptyNodeDBs
        leiosDb <- LeiosDb.newLeiosDBInMemory

        before <- withNode securityParam nodeDBs leiosDb $ \node -> do
          addAtGenesis node announcer
          addWithCommittee node announcer certRB
          claims <- claimCount node
          -- Switch away, so the CertRB is off the selection at shutdown.
          addAtGenesis node better1
          addWithCommittee node better1 better2
          addWithCommittee node better2 better3
          tip <- tipPoint node
          pure (claims, tip)

        restarted <- withNode securityParam nodeDBs leiosDb $ \node -> do
          claimsAtStartUp <- claimCount node
          certRBForgotten <- not <$> isFetched node certRB
          -- The announcer is off the selection too, but carries no
          -- certificate, so it is not forgotten.
          announcerKept <- isFetched node announcer
          -- Re-acquire the CertRB the way BlockFetch would.
          addWithCommittee node announcer certRB
          reestablished <- claimEstablished node announcerClaim
          readmitted <- isFetched node certRB
          pure
            ( claimsAtStartUp
            , certRBForgotten
            , announcerKept
            , reestablished
            , readmitted
            )

        -- A third open sees a VolatileDB that was written to twice for the same
        -- block; it must not have been truncated.
        third <- withNode securityParam nodeDBs leiosDb $ \node ->
          (,) <$> tipPoint node <*> isFetched node announcer

        pure (before, restarted, third)

  let (claimsBefore, tipBefore) = beforeRestart
  assertEqual "the claim is established before the restart" 1 claimsBefore
  assertEqual
    "the better chain is selected"
    (blockPoint better3)
    tipBefore

  let ( claimsAtStartUp
        , certRBForgotten
        , announcerKept
        , reestablished
        , readmitted
        ) = afterRestart
  assertEqual "no claim survives the restart" 0 claimsAtStartUp
  assertBool "the off-selection CertRB is forgotten" certRBForgotten
  assertBool "the off-selection announcer is kept" announcerKept
  assertBool "re-acquiring the CertRB re-establishes the claim" reestablished
  assertBool "and re-admits the block" readmitted

  let (tipThird, announcerThird) = afterThirdOpen
  assertEqual
    "the third open still selects the better chain"
    (blockPoint better3)
    tipThird
  assertBool "and still holds the announcer" announcerThird

-- | A CertRB is selectable only once its EB's closure has been acquired. With
-- an empty LeiosDB it is parked when it arrives and ignored again by initial
-- chain selection, so it is never on the selection and the startup policy
-- forgets it --- which is what lets its claim be re-established later.
--
-- NOT COVERED HERE: the other side of that policy, a CertRB that /is/ on the
-- selection and so must be kept. Reaching that state means planting the
-- closure in the LeiosDB, which this harness cannot do yet.
test_unacquiredCertRBAtStartUp :: Assertion
test_unacquiredCertRBAtStartUp = do
  let (tipBefore, (fetched, tip, claims)) = runSimOrThrow $ do
        nodeDBs <- emptyNodeDBs
        leiosDb <- LeiosDb.newLeiosDBInMemory

        before <- withNode securityParam nodeDBs leiosDb $ \node -> do
          addAtGenesis node announcer
          addWithCommittee node announcer certRB
          tipPoint node

        after' <- withNode securityParam nodeDBs leiosDb $ \node ->
          (,,)
            <$> isFetched node certRB
            <*> tipPoint node
            <*> claimCount node
        pure (before, after')
  assertEqual
    "the CertRB is parked, not selected"
    (blockPoint announcer)
    tipBefore
  assertBool "the CertRB is forgotten" (not fetched)
  assertEqual "the announcer is the selection" (blockPoint announcer) tip
  assertEqual "no claim survives the restart" 0 claims
