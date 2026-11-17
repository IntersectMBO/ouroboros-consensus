-- | 'LeiosValidClaims', the claims that some valid certificate established.
module Test.Consensus.Leios.ValidClaims (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import qualified Data.ByteString.Short as SBS
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Word (Word8)
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Cert (LeiosClaim (..))
import Ouroboros.Consensus.Leios.Types (RankingBlockHash (..))
import Ouroboros.Consensus.Storage.ChainDB.Impl.Leios
  ( LeiosValidClaims
  , emptyLeiosValidClaims
  , insertLeiosValidClaim
  , isLeiosValidClaim
  , pruneLeiosValidClaims
  )
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Leios valid claims"
    [ testCase "a recorded claim is valid, and others are not" $
        map (`isLeiosValidClaim` claims) [claimA, claimB, claimC, claimD]
          @?= [True, True, True, False]
    , testCase "new claims are inserted once and repeat insertion changes nothing" $ do
        let first = insertLeiosValidClaim claimA emptyLeiosValidClaims
        assertBool "first insertion" (isJust first)
        case first of
          Nothing -> assertFailure "first claim was rejected"
          Just established -> do
            assertBool "claim was not retained" (isLeiosValidClaim claimA established)
            assertBool "repeat was inserted" (isNothing (insertLeiosValidClaim claimA established))
    , testCase "membership includes both the predecessor slot and hash" $ do
        let sameHash = claimA{leiosClaimSlot = SlotNo 1}
            first = insert claimA emptyLeiosValidClaims
        assertBool "different slot was already held" (not (isLeiosValidClaim sameHash first))
        let both = insert sameHash first
        map (`isLeiosValidClaim` both) [claimA, sameHash] @?= [True, True]
    , testCase "pruning forgets the claims older than the slot" $
        map (`isLeiosValidClaim` pruneLeiosValidClaims (SlotNo 2) claims) [claimA, claimB, claimC]
          @?= [False, False, True]
    , testCase "a claim older than the slot pruned to is not recorded" $
        assertBool "recorded" $
          isNothing $
            insertLeiosValidClaim claimA (pruneLeiosValidClaims (SlotNo 2) emptyLeiosValidClaims)
    , testCase "an established claim is not recorded again" $
        assertBool "recorded again" $
          isNothing $
            insertLeiosValidClaim claimB claims
    , testCase "a claim exactly at the pruning fence is accepted" $ do
        let pruned = pruneLeiosValidClaims (SlotNo 2) emptyLeiosValidClaims
        assertBool "below-fence insertion succeeded" (isNothing (insertLeiosValidClaim claimB pruned))
        case insertLeiosValidClaim claimC pruned of
          Nothing -> assertFailure "fence equality was rejected"
          Just accepted -> do
            assertBool "equal-fence claim is missing" (isLeiosValidClaim claimC accepted)
            assertBool "rejected claim appeared" (not (isLeiosValidClaim claimB accepted))
    , testCase "an earlier prune cannot reopen a newer pruning fence" $ do
        let high = pruneLeiosValidClaims (SlotNo 5) claims
            earlier = pruneLeiosValidClaims (SlotNo 3) high
            below = claimD{leiosClaimSlot = SlotNo 4}
            equal = claimD{leiosClaimSlot = SlotNo 5}
        assertBool "backwards prune admitted an old claim" (isNothing (insertLeiosValidClaim below earlier))
        case insertLeiosValidClaim equal earlier of
          Nothing -> assertFailure "newer fence equality was rejected"
          Just accepted -> do
            assertBool "equal-fence claim is missing" (isLeiosValidClaim equal accepted)
            assertBool "old claim was restored" (not (isLeiosValidClaim below accepted))
    ]
 where
  claims = insert claimC $ insert claimB $ insert claimA emptyLeiosValidClaims

insert :: LeiosClaim -> LeiosValidClaims -> LeiosValidClaims
insert claim claims = fromMaybe claims (insertLeiosValidClaim claim claims)

claimA, claimB, claimC, claimD :: LeiosClaim
claimA = LeiosClaim (SlotNo 0) (rbHash 1)
claimB = LeiosClaim (SlotNo 1) (rbHash 2)
claimC = LeiosClaim (SlotNo 2) (rbHash 3)
claimD = LeiosClaim (SlotNo 3) (rbHash 4)

rbHash :: Word8 -> RankingBlockHash
rbHash = MkRankingBlockHash . UnsafeHash . SBS.replicate 32
