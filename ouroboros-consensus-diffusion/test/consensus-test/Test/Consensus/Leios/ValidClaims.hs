-- | 'LeiosValidClaims', the claims that some valid certificate established.
module Test.Consensus.Leios.ValidClaims (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Keys (KeyHash (..))
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Cert (LeiosClaim (..))
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement (..))
import Ouroboros.Consensus.Leios.Types (RankingBlockHash (..))
import Ouroboros.Consensus.Storage.ChainDB.Impl.Leios
  ( emptyLeiosValidClaims
  , insertLeiosValidClaim
  , isLeiosValidClaim
  , leiosCertifiedEbs
  , pruneLeiosValidClaims
  )
import Ouroboros.Consensus.Util.STM (Fingerprint (..), WithFingerprint (..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Leios valid claims"
    [ testCase "a recorded claim is valid, and others are not" $
        map (`isLeiosValidClaim` claims) [rbA, rbB, rbC, rbD]
          @?= [True, True, True, False]
    , testCase "new claims advance the fingerprint; repeats and pruning do not" $ do
        let empty = emptyLeiosValidClaims
            first = insertLeiosValidClaim (LeiosClaim (SlotNo 0) rbA) announcement empty
            repeated = insertLeiosValidClaim (LeiosClaim (SlotNo 0) rbA) announcement first
            pruned = pruneLeiosValidClaims (SlotNo 2) claims
            later = insertLeiosValidClaim (LeiosClaim (SlotNo 3) rbD) announcement pruned
        map (getFingerprint . leiosCertifiedEbs) [empty, first, repeated, claims, pruned, later]
          @?= map Fingerprint [0, 1, 1, 3, 3, 4]
        map (Map.size . forgetFingerprint . leiosCertifiedEbs) [claims, pruned, later]
          @?= [3, 1, 2]
    , testCase "pruning forgets the claims older than the slot" $
        map (`isLeiosValidClaim` pruneLeiosValidClaims (SlotNo 2) claims) [rbA, rbB, rbC]
          @?= [False, False, True]
    ]
 where
  claims =
    insertLeiosValidClaim (LeiosClaim (SlotNo 2) rbC) announcement $
      insertLeiosValidClaim (LeiosClaim (SlotNo 1) rbB) announcement $
        insertLeiosValidClaim (LeiosClaim (SlotNo 0) rbA) announcement emptyLeiosValidClaims

rbA, rbB, rbC, rbD :: RankingBlockHash
rbA = MkRankingBlockHash (UnsafeHash (SBS.replicate 32 1))
rbB = MkRankingBlockHash (UnsafeHash (SBS.replicate 32 2))
rbC = MkRankingBlockHash (UnsafeHash (SBS.replicate 32 3))
rbD = MkRankingBlockHash (UnsafeHash (SBS.replicate 32 4))

-- | What each of 'claims' announces; these tests do not depend on it.
announcement :: LeiosAnnouncement
announcement =
  MkLeiosAnnouncement
    { announcementElectee = KeyHash (UnsafeHash (SBS.replicate 28 0))
    , announcementEbHash = UnsafeHash (SBS.replicate 32 0)
    , announcementEbSize = 0
    }
