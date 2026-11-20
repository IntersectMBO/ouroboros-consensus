-- | 'LeiosValidClaims', the claims that some valid certificate established.
module Test.Consensus.Leios.ValidClaims (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Keys (KeyHash (..))
import qualified Data.ByteString.Short as SBS
import Data.Word (Word8)
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Cert (LeiosClaim (..))
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement (..))
import Ouroboros.Consensus.Leios.Types (RankingBlockHash (..))
import Ouroboros.Consensus.Storage.ChainDB.Impl.Leios
  ( emptyLeiosValidClaims
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
    , testCase "pruning forgets the claims older than the slot" $
        map (`isLeiosValidClaim` pruneLeiosValidClaims (SlotNo 2) claims) [claimA, claimB, claimC]
          @?= [False, False, True]
    , testCase "a claim older than the slot pruned to is not recorded" $
        isLeiosValidClaim
          claimA
          ( insertLeiosValidClaim claimA announcement $
              pruneLeiosValidClaims (SlotNo 2) emptyLeiosValidClaims
          )
          @?= False
    ]
 where
  claims =
    insertLeiosValidClaim claimC announcement $
      insertLeiosValidClaim claimB announcement $
        insertLeiosValidClaim claimA announcement emptyLeiosValidClaims

claimA, claimB, claimC, claimD :: LeiosClaim
claimA = LeiosClaim (SlotNo 0) (rbHash 1)
claimB = LeiosClaim (SlotNo 1) (rbHash 2)
claimC = LeiosClaim (SlotNo 2) (rbHash 3)
claimD = LeiosClaim (SlotNo 3) (rbHash 4)

rbHash :: Word8 -> RankingBlockHash
rbHash = MkRankingBlockHash . UnsafeHash . SBS.replicate 32

-- | What each of 'claims' announces; these tests do not depend on it.
announcement :: LeiosAnnouncement
announcement =
  MkLeiosAnnouncement
    { announcementElectee = KeyHash (UnsafeHash (SBS.replicate 28 0))
    , announcementEbHash = UnsafeHash (SBS.replicate 32 0)
    , announcementEbSize = 0
    }
