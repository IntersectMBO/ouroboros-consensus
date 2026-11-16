-- | 'LeiosValidClaims', the claims that some valid certificate established.
module Test.Consensus.Leios.ValidClaims (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import qualified Data.ByteString.Short as SBS
import Data.Maybe (fromMaybe, isNothing)
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
