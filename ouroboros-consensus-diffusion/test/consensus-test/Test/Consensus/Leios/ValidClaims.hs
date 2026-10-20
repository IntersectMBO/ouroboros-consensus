-- | 'LeiosValidClaims', the claims that some valid certificate established.
module Test.Consensus.Leios.ValidClaims (tests) where

import qualified Data.ByteString as BS
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Cert (LeiosClaim (..))
import Ouroboros.Consensus.Leios.Types (RbHash (..))
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
        map (`isLeiosValidClaim` claims) [rbA, rbB, rbC, rbD]
          @?= [True, True, True, False]
    , testCase "pruning forgets the claims older than the slot" $
        map (`isLeiosValidClaim` pruneLeiosValidClaims (SlotNo 2) claims) [rbA, rbB, rbC]
          @?= [False, False, True]
    ]
 where
  claims =
    insertLeiosValidClaim (LeiosClaim (SlotNo 2) rbC) $
      insertLeiosValidClaim (LeiosClaim (SlotNo 1) rbB) $
        insertLeiosValidClaim (LeiosClaim (SlotNo 0) rbA) emptyLeiosValidClaims

rbA, rbB, rbC, rbD :: RbHash
rbA = MkRbHash (BS.replicate 32 1)
rbB = MkRbHash (BS.replicate 32 2)
rbC = MkRbHash (BS.replicate 32 3)
rbD = MkRbHash (BS.replicate 32 4)
