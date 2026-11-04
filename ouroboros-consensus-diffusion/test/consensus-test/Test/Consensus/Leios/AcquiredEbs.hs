-- | 'AcquiredLeiosEbs', the endorser blocks whose closure the LeiosDB holds.
module Test.Consensus.Leios.AcquiredEbs (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import qualified Data.ByteString.Short as SBS
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types (EndorserBlockHash, LeiosPoint (..))
import Ouroboros.Consensus.Storage.ChainDB.Impl.Leios
  ( insertAcquiredLeiosEb
  , isAcquiredLeiosEb
  , mkAcquiredLeiosEbs
  , pruneAcquiredLeiosEbs
  )
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Leios acquired endorser blocks"
    [ testCase "pruning forgets the points older than the slot" $
        map (`isAcquiredLeiosEb` pruned) [pointA, pointB, laterPointA] @?= [False, True, True]
    , testCase "a point older than the slot pruned to is not recorded afterwards" $
        isAcquiredLeiosEb pointA (insertAcquiredLeiosEb pointA pruned)
          @?= False
    ]
 where
  pointA = MkLeiosPoint (SlotNo 1) ebA
  pointB = MkLeiosPoint (SlotNo 2) ebB
  laterPointA = MkLeiosPoint (SlotNo 3) ebA
  pruned =
    pruneAcquiredLeiosEbs (SlotNo 2) $
      mkAcquiredLeiosEbs [pointA, pointB, laterPointA]

ebA, ebB :: EndorserBlockHash
ebA = UnsafeHash (SBS.replicate 32 1)
ebB = UnsafeHash (SBS.replicate 32 2)
