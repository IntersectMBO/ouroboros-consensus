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
    [ testCase "pruning forgets the endorser blocks all of whose foci are older than the slot" $
        map (`isAcquiredLeiosEb` pruned) [ebA, ebB] @?= [False, True]
    , testCase "a focus older than the slot pruned to is not recorded afterwards" $
        isAcquiredLeiosEb ebA (insertAcquiredLeiosEb (MkLeiosPoint (SlotNo 1) ebA) pruned)
          @?= False
    ]
 where
  pruned =
    pruneAcquiredLeiosEbs (SlotNo 2) $
      mkAcquiredLeiosEbs [MkLeiosPoint (SlotNo 1) ebA, MkLeiosPoint (SlotNo 2) ebB]

ebA, ebB :: EndorserBlockHash
ebA = UnsafeHash (SBS.replicate 32 1)
ebB = UnsafeHash (SBS.replicate 32 2)
