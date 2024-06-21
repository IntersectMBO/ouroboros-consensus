{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.ThreadNet.Util.NodeJoinPlan (
    -- * Node Join Plan
    NodeJoinPlan (..)
  , coreNodeIdJoinSlot
  , genNodeJoinPlan
  , nodeIdJoinSlot
  , trivialNodeJoinPlan
  ) where

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Test.QuickCheck
import           Test.Util.Slots (NumSlots (..))

{-------------------------------------------------------------------------------
  Node Join Plans
-------------------------------------------------------------------------------}

-- | In which slot each node joins the network
--
newtype NodeJoinPlan = NodeJoinPlan (Map CoreNodeId SlotNo)
  deriving (Show)

instance Condense NodeJoinPlan where
  condense (NodeJoinPlan m) = condense
      [ (fromCoreNodeId nid, slot) | (nid, slot) <- Map.toAscList m ]

-- | All nodes join immediately
--
trivialNodeJoinPlan :: NumCoreNodes -> NodeJoinPlan
trivialNodeJoinPlan numCoreNodes =
    NodeJoinPlan $
    Map.fromList $
    [ (nid, SlotNo 0) | nid <- enumCoreNodes numCoreNodes ]

-- | Generate a 'NodeJoinPlan' consistent with the given properties
--
-- INVARIANT: Nodes with higher Ids will not join before nodes with lower Ids.
-- This eliminates some uninteresting symmetry and makes the counter-examples
-- easier for humans to interpret.
--
genNodeJoinPlan ::
     NumCoreNodes
     -- ^ PRECONDITION: non-negative
  -> NumSlots
     -- ^ PRECONDITION: positive
  -> Gen NodeJoinPlan
genNodeJoinPlan numCoreNodes@(NumCoreNodes n) numSlots@(NumSlots t)
  | n < 0 || t < 1 = error $ "Cannot generate TestConfig: "
    ++ show (numCoreNodes, numSlots)

  | otherwise = do
    let genJoinSlot = do
            let lastSlot = t - 1
            SlotNo <$> choose (0, lastSlot)

    let nids = enumCoreNodes numCoreNodes :: [CoreNodeId]
    schedules <- vectorOf (fromIntegral n) genJoinSlot
    -- without loss of generality, the nodes start initializing in order of
    -- their Ids; this merely makes it easer to interpret the counterexamples
    pure $ NodeJoinPlan $ Map.fromList $ zip nids $ List.sort schedules

-- | Partial; @error@ for a node not in the plan
--
coreNodeIdJoinSlot ::
     HasCallStack
  => NodeJoinPlan -> CoreNodeId -> SlotNo
coreNodeIdJoinSlot (NodeJoinPlan m) nid =
    Map.findWithDefault
        (error $ "not found: " <> condense (nid, Map.toList m))
        nid m

-- | Partial; @error@ for a node not in the plan
--
nodeIdJoinSlot ::
     HasCallStack
  => NodeJoinPlan -> NodeId -> SlotNo
nodeIdJoinSlot nodeJoinPlan@(NodeJoinPlan m) ni = case ni of
    CoreId cni -> coreNodeIdJoinSlot nodeJoinPlan cni
    _          -> error $ "not found: " <> condense (ni, Map.toList m)
