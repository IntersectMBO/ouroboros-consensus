{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.NodeState (
    NodeState (..)
  , genesisNodeState
  , nsTipTip
  ) where

import           Ouroboros.Consensus.Block.Abstract (WithOrigin (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..),
                     CondenseList (..), PaddingDirection (..), padListWith)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (..), tipFromHeader)
import           Ouroboros.Network.Point (withOrigin)
import           Test.Util.TersePrinting (terseBlock, terseWithOrigin)
import           Test.Util.TestBlock (TestBlock)

-- | The state of a peer at a given point in time.
data NodeState blk =
  NodeState {
    nsTip    :: WithOrigin blk,
    nsHeader :: WithOrigin blk,
    nsBlock  :: WithOrigin blk
  }
  deriving (Eq, Show)

nsTipTip :: AF.HasHeader blk => NodeState blk -> Tip blk
nsTipTip = withOrigin TipGenesis tipFromHeader . nsTip

instance Condense (NodeState TestBlock) where
  condense NodeState {nsTip, nsHeader, nsBlock} =
    "TP " ++ terseWithOrigin terseBlock nsTip ++
    " | HP " ++ terseWithOrigin terseBlock nsHeader ++
    " | BP " ++ terseWithOrigin terseBlock nsBlock

instance CondenseList (NodeState TestBlock) where
  condenseList points =
    zipWith3
      (\tip header block ->
        "TP " ++ tip ++
          " | HP " ++ header ++
          " | BP " ++ block
      )
      (padListWith PadRight $ map (terseWithOrigin terseBlock . nsTip) points)
      (padListWith PadRight $ map (terseWithOrigin terseBlock . nsHeader) points)
      (padListWith PadRight $ map (terseWithOrigin terseBlock . nsBlock) points)

genesisNodeState :: NodeState blk
genesisNodeState =
  NodeState {
    nsTip = Origin,
    nsHeader = Origin,
    nsBlock = Origin
  }


