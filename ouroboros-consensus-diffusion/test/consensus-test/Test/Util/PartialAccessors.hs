{-# LANGUAGE NamedFieldPuns #-}

-- | Helpers to access particular parts of trees and schedules
-- Those functions are partial, and are designed to only be used in tests.
-- We know they won't fail there, because we generated the structures
-- with the correct properties.
module Test.Util.PartialAccessors (
    getHonestPeer
  , getOnlyBranch
  , getOnlyBranchTip
  , getTrunkTip
  ) where

import qualified Data.Map as Map
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader)
import           Test.Consensus.BlockTree

getOnlyBranch :: BlockTree blk -> BlockTreeBranch blk
getOnlyBranch BlockTree {btBranches} = case btBranches of
  [branch] -> branch
  _        -> error "tree must have exactly one alternate branch"

getTrunkTip :: HasHeader blk => BlockTree blk -> blk
getTrunkTip tree = case btTrunk tree of
  (AF.Empty _)       -> error "tree must have at least one block"
  (_ AF.:> tipBlock) -> tipBlock

getOnlyBranchTip :: HasHeader blk => BlockTree blk -> blk
getOnlyBranchTip BlockTree {btBranches} = case btBranches of
  [branch] -> case btbFull branch of
    (AF.Empty _)       -> error "alternate branch must have at least one block"
    (_ AF.:> tipBlock) -> tipBlock
  _ -> error "tree must have exactly one alternate branch"

getHonestPeer :: Map.Map Int a -> a
getHonestPeer honests =
  if Map.size honests /= 1
    then error "there must be exactly one honest peer"
    else case Map.lookup 1 honests of
      Nothing -> error "the only honest peer must have id 1"
      Just p  -> p
