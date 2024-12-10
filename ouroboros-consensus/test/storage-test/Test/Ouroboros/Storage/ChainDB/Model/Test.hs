{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns #-}

-- | Tests of properties of the chain DB model
--
-- The model for the chain DB (@Test.Ouroboros.Storage.ChainDB.Model@) contains
-- a quite a bit of info, but that is primarily because it needs to support
-- stateful APIs such as followers (that follow the tip of the chain) and
-- iterators (which stream a chunk of the chain). The main part of the model is
-- it's model of the volatile DB and the immutable DB, which is again satisfyingly
-- simple: the volatile DB is modelled simply as a set of blocks, and the
-- immutable DB is modelled simply as a list of blocks (i.e., a chain).
--
-- Nonetheless, the implementation of the operations on that model is subtle.
-- In particular, the chain DB is responsible for chain selection, and so the
-- model must too. So we have a few properties checking some aspects of the model;
-- in particular, we verify that no matter in which order we add blocks to the
-- chain DB, we always pick the most preferred chain.
--
module Test.Ouroboros.Storage.ChainDB.Model.Test (tests) where

import           GHC.Stack
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Storage.ChainDB.API (LoE (..),
                     StreamFrom (..), StreamTo (..))
import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.Mock.Chain as Chain
import qualified Test.Ouroboros.Storage.ChainDB.Model as M
import           Test.Ouroboros.Storage.ChainDB.Model (ModelSupportsBlock)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.TestBlock

tests :: TestTree
tests = testGroup "Model" [
      testProperty "getBlock_addBlock"        prop_getBlock_addBlock
    , testProperty "getChain_addChain"        prop_getChain_addChain
    , testProperty "alwaysPickPreferredChain" prop_alwaysPickPreferredChain
    , testProperty "between_currentChain"     prop_between_currentChain
    ]

addBlocks :: LoE () -> [TestBlock] -> M.Model TestBlock
addBlocks loe blks = M.addBlocks cfg blks (M.empty loe (convertMapKind testInitExtLedger))
  where
    cfg = singleNodeTestConfig

prop_getBlock_addBlock :: LoE () -> BlockTree -> Permutation -> Property
prop_getBlock_addBlock loe bt p =
        M.getBlock (blockHash newBlock) (M.addBlock singleNodeTestConfig newBlock model)
    === if NotOrigin (blockNo newBlock) > M.immutableBlockNo secParam model
        then Just newBlock
        else Nothing
  where
    (newBlock:initBlocks) = permute p $ treeToBlocks bt
    model = addBlocks loe initBlocks
    secParam = configSecurityParam singleNodeTestConfig

-- | Test that, for any chain @bc@, adding its blocks to an empty model causes
-- the selection to be @bc@. This is only true with LoE disabled.
prop_getChain_addChain :: BlockChain -> Property
prop_getChain_addChain bc =
    counterexample ("model: " ++ show model) $
    blockChain bc === M.currentChain model
  where
    blocks = chainToBlocks bc
    model  = addBlocks LoEDisabled blocks

-- | Test that, no matter in which order we add blocks to the chain DB, we
-- always pick the most preferred chain. This is only true with LoE disabled.
prop_alwaysPickPreferredChain :: BlockTree -> Permutation -> Property
prop_alwaysPickPreferredChain bt p =
    counterexample ("blocks: " ++ show blocks) $
    counterexample ("invalid: " ++ show (M.invalid model)) $
    conjoin [
        not $ preferCandidate' candidate
      | candidate <- treeToChains bt
      ]
  where
    blocks  = permute p $ treeToBlocks bt
    model   = addBlocks LoEDisabled blocks
    current = M.currentChain model

    curFragment = Chain.toAnchoredFragment (getHeader <$> current)

    SecurityParam k = configSecurityParam singleNodeTestConfig

    bcfg = configBlock singleNodeTestConfig

    preferCandidate' candidate =
        AF.preferAnchoredCandidate bcfg curFragment candFragment &&
        AF.forksAtMostKBlocks k curFragment candFragment
      where
        candFragment = Chain.toAnchoredFragment (getHeader <$> candidate)

-- TODO add properties about forks too
prop_between_currentChain :: LoE () -> BlockTree -> Property
prop_between_currentChain loe bt =
    Right (AF.toOldestFirst $ Chain.toAnchoredFragment $ M.currentChain model) ===
    M.between secParam from to model
  where
    blocks   = treeToBlocks bt
    model    = addBlocks loe blocks
    from     = StreamFromExclusive GenesisPoint
    to       = StreamToInclusive $ cantBeGenesis (M.tipPoint model)
    secParam = configSecurityParam singleNodeTestConfig

-- | Workaround when we know the DB can't be empty, but the types don't
cantBeGenesis :: HasCallStack => Point blk -> RealPoint blk
cantBeGenesis GenesisPoint     = error "cantBeGenesis: what did I tell you!?"
cantBeGenesis (BlockPoint s h) = RealPoint s h

{-------------------------------------------------------------------------------
  Orphan instances
-------------------------------------------------------------------------------}

instance ModelSupportsBlock TestBlock
