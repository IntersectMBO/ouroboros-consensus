{-# LANGUAGE BangPatterns #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.ChainDb (computePastLedger, computeHeaderStateHistory) where

import Ouroboros.Consensus.Config (SecurityParam (SecurityParam), TopLevelConfig, configSecurityParam)
import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
import Ouroboros.Consensus.HeaderStateHistory (HeaderStateHistory)
import Ouroboros.Consensus.Ledger.Abstract (tickThenReapply)
import Ouroboros.Consensus.Ledger.Basics (getTip)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (ExtLedgerCfg), ExtLedgerState)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.Block (Point, castPoint)
import qualified Ouroboros.Network.Mock.Chain as Chain
import Ouroboros.Network.Mock.Chain (Chain, blockPoint)
import Test.Util.TestBlock (TestBlock, testInitExtLedger)

-- | Simulates 'ChainDB.getPastLedger'.
computePastLedger ::
     TopLevelConfig TestBlock
  -> Point TestBlock
  -> Chain TestBlock
  -> Maybe (ExtLedgerState TestBlock)
computePastLedger cfg pt chain
    | pt `elem` validPoints
    = Just $ go testInitExtLedger (Chain.toOldestFirst chain)
    | otherwise
    = Nothing
  where
    SecurityParam k = configSecurityParam cfg

    curFrag :: AnchoredFragment TestBlock
    curFrag =
          AF.anchorNewest k
        . Chain.toAnchoredFragment
        $ chain

    validPoints :: [Point TestBlock]
    validPoints =
        AF.anchorPoint curFrag : map blockPoint (AF.toOldestFirst curFrag)

    -- | Apply blocks to the ledger state until we have applied the block
    -- matching @pt@, after which we return the resulting ledger.
    --
    -- PRECONDITION: @pt@ is in the list of blocks or genesis.
    go :: ExtLedgerState TestBlock -> [TestBlock] -> ExtLedgerState TestBlock
    go !st blks
        | castPoint (getTip st) == pt
        = st
        | blk:blks' <- blks
        = go (tickThenReapply (ExtLedgerCfg cfg) blk st) blks'
        | otherwise
        = error "point not in the list of blocks"

-- | Simulates 'ChainDB.getHeaderStateHistory'.
computeHeaderStateHistory ::
     TopLevelConfig TestBlock
  -> Chain TestBlock
  -> HeaderStateHistory TestBlock
computeHeaderStateHistory cfg =
      HeaderStateHistory.trim (fromIntegral k)
    . HeaderStateHistory.fromChain cfg testInitExtLedger
  where
    SecurityParam k = configSecurityParam cfg
