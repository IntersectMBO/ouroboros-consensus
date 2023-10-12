{-# LANGUAGE LambdaCase #-}

module Test.Consensus.PeerSimulator.Tests.Timeouts (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import           Ouroboros.Consensus.Block (getHeader)
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (tipFromHeader)
import           Ouroboros.Network.Driver.Limits
                     (ProtocolLimitFailure (ExceededTimeLimit))
import           Test.Consensus.BlockTree (btTrunk)
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()

tests :: TestTree
tests = testProperty "timeouts" prop_timeouts

prop_timeouts :: QC.Gen QC.Property
prop_timeouts = do
  genesisTest <- genChains 0
  let schedule = dullSchedule $ btTrunk $ gtBlockTree genesisTest

  pure $ withMaxSuccess 10 $ runSimOrThrow $
    runTest' genesisTest schedule $ \case
        -- FIXME: ExceededTimeLimit
        Left ( _ :| []) -> counterexample "foo" True
        Left exn -> counterexample ("exception: " ++ show exn) False
        Right result -> counterexample ("result: " ++ condense result) False

  where
    dullSchedule :: TestFrag -> PointSchedule
    dullSchedule (AF.Empty _) = error "requires a non-empty block tree"
    dullSchedule (_ AF.:> tipBlock) =
      let tipPoint = TipPoint $ tipFromHeader tipBlock
          headerPoint = HeaderPoint $ getHeader tipBlock
          blockPoint = BlockPoint tipBlock
          state = Peer HonestPeer $ NodeOnline $ AdvertisedPoints tipPoint headerPoint blockPoint
          tick = Tick { active = state, peers = Peers state Map.empty }
      in
      PointSchedule (tick :| replicate 1000000 tick)
