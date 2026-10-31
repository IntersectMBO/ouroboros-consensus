{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | What one announcement can carry at most, read from a ledger view: the
-- block body, plus an endorser block and its closure where the protocol has
-- Leios, each at its maximum. Diffusion sizes its egress credit buckets by it.
module Test.Consensus.Cardano.FreshMax (tests) where

import Cardano.Ledger.BaseTypes (Milliseconds32 (..), ProtVer (..))
import Cardano.Ledger.Coin (knownNonZeroCoin)
import Cardano.Ledger.State (PoolDistr (..), emptyLeiosCommittee)
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Ouroboros.Consensus.Protocol.Praos (LeiosOnly (PraosLacksLeios), Praos)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.Praos2 (LeiosOnly (Praos2HasLeios), Praos2)
import Ouroboros.Consensus.Shelley.Eras (ConwayEra, DijkstraEra)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Leios ()
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveLeiosBlock (..))
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "FreshMax"
    [ testProperty "with Leios: block body, endorser block and closure at their maxima" prop_leios
    , testProperty "without Leios: the block body alone, whatever the view says otherwise" prop_praos
    ]

-- | The three maxima a view carries, in any combination.
data Maxima = Maxima Word32 Word32 Word32
  deriving Show

instance Arbitrary Maxima where
  arbitrary = Maxima <$> size <*> size <*> size
   where
    size = frequency [(1, pure 0), (3, choose (1, 100_000)), (2, choose (100_000, 20_000_000))]
  shrink (Maxima b e t) =
    [Maxima b' e t | b' <- shrink b] ++ [Maxima b e' t | e' <- shrink e] ++ [Maxima b e t' | t' <- shrink t]

prop_leios :: Maxima -> Property
prop_leios (Maxima body ebBody ebTxs) =
  toInteger (getFreshMaxBytesFromView (Proxy @(ShelleyBlock (Praos2 StandardCrypto) DijkstraEra)) view)
    === toInteger body + toInteger ebBody + toInteger ebTxs
 where
  view =
    Views.PraosLedgerView
      { Views.plvPoolDistr = PoolDistr Map.empty (knownNonZeroCoin @1)
      , Views.plvMaxHeaderSize = 0
      , Views.plvMaxBodySize = body
      , Views.plvProtocolVersion = ProtVer minBound 0
      , Views.plvCommittee = Praos2HasLeios emptyLeiosCommittee
      , Views.plvQuorumStakeThreshold = Praos2HasLeios maxBound
      , Views.plvAnnouncementPeriodLength = Praos2HasLeios (Milliseconds32 1000)
      , Views.plvVotePeriodLength = Praos2HasLeios (Milliseconds32 4000)
      , Views.plvDiffusionPeriodLength = Praos2HasLeios (Milliseconds32 7000)
      , Views.plvMaxEbBodySize = Praos2HasLeios ebBody
      , Views.plvMaxEbTxsSize = Praos2HasLeios ebTxs
      }

-- | A protocol without Leios has nothing in the endorser-block fields, so the
-- maxima generated for them cannot reach the answer.
prop_praos :: Maxima -> Property
prop_praos (Maxima body _ _) =
  toInteger (getFreshMaxBytesFromView (Proxy @(ShelleyBlock (Praos StandardCrypto) ConwayEra)) view)
    === toInteger body
 where
  view =
    Views.PraosLedgerView
      { Views.plvPoolDistr = PoolDistr Map.empty (knownNonZeroCoin @1)
      , Views.plvMaxHeaderSize = 0
      , Views.plvMaxBodySize = body
      , Views.plvProtocolVersion = ProtVer minBound 0
      , Views.plvCommittee = PraosLacksLeios ()
      , Views.plvQuorumStakeThreshold = PraosLacksLeios ()
      , Views.plvAnnouncementPeriodLength = PraosLacksLeios ()
      , Views.plvVotePeriodLength = PraosLacksLeios ()
      , Views.plvDiffusionPeriodLength = PraosLacksLeios ()
      , Views.plvMaxEbBodySize = PraosLacksLeios ()
      , Views.plvMaxEbTxsSize = PraosLacksLeios ()
      }
