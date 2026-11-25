{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | What one announcement can carry at most, read from a ledger view: the
-- block body, plus an endorser block and its closure where the protocol has
-- Leios, each at its maximum. Diffusion sizes its egress credit buckets by it.
module Test.Consensus.Cardano.FreshMax (tests) where

import Cardano.Ledger.BaseTypes
  ( Milliseconds32 (..)
  , ProtVer (..)
  , StrictMaybe (..)
  , boundRational
  )
import Cardano.Ledger.Coin (knownNonZeroCoin)
import Cardano.Ledger.State (PoolDistr (..), emptyLeiosCommittee)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Slotting.Slot (SlotInterval (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Ouroboros.Consensus.Leios.FreshBytes (freshMaxBytesFromView, maxBlockBodyBytes)
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Peras.Protocol (PerasLedgerView (..))
import Ouroboros.Consensus.Protocol.Praos (LeiosOnly (..), PerasOnly (..), Praos)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.Praos2 (LeiosOnly (..), PerasOnly (..), Praos2)
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "FreshMax"
    [ testProperty "with Leios: block body, endorser block and closure at their maxima" prop_leios
    , testProperty "without Leios: the block body alone" prop_praos
    ]

-- | The block body maximum a view carries, and the two endorser-block maxima,
-- in any combination.
data Maxima = Maxima Word32 Word32 Word32
  deriving Show

instance Arbitrary Maxima where
  arbitrary = Maxima <$> size <*> size <*> size
   where
    size = frequency [(1, pure 0), (3, choose (1, 100_000)), (2, choose (100_000, 20_000_000))]
  shrink (Maxima b e t) =
    [Maxima b' e t | b' <- shrink b]
      ++ [Maxima b e' t | e' <- shrink e]
      ++ [Maxima b e t' | t' <- shrink t]

-- | The block body term is 'maxBlockBodyBytes' whatever the view carries: the
-- protocol-generic view does not give its own body maximum.
prop_leios :: Maxima -> Property
prop_leios (Maxima body ebBody ebTxs) =
  toInteger (freshMaxBytesFromView (Proxy @(Praos2 StandardCrypto)) view)
    === toInteger maxBlockBodyBytes + toInteger ebBody + toInteger ebTxs
 where
  view =
    Views.PraosLedgerView
      { Views.plvPoolDistr = PoolDistr Map.empty (knownNonZeroCoin @1)
      , Views.plvMaxHeaderSize = 0
      , Views.plvMaxBodySize = body
      , Views.plvProtocolVersion = ProtVer minBound 0
      , Views.plvLeios =
          Praos2HasLeios
            MkLeiosLedgerView
              { llvCommittee = emptyLeiosCommittee
              , llvQuorumStakeThreshold = maxBound
              , llvAnnouncementPeriod = Milliseconds32 1000
              , llvVotePeriod = Milliseconds32 4000
              , llvDiffusionPeriod = Milliseconds32 7000
              , llvMaxEbBodySize = ebBody
              , llvMaxEbTxsSize = ebTxs
              }
      , Views.plvPeras = Praos2HasPeras perasLedgerView
      }

-- | A protocol without Leios has nothing in the endorser-block fields, so the
-- maxima generated for them cannot reach the answer.
prop_praos :: Maxima -> Property
prop_praos (Maxima body _ _) =
  toInteger (freshMaxBytesFromView (Proxy @(Praos StandardCrypto)) view)
    === toInteger maxBlockBodyBytes
 where
  view =
    Views.PraosLedgerView
      { Views.plvPoolDistr = PoolDistr Map.empty (knownNonZeroCoin @1)
      , Views.plvMaxHeaderSize = 0
      , Views.plvMaxBodySize = body
      , Views.plvProtocolVersion = ProtVer minBound 0
      , Views.plvLeios = PraosLacksLeios ()
      , Views.plvPeras = PraosLacksPeras ()
      }

perasLedgerView :: PerasLedgerView
perasLedgerView =
  MkPerasLedgerView
    { plvMinCandidateBlockAge = SlotInterval 90
    , plvHealingFactor = fromMaybe (error "impossible") (boundRational 1)
    , plvCertBoost = 15
    , plvTargetCommitteeSize = 900
    , plvBootstrapRound = SNothing
    , plvQuorumThresholdSafetyMargin = fromMaybe (error "impossible") (boundRational 0.02)
    }
