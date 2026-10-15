{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Protocol.Praos2 (tests) where

import Cardano.Ledger.BaseTypes (Milliseconds32 (..), ProtVer (..), StrictMaybe (..))
import Cardano.Ledger.Coin (knownNonZeroCoin)
import Cardano.Ledger.State (PoolDistr (..), emptyLeiosCommittee)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..), WithOrigin (..))
import Cardano.Slotting.Time (slotLengthFromSec)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Ouroboros.Consensus.Protocol.PolyPraos
  ( PolyPraosState (..)
  , Ticked (..)
  , tickChainDepStatePolyPraos
  )
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.Praos2 (LeiosOnly (Praos2HasLeios), Praos2)
import Test.Consensus.Protocol.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Praos2"
    [ testProperty
        "crossing into another epoch makes an announcement uncertifiable"
        prop_crossingEpochMakesAnnouncementUncertifiable
    ]

-- | An endorser block may not be certified across an epoch boundary.
prop_crossingEpochMakesAnnouncementUncertifiable ::
  PolyPraosState (Praos2 StandardCrypto) -> Property
prop_crossingEpochMakesAnnouncementUncertifiable st =
  forAll (choose (1, 2 * epochSize)) $ \delta ->
    let slot = SlotNo $ lastSlot + delta
        crosses = epochOf (unSlotNo slot) > epochOf lastSlot
        expected
          | crosses = SNothing
          | otherwise = before
     in classify crosses "crosses an epoch boundary" $
          praosStateLeiosAnnouncement (tickTo slot st) === Praos2HasLeios expected
 where
  Praos2HasLeios before = praosStateLeiosAnnouncement st
  lastSlot = case praosStateLastSlot st of
    Origin -> 0
    At (SlotNo s) -> s

tickTo :: SlotNo -> PolyPraosState (Praos2 StandardCrypto) -> PolyPraosState (Praos2 StandardCrypto)
tickTo slot =
  tickedPraosStateChainDepState
    . tickChainDepStatePolyPraos
      (fixedEpochInfo (EpochSize epochSize) (slotLengthFromSec 1))
      ledgerView
      slot

epochSize :: Word64
epochSize = 5

epochOf :: Word64 -> Word64
epochOf s = s `div` epochSize

-- | The tick only carries it along.
ledgerView :: Views.PolyPraosLedgerView (Praos2 StandardCrypto)
ledgerView =
  Views.PraosLedgerView
    { Views.plvPoolDistr = PoolDistr Map.empty (knownNonZeroCoin @1)
    , Views.plvMaxHeaderSize = 0
    , Views.plvMaxBodySize = 0
    , Views.plvProtocolVersion = ProtVer minBound 0
    , Views.plvCommittee = Praos2HasLeios emptyLeiosCommittee
    , Views.plvQuorumStakeThreshold = Praos2HasLeios maxBound
    , Views.plvAnnouncementPeriodLength = Praos2HasLeios (Milliseconds32 1000)
    , Views.plvVotePeriodLength = Praos2HasLeios (Milliseconds32 4000)
    , Views.plvDiffusionPeriodLength = Praos2HasLeios (Milliseconds32 1000)
    , Views.plvMaxEbBodySize = Praos2HasLeios 0
    , Views.plvMaxEbTxsSize = Praos2HasLeios 0
    }
