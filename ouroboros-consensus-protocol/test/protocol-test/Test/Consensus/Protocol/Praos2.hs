{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Protocol.Praos2 (tests) where

import Cardano.Protocol.Crypto (StandardCrypto)
import Data.Proxy (Proxy (..))
import qualified Data.Strict.Either as Strict
import Ouroboros.Consensus.Protocol.Abstract (TranslateProto (..))
import Ouroboros.Consensus.Protocol.PolyPraos (PolyPraosState (..), WhyNotCertifiable (..))
import Ouroboros.Consensus.Protocol.Praos2 (LeiosOnly (Praos2HasLeios), Praos2)
import Test.Consensus.Protocol.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Praos2"
    [ testProperty
        "crossing into another era with Leios makes an announcement uncertifiable"
        prop_crossingEraMakesAnnouncementUncertifiable
    ]

-- | An endorser block may not be certified across an era boundary.
prop_crossingEraMakesAnnouncementUncertifiable ::
  PolyPraosState (Praos2 StandardCrypto) -> Property
prop_crossingEraMakesAnnouncementUncertifiable st =
  translateChainDepState (Proxy @(Praos2 StandardCrypto, Praos2 StandardCrypto, ())) st
    === st{praosStateLeiosAnnouncement = Praos2HasLeios expected}
 where
  Praos2HasLeios before = praosStateLeiosAnnouncement st
  expected = case before of
    Strict.Left why -> Strict.Left why
    Strict.Right _ann -> Strict.Left AnnouncedInPreviousEra
