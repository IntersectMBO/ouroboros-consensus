{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Protocol.Praos.SelectView (tests) where

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Util as Crypto
import Cardano.Crypto.VRF (OutputVRF, mkTestOutputVRF)
import qualified Cardano.Ledger.Keys as SL
import Cardano.Protocol.Crypto (Crypto (..), StandardCrypto)
import Codec.Serialise (encode)
import Control.Monad
import Data.Containers.ListUtils (nubOrdOn)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.Praos.Common
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Ouroboros.Consensus.Protocol
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.TestEnv

tests :: TestTree
tests =
  testGroup
    "Praos SelectView"
    [ adjustQuickCheckTests (* 50)
      -- Use a small max size by default in order to have a decent chance to
      -- trigger the actual tiebreaker cases.
      $
        adjustQuickCheckMaxSize (`div` 10) $
          tests_chainOrder (Proxy @(SelectView (Praos StandardCrypto)))
    ]

instance Crypto c => Arbitrary (SelectView (Praos c)) where
  arbitrary = do
    size <- fromIntegral <$> getSize
    svBlockNo <- BlockNo <$> choose (1, size)
    ptvSlotNo <- SlotNo <$> choose (1, size)
    ptvIssuer <- elements knownIssuers
    ptvIssueNo <- choose (1, 10)
    pure
      SelectView
        { svBlockNo = svBlockNo
        , svTiebreakerView =
            PraosTiebreakerView
              { ptvSlotNo
              , ptvIssuer
              , ptvIssueNo
              , ptvTieBreakVRF = mkVRFFor ptvIssuer ptvSlotNo
              }
        }
   where
    -- We want to draw from the same small set of issuer identities in order to
    -- have a chance to explore cases where the issuers of two 'SelectView's
    -- are identical.
    knownIssuers :: [SL.VKey SL.BlockIssuer]
    knownIssuers =
      nubOrdOn SL.hashKey $
        unGen (replicateM numIssuers (SL.VKey <$> arbitrary)) randomSeed 100
     where
      randomSeed = mkQCGen 4 -- chosen by fair dice roll
      numIssuers = 10

    -- The header VRF is a deterministic function of the issuer VRF key, the
    -- slot and the epoch nonce. Additionally, for any particular chain, the
    -- slot determines the epoch nonce.
    mkVRFFor :: SL.VKey SL.BlockIssuer -> SlotNo -> OutputVRF (VRF c)
    mkVRFFor issuer slot =
      mkTestOutputVRF $
        Crypto.bytesToNatural $
          Crypto.hashToBytes $
            Crypto.xor (Crypto.castHash issuerHash) $
              Crypto.hashWithSerialiser encode slot
     where
      SL.KeyHash issuerHash = SL.hashKey issuer

-- | @'ChainOrderConfig' ('SelectView' 'Praos')@
instance Arbitrary VRFTiebreakerFlavor where
  arbitrary =
    oneof
      [ pure UnrestrictedVRFTiebreaker
      , do
          size <- max 1 . fromIntegral <$> getSize
          RestrictedVRFTiebreaker . SlotNo <$> choose (1, size)
      ]

  shrink = \case
    UnrestrictedVRFTiebreaker -> []
    RestrictedVRFTiebreaker maxDist ->
      UnrestrictedVRFTiebreaker
        : (RestrictedVRFTiebreaker <$> shrink maxDist)
