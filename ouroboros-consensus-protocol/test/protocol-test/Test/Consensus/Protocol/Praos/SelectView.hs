{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Protocol.Praos.SelectView (tests) where

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Util as Crypto
import           Cardano.Crypto.VRF (OutputVRF, mkTestOutputVRF)
import           Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import qualified Cardano.Ledger.Keys as SL
import           Codec.Serialise (encode)
import           Control.Monad
import           Data.Containers.ListUtils (nubOrdOn)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Praos.Common
import           Test.Cardano.Ledger.Binary.Arbitrary ()
import           Test.Ouroboros.Consensus.Protocol
import           Test.QuickCheck.Gen (Gen (..))
import           Test.QuickCheck.Random (mkQCGen)
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (elements)
import           Test.Util.QuickCheck
import           Test.Util.TestEnv

tests :: TestTree
tests = testGroup "PraosChainSelectView"
    [   adjustQuickCheckTests (* 50)
      -- Use a small max size by default in order to have a decent chance to
      -- trigger the actual tiebreaker cases.
      $ adjustQuickCheckMaxSize (`div` 10)
      $ tests_chainOrder (Proxy @(PraosChainSelectView StandardCrypto))
    ]

instance Crypto c => Arbitrary (PraosChainSelectView c) where
  arbitrary = do
      size           <- fromIntegral <$> getSize
      csvChainLength <- BlockNo <$> choose (1, size)
      csvSlotNo      <- SlotNo  <$> choose (1, size)
      csvIssuer      <- elements knownIssuers
      csvIssueNo     <- genIssueNo
      pure PraosChainSelectView {
          csvChainLength
        , csvSlotNo
        , csvIssuer
        , csvIssueNo
        , csvTieBreakVRF = mkVRFFor csvIssuer csvSlotNo
        }
   where
     -- We want to draw from the same small set of issuer identities in order to
     -- have a chance to explore cases where the issuers of two 'SelectView's
     -- are identical.
     knownIssuers :: [SL.VKey SL.BlockIssuer c]
     knownIssuers =
           nubOrdOn SL.hashKey
         $ unGen (replicateM numIssuers (SL.VKey <$> arbitrary)) randomSeed 100
       where
         randomSeed = mkQCGen 4 -- chosen by fair dice roll
         numIssuers = 10

     -- TODO Actually randomize this once the issue number tiebreaker has been
     -- fixed to be transitive. See the document in
     -- https://github.com/IntersectMBO/ouroboros-consensus/pull/891 for
     -- details.
     --
     -- TL;DR: In an edge case, the issue number tiebreaker prevents the
     -- chain order from being transitive. This could be fixed relatively
     -- easily, namely by swapping the issue number tiebreaker and the VRF
     -- tiebreaker. However, this is technically not backwards-compatible,
     -- impacting the current pre-Conway diffusion pipelining scheme.
     --
     -- See https://github.com/IntersectMBO/ouroboros-consensus/issues/1075.
     genIssueNo = pure 1

     -- The header VRF is a deterministic function of the issuer VRF key, the
     -- slot and the epoch nonce. Additionally, for any particular chain, the
     -- slot determines the epoch nonce.
     mkVRFFor :: SL.VKey SL.BlockIssuer c -> SlotNo -> OutputVRF (VRF c)
     mkVRFFor issuer slot =
           mkTestOutputVRF
         $ Crypto.bytesToNatural
         $ Crypto.hashToBytes
         $ Crypto.xor (Crypto.castHash issuerHash)
         $ Crypto.hashWithSerialiser encode slot
       where
         SL.KeyHash issuerHash = SL.hashKey issuer

-- | 'ChainOrderConfig' 'PraosChainSelectView'
instance Arbitrary VRFTiebreakerFlavor where
  arbitrary = oneof
      [ pure UnrestrictedVRFTiebreaker
      , do
          size <- max 1 . fromIntegral <$> getSize
          RestrictedVRFTiebreaker . SlotNo <$> choose (1, size)
      ]

  shrink = \case
      UnrestrictedVRFTiebreaker       -> []
      RestrictedVRFTiebreaker maxDist ->
          UnrestrictedVRFTiebreaker
        : (RestrictedVRFTiebreaker <$> shrink maxDist)
