{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generators suitable for serialisation. Note that these are not guaranteed
-- to be semantically correct at all, only structurally correct.
module Test.Consensus.Protocol.Serialisation.Generators () where

import Cardano.Crypto.KES (unsoundPureSignedKES)
import Cardano.Crypto.VRF (evalCertified)
import Cardano.Protocol.TPraos.BHeader (HashHeader, PrevHash (..))
import Cardano.Protocol.TPraos.OCert
  ( KESPeriod (KESPeriod)
  , OCert (OCert)
  )
import Cardano.Slotting.Block (BlockNo (BlockNo))
import Cardano.Slotting.Slot
  ( SlotNo (SlotNo)
  , WithOrigin (At, Origin)
  )
import qualified Data.ByteString as BS
import LeiosDemoTypes (EbAnnouncement (EbAnnouncement), EbHash (MkEbHash))
import Ouroboros.Consensus.Protocol.Praos (LeiosState (LeiosState), PraosState (PraosState))
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import Ouroboros.Consensus.Protocol.Praos.Header
  ( Header (Header)
  , HeaderBody (HeaderBody)
  )
import Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF, mkInputVRF)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Crypto.KES ()
import Test.Crypto.VRF ()
import Test.QuickCheck (Arbitrary (..), Gen, choose, oneof, vector)

instance Arbitrary InputVRF where
  arbitrary = mkInputVRF <$> arbitrary <*> arbitrary

instance Praos.PraosCrypto c => Arbitrary (HeaderBody c) where
  arbitrary =
    let ocert =
          OCert
            <$> arbitrary
            <*> arbitrary
            <*> (KESPeriod <$> arbitrary)
            <*> arbitrary

        certVrf =
          evalCertified ()
            <$> (arbitrary :: Gen InputVRF)
            <*> arbitrary
     in HeaderBody
          <$> (BlockNo <$> choose (1, 10))
          <*> (SlotNo <$> choose (1, 10))
          <*> oneof
            [ pure GenesisHash
            , BlockHash <$> (arbitrary :: Gen HashHeader)
            ]
          <*> arbitrary
          <*> arbitrary
          <*> certVrf
          <*> arbitrary
          <*> arbitrary
          <*> ocert
          <*> arbitrary
          <*> arbitrary

instance Praos.PraosCrypto c => Arbitrary (Header c) where
  arbitrary = do
    hBody <- arbitrary
    period <- arbitrary
    sKey <- arbitrary
    let hSig = unsoundPureSignedKES () period hBody sKey
    pure $ Header hBody hSig

instance Arbitrary PraosState where
  arbitrary =
    PraosState
      <$> oneof
        [ pure Origin
        , At <$> (SlotNo <$> choose (1, 10))
        ]
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary EbAnnouncement where
  arbitrary = EbAnnouncement <$> (MkEbHash . BS.pack <$> vector 32) <*> arbitrary

instance Arbitrary LeiosState where
  arbitrary = pure $ LeiosState Nothing False 0
