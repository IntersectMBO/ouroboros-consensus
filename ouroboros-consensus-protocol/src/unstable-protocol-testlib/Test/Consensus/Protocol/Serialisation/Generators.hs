{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generators suitable for serialisation. Note that these are not guaranteed
-- to be semantically correct at all, only structurally correct.
module Test.Consensus.Protocol.Serialisation.Generators () where

import Cardano.Crypto.KES (unsoundPureSignedKES)
import Cardano.Crypto.VRF (evalCertified)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import Cardano.Protocol.Praos.BlockHeader
  ( Header (Header)
  , HeaderBody (..)
  )
import Cardano.Protocol.Praos.VRF (InputVRF, mkInputVRF)
import Cardano.Protocol.TPraos.BlockHeader (HashHeader, PrevHash (..))
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
import Data.Proxy (Proxy (Proxy))
import LeiosDemoTypes
  ( EbAnnouncement (EbAnnouncement)
  , EbHash (MkEbHash)
  )
import Ouroboros.Consensus.Protocol.Praos (BasePraosState (PraosState))
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import Ouroboros.Consensus.Protocol.Praos.Common
  ( KnownPraosExtension (praosExtensionHasLeios)
  , StrictMaybeLeios (SJustLeios, SNothingLeios)
  , toCodecEbAnnouncement
  , WhetherHasLeiosDecided
      ( PextDoesNotHaveLeiosDecided
      , PextHasLeiosDecided
      )
  )
import Ouroboros.Consensus.Protocol.Praos.Views (extendHeaderBodyWithLeios)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.StrictContainers.Instances ()
import Test.Crypto.KES ()
import Test.QuickCheck (Arbitrary (..), Gen, choose, oneof)

instance Arbitrary EbHash where
  arbitrary = MkEbHash . BS.pack <$> vectorOfWord8 32
   where
    vectorOfWord8 n = sequence (replicate n arbitrary)

instance Arbitrary EbAnnouncement where
  arbitrary = EbAnnouncement <$> arbitrary <*> arbitrary

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

instance Praos.PraosCrypto c => Arbitrary (Header c) where
  arbitrary = do
    hBody <- arbitrary
    period <- arbitrary
    sKey <- arbitrary
    let hSig = unsoundPureSignedKES () period hBody sKey
    pure $ Header hBody hSig

instance Arbitrary Leios.EbAnnouncement where
  arbitrary = toCodecEbAnnouncement <$> arbitrary

instance Praos.PraosCrypto c => Arbitrary (Leios.HeaderBody c) where
  arbitrary = extendHeaderBodyWithLeios <$> arbitrary <*> arbitrary <*> arbitrary

instance Praos.PraosCrypto c => Arbitrary (Leios.Header c) where
  arbitrary = do
    hBody <- arbitrary
    period <- arbitrary
    sKey <- arbitrary
    let hSig = unsoundPureSignedKES () period hBody sKey
    pure $ Leios.Header hBody hSig

instance KnownPraosExtension pext => Arbitrary (BasePraosState pext) where
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
      <*> traverse
        (\() -> arbitrary)
        ( case praosExtensionHasLeios (Proxy @pext) of
            PextDoesNotHaveLeiosDecided -> SNothingLeios
            PextHasLeiosDecided -> SJustLeios ()
        )
