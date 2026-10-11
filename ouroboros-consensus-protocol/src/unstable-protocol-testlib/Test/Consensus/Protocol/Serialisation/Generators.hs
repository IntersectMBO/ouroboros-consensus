{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generators suitable for serialisation. Note that these are not guaranteed
-- to be semantically correct at all, only structurally correct.
module Test.Consensus.Protocol.Serialisation.Generators () where

import Cardano.Crypto.KES (unsoundPureSignedKES)
import Cardano.Crypto.VRF (evalCertified)
import Cardano.Ledger.BaseTypes (ProtVer (..), getVersion32)
import Cardano.Ledger.Block (BlockHeaderVersionInfo (..), EbReferencesAnnouncement (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import Cardano.Protocol.Praos.BlockHeader
  ( Header (Header)
  , HeaderBody (HeaderBody)
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
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Protocol.Praos
  ( AnnouncedBy (MkAnnouncedBy)
  , PolyPraosState (PraosState)
  )
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import Ouroboros.Consensus.Protocol.Praos.Common (pureLeiosOnly)
import Ouroboros.Consensus.Protocol.Praos2 (LeiosCrypto)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Crypto.KES ()
import Test.QuickCheck (Arbitrary (..), Gen, choose, oneof)

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

-- | A Praos header body extended with arbitrary Leios fields.
instance (Praos.PraosCrypto c, LeiosCrypto c) => Arbitrary (Leios.HeaderBody c) where
  arbitrary = do
    HeaderBody blockNo slotNo prev vk vrfVk vrfRes bodySize bodyHash ocert (ProtVer major minor) <-
      arbitrary
    containsCert <- arbitrary
    ann <- arbitrary
    pure $
      Leios.mkHeaderBody (Proxy @DijkstraEra) $
        Leios.HeaderBodyRaw
          { Leios.hbrBlockNo = blockNo
          , Leios.hbrSlotNo = slotNo
          , Leios.hbrPrev = prev
          , Leios.hbrVk = vk
          , Leios.hbrVrfVk = vrfVk
          , Leios.hbrVrfRes = vrfRes
          , Leios.hbrBodySize = bodySize
          , Leios.hbrBodyHash = bodyHash
          , Leios.hbrOCert = ocert
          , Leios.hbrVersionInfo = BlockHeaderVersionInfo (getVersion32 major) minor
          , Leios.hbrBlockBodyContainsLeiosCert = containsCert
          , Leios.hbrEbReferencesAnnouncement = uncurry EbReferencesAnnouncement <$> ann
          }

instance (Praos.PraosCrypto c, LeiosCrypto c) => Arbitrary (Leios.Header c) where
  arbitrary = do
    hBody <- arbitrary
    period <- arbitrary
    sKey <- arbitrary
    let hSig = unsoundPureSignedKES () period hBody sKey
    pure $ Leios.mkHeader (Proxy @DijkstraEra) hBody hSig

instance Arbitrary AnnouncedBy where
  arbitrary =
    MkAnnouncedBy
      <$> arbitrary
      <*> (EbReferencesAnnouncement <$> arbitrary <*> arbitrary)

instance
  ( Applicative (Praos.WhenLeios proto)
  , Traversable (Praos.WhenLeios proto)
  ) =>
  Arbitrary (PolyPraosState proto)
  where
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
      <*> traverse (\() -> arbitrary) (pureLeiosOnly ())
