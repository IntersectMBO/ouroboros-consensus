{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generators suitable for serialisation. Note that these are not guaranteed
-- to be semantically correct at all, only structurally correct.
module Test.Consensus.Protocol.Serialisation.Generators () where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Crypto.KES (unsoundPureSignedKES)
import Cardano.Crypto.VRF (evalCertified)
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
import qualified Data.ByteString.Short as SBS
import qualified Data.Strict.Either as Strict
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement (..))
import Ouroboros.Consensus.Protocol.PolyPraos (WhyNotCertifiable (..))
import Ouroboros.Consensus.Protocol.Praos
  ( PolyPraosState (PraosState)
  )
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import Ouroboros.Consensus.Protocol.Praos.Common (pureLeiosOnly)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Crypto.KES ()
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, oneof, vectorOf)

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

instance Arbitrary LeiosAnnouncement where
  arbitrary =
    MkLeiosAnnouncement
      <$> arbitrary
      <*> (UnsafeHash . SBS.pack <$> vectorOf 32 arbitrary)
      <*> arbitrary

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
      <*> traverse (\() -> genCertifiable) (pureLeiosOnly ())

genCertifiable :: Gen (Strict.Either WhyNotCertifiable LeiosAnnouncement)
genCertifiable =
  oneof
    [ Strict.Left <$> arbitrary
    , Strict.Right <$> arbitrary
    ]

instance Arbitrary WhyNotCertifiable where
  arbitrary = elements [NothingAnnounced, AnnouncedInPreviousEra, AnnouncedInPreviousEpoch]
