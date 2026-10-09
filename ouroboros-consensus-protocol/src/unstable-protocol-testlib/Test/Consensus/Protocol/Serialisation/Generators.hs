{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generators suitable for serialisation. Note that these are not guaranteed
-- to be semantically correct at all, only structurally correct.
module Test.Consensus.Protocol.Serialisation.Generators () where

import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.KES (unsoundPureSignedKES)
import Cardano.Crypto.VRF (evalCertified)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Keys (DSIGN)
import Cardano.Protocol.Crypto (Crypto, VRF)
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
  , OCertSignable
  )
import Cardano.Slotting.Block (BlockNo (BlockNo))
import Cardano.Slotting.Slot
  ( SlotNo (SlotNo)
  , WithOrigin (At, Origin)
  )
import qualified Data.ByteString as BS
import LeiosDemoTypes
  ( EbAnnouncement (EbAnnouncement)
  , EbHash
  )
import Ouroboros.Consensus.Protocol.Leios (LeiosCrypto)
import Ouroboros.Consensus.Protocol.Praos
  ( AnnouncedBy (MkAnnouncedBy)
  , BasePraosState (PraosState)
  , EitherLeiosF
  )
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import Ouroboros.Consensus.Protocol.Praos.Common (toCodecEbAnnouncement)
import Ouroboros.Consensus.Protocol.Praos.Views (extendHeaderBodyWithLeios)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.StrictContainers.Instances ()
import Test.Crypto.KES ()
import Test.QuickCheck (Arbitrary (..), Gen, choose, oneof)
import Test.Util.LeiosHash (unsafeEbHashFromBytes)

instance Arbitrary EbHash where
  arbitrary = unsafeEbHashFromBytes . BS.pack <$> vectorOfWord8 32
   where
    vectorOfWord8 n = sequence (replicate n arbitrary)

instance Arbitrary EbAnnouncement where
  arbitrary = EbAnnouncement <$> arbitrary <*> arbitrary

instance Arbitrary AnnouncedBy where
  arbitrary = MkAnnouncedBy <$> arbitrary <*> arbitrary

instance Arbitrary InputVRF where
  arbitrary = mkInputVRF <$> arbitrary <*> arbitrary

instance
  ( Crypto c
  , DSIGN.Signable DSIGN (OCertSignable c)
  , VRF.Signable (VRF c) InputVRF
  ) =>
  Arbitrary (HeaderBody c)
  where
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

instance LeiosCrypto c => Arbitrary (Leios.HeaderBody c) where
  arbitrary = extendHeaderBodyWithLeios <$> arbitrary <*> arbitrary <*> arbitrary

instance LeiosCrypto c => Arbitrary (Leios.Header c) where
  arbitrary = do
    hBody <- arbitrary
    period <- arbitrary
    sKey <- arbitrary
    let hSig = unsoundPureSignedKES () period hBody sKey
    pure $ Leios.Header hBody hSig

instance
  forall proto.
  ( Applicative (EitherLeiosF proto ())
  , Traversable (EitherLeiosF proto ())
  ) =>
  Arbitrary (BasePraosState proto)
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
      <*> traverse (\() -> arbitrary) (pure () :: EitherLeiosF proto () ())
