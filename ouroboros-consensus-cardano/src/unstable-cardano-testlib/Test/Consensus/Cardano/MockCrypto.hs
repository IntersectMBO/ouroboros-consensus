{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron) where

import           Cardano.Crypto.KES (MockKES)
import           Cardano.Crypto.VRF (MockVRF)
import           Cardano.Protocol.Crypto (Crypto (..))
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos

-- | A replacement for 'Test.Consensus.Shelley.MockCrypto' that is compatible
-- with bootstrapping from Byron.
--
-- * The "Ouroboros.Consensus.Cardano.CanHardFork" translation requires that
--   @ADDRHASH@ has the same bit size as Byron address hashes (ie 224); that's why
--   we use 'Blake2b_224' here.
--
-- * Similarly, @HASH@ has to have the same bit size as Byron header hashes (ie
--   256), that's why we use 'Blake2b_256' here.
--
-- * The @cardano-ledger-shelley@ package currently requires that @'DSIGN' ~
--   'Ed25519DSIGN' in order to use Byron bootstrap witnesses.
--
-- * We can still use mock KES and mock VRF.
--
-- Note that many Shelley generators are not instantiated to 'MockShelley' but
-- are constrained by @'CanMock' era@. @'ShelleyEra' 'MockCryptoCompatByron'@
-- satisfies this constraint, allowing us to reuse these generators for Cardano.
data MockCryptoCompatByron

instance Crypto MockCryptoCompatByron where
  type KES      MockCryptoCompatByron = MockKES 10
  type VRF      MockCryptoCompatByron = MockVRF

instance TPraos.PraosCrypto MockCryptoCompatByron

instance Praos.PraosCrypto MockCryptoCompatByron
