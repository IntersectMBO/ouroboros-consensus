{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Shelley.MockCrypto (
    Block
  , CanMock
  , MockCrypto
  ) where

import           Cardano.Crypto.KES (MockKES)
import           Cardano.Crypto.VRF (MockVRF)
-- import           Test.Cardano.Protocol.Crypto.VRF.Fake (SneakilyContainResult)
-- import           Cardano.Crypto.Hash (hashToBytes)
import           Cardano.Protocol.Crypto (Crypto (..))
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Core as Core
import           Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses)
-- import           Cardano.Ledger.BaseTypes (shelleyProtVer)
-- import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), hashWithEncoder, encCBOR)
import qualified Cardano.Protocol.TPraos.API as SL
import           Control.State.Transition.Extended (PredicateFailure)
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
-- import           Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras (ShelleyEra)
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, ShelleyCompatible)
import           Test.QuickCheck (Arbitrary)
-- import           Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)

-- | A mock replacement for 'StandardCrypto'
--
-- We run the tests with this mock crypto, as it is easier to generate and
-- debug things. The code is parametric in the crypto, so it shouldn't make
-- much of a difference. This also has the important advantage
-- that we can reuse the generators from cardano-ledger-specs.
data MockCrypto

instance Crypto MockCrypto where
  type KES      MockCrypto = MockKES 10
  type VRF      MockCrypto = MockVRF

instance SL.PraosCrypto MockCrypto
-- instance Praos.PraosCrypto MockCrypto

-- instance SneakilyContainResult InputVRF where
--   type Payload InputVRF = InputVRF
--   sneakilyExtractResult s sk =
--     OutputVRF
--       . hashToBytes
--       . hashWithEncoder @Blake2b_224 shelleyProtVer id
--       $ encCBOR s <> encCBOR sk
--   unsneakilyExtractPayload = id

--   -- sneakilyExtractResult :: a -> SignKeyVRF FakeVRF -> OutputVRF FakeVRF 
--   -- unsneakilyExtractPayload :: a -> Payload a

type Block = ShelleyBlock (TPraos MockCrypto) ShelleyEra

-- | Cryptography that can easily be mocked
type CanMock proto era =
  ( ShelleyCompatible proto era
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  , Praos.PraosCrypto (ProtoCrypto proto)
  , Core.EraTx era
  , Arbitrary (Core.TxAuxData era)
  , Arbitrary (Core.PParams era)
  , Arbitrary (Core.PParamsUpdate era)
  , Arbitrary (Core.Script era)
  , Arbitrary (Core.TxBody era)
  , Arbitrary (Core.Tx era)
  , Arbitrary (Core.TxOut era)
  , Arbitrary (Core.Value era)
  , Arbitrary (PredicateFailure (SL.ShelleyUTXOW era))
  , Arbitrary (Core.TxWits era)
  , Arbitrary (StashedAVVMAddresses era)
  , Arbitrary (Core.GovState era)
  )
