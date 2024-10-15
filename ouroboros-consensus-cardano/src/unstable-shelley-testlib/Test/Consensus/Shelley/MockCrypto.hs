{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeOperators #-}

module Test.Consensus.Shelley.MockCrypto (
    Block
  , CanMock
  , MockCrypto
  ) where

import           Cardano.Crypto.KES (MockKES)
import qualified Cardano.Crypto.KES as KES (Signable)
import           Cardano.Crypto.Util (SignableRepresentation)
import           Cardano.Crypto.VRF (MockVRF)
import           Cardano.Ledger.BaseTypes (Seed)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Core as Core
import           Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses)
import           Cardano.Protocol.Crypto (Crypto (..))
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL
import           Control.State.Transition.Extended (PredicateFailure)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.Praos.AgentClient (AgentCrypto (..))
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras (ShelleyEra)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible)
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import           Test.QuickCheck (Arbitrary)
import qualified Cardano.KESAgent.KES.Crypto as Agent
import qualified Cardano.KESAgent.Protocols.VersionedProtocol as Agent
import qualified Cardano.KESAgent.Processes.ServiceClient as Agent

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
instance Praos.PraosCrypto MockCrypto

type Block = ShelleyBlock (TPraos MockCrypto) ShelleyEra

-- | Cryptography that can easily be mocked
type CanMock proto era =
  ( ShelleyCompatible proto era
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  , Praos.PraosCrypto (ProtoCrypto proto)
  , SL.PraosCrypto (ProtoCrypto proto)
  , Core.EraTx era
  , SignableRepresentation Seed
  , SignableRepresentation (SL.BHBody (ProtoCrypto proto))
  , KES.Signable (KES (ProtoCrypto proto)) ~ SignableRepresentation
  , Eq (PredicateFailure (Core.EraRule "LEDGER" era))
  , Show (PredicateFailure (Core.EraRule "LEDGER" era))
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
  , Arbitrary (SL.CertState era)
  )

instance Agent.NamedCrypto (MockCrypto h) where
  cryptoName _ = Agent.CryptoName "Mock"

instance Agent.ServiceClientDrivers (MockCrypto h) where
  availableServiceClientDrivers = []

instance Agent.Crypto (MockCrypto h) where
  type KES (MockCrypto h) = MockKES 10
  type DSIGN (MockCrypto h) = MockDSIGN

instance HashAlgorithm h => AgentCrypto (MockCrypto h) where
  type ACrypto (MockCrypto h) = MockCrypto h
