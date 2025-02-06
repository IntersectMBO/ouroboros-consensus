{-# LANGUAGE DataKinds #-}

module Ouroboros.Consensus.Cardano (
    -- * The block type of the Cardano block chain
    CardanoBlock
    -- * Supported protocols
  , ProtocolByron
  , ProtocolCardano
  , ProtocolShelley
    -- * Abstract over the various protocols
  , CardanoHardForkTrigger (..)
  , CardanoHardForkTriggers (..)
  , module X
  ) where

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node as X
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node as X
import           Ouroboros.Consensus.Shelley.ShelleyHFC

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @cardano-node@.
-------------------------------------------------------------------------------}

type ProtocolByron   = HardForkProtocol '[ ByronBlock ]
type ProtocolCardano = HardForkProtocol '[ ByronBlock
                                         , ShelleyBlock (TPraos StandardCrypto) ShelleyEra
                                         , ShelleyBlock (TPraos StandardCrypto) AllegraEra
                                         , ShelleyBlock (TPraos StandardCrypto) MaryEra
                                         , ShelleyBlock (TPraos StandardCrypto) AlonzoEra
                                         , ShelleyBlock (Praos StandardCrypto)  BabbageEra
                                         , ShelleyBlock (Praos StandardCrypto)  ConwayEra
                                         ]
