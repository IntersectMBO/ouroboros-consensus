{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module Legacy.Cardano.Block (
    -- * The eras of the Cardano blockchain
    LegacyCardanoEras
  , LegacyCardanoShelleyEras
    -- * The block type of the Cardano blockchain
  , LegacyCardanoBlock
  , pattern LegacyCardanoBlock
    -- * Generalised transactions
  , LegacyCardanoApplyTxErr
  , LegacyCardanoGenTx
  , LegacyCardanoGenTxId
  , pattern LegacyCardanoApplyTxErr
  , pattern LegacyCardanoGenTx
  , pattern LegacyCardanoGenTxId
    -- * LedgerConfig
  , LegacyCardanoLedgerConfig
  , pattern LegacyCardanoLedgerConfig
  ) where

import           Data.Kind
import           Data.SOP.Strict
import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Trans
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Legacy.Block
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  The eras of the Cardano blockchain
-------------------------------------------------------------------------------}

type LegacyCardanoEras :: Type -> [Type]
type LegacyCardanoEras c =  LegacyBlock ByronBlock
                         ': LegacyCardanoShelleyEras c

type LegacyCardanoShelleyEras :: Type -> [Type]
type LegacyCardanoShelleyEras c =
  '[ LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c))
   , LegacyBlock (ShelleyBlock (Praos c)  (BabbageEra c))
   , LegacyBlock (ShelleyBlock (Praos c)  (ConwayEra c))
   ]

{-------------------------------------------------------------------------------
  The block type of the Cardano blockchain
-------------------------------------------------------------------------------}

type LegacyCardanoBlock c = LegacyBlock (HardForkBlock (LegacyCardanoEras c))

{-# COMPLETE LegacyCardanoBlock #-}

pattern LegacyCardanoBlock :: CardanoBlock c -> LegacyCardanoBlock c
pattern LegacyCardanoBlock b <- ( hcoerce_HardForkBlock . getLegacyBlock -> b)
  where LegacyCardanoBlock b = LegacyBlock . hcoerce_HardForkBlock $ b

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

type LegacyCardanoGenTx c = GenTx (LegacyCardanoBlock c)

{-# COMPLETE LegacyCardanoGenTx #-}

pattern LegacyCardanoGenTx :: CardanoGenTx c -> LegacyCardanoGenTx c
pattern LegacyCardanoGenTx gentx <- (hcoerce_GenTx . getLegacyGenTx -> gentx)
  where LegacyCardanoGenTx gentx = LegacyGenTx . hcoerce_GenTx $ gentx

type LegacyCardanoGenTxId c = GenTxId (LegacyCardanoBlock c)

{-# COMPLETE LegacyCardanoGenTxId #-}

pattern LegacyCardanoGenTxId :: CardanoGenTxId c -> LegacyCardanoGenTxId c
pattern LegacyCardanoGenTxId gtxid <- (hcoerce_GenTxId . getLegacyGenTxId -> gtxid)
  where LegacyCardanoGenTxId gtxid = LegacyGenTxId . hcoerce_GenTxId $ gtxid

type LegacyCardanoApplyTxErr c = HardForkApplyTxErr (LegacyCardanoEras c)

{-# COMPLETE LegacyCardanoApplyTxErr #-}

pattern LegacyCardanoApplyTxErr :: CardanoApplyTxErr c -> LegacyCardanoApplyTxErr c
pattern LegacyCardanoApplyTxErr err <- (hcoerce_ApplyTxErr -> err)
  where LegacyCardanoApplyTxErr err = hcoerce_ApplyTxErr err

{-------------------------------------------------------------------------------
  LedgerConfig
-------------------------------------------------------------------------------}

type LegacyCardanoLedgerConfig c = HardForkLedgerConfig (LegacyCardanoEras c)

{-# COMPLETE LegacyCardanoLedgerConfig #-}

pattern LegacyCardanoLedgerConfig :: CardanoLedgerConfig c -> LegacyCardanoLedgerConfig c
pattern LegacyCardanoLedgerConfig cfg <- (hcoerce_LedgerConfig -> cfg)
  where LegacyCardanoLedgerConfig cfg = hcoerce_LedgerConfig cfg
