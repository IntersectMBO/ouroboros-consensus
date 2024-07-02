{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (CanHardFork (..)) where

import           NoThunks.Class (NoThunks)
import           Data.Measure (BoundedMeasure)
import           Data.SOP.Constraint
import           Data.SOP.Functors (Product2)
import           Data.SOP.InPairs (InPairs, RequiringBoth)
import qualified Data.SOP.InPairs as InPairs
import           Data.SOP.NonEmpty
import qualified Data.SOP.Strict as SOP
import           Data.SOP.Tails (Tails)
import qualified Data.SOP.Tails as Tails
import           Data.Typeable
import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Network.SizeInBytes (SizeInBytes)

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

class ( All SingleEraBlock xs
      , Typeable xs
      , IsNonEmpty xs
      , BoundedMeasure (HardForkTxMeasure xs)
      , NoThunks (HardForkTxMeasure xs)
      ) => CanHardFork xs where
  type HardForkTxMeasure xs

  hardForkMeasureTx :: SOP.NS WrapTxMeasure xs -> HardForkTxMeasure xs

  hardForkTxMeasureBytes :: proxy xs -> HardForkTxMeasure xs -> SizeInBytes

  hardForkEraTranslation :: EraTranslation xs
  hardForkChainSel       :: Tails AcrossEraSelection xs
  hardForkInjectTxs      ::
    InPairs
      ( RequiringBoth
          WrapLedgerConfig
          (Product2 InjectTx InjectValidatedTx)
      )
      xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  type HardForkTxMeasure '[blk] = TxMeasure blk

  hardForkMeasureTx (SOP.Z (WrapTxMeasure x)) = x

  hardForkTxMeasureBytes _ = txMeasureBytes (Proxy @blk)

  hardForkEraTranslation = trivialEraTranslation
  hardForkChainSel       = Tails.mk1
  hardForkInjectTxs      = InPairs.mk1
