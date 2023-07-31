{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The implementation of the LedgerTables for Cardano
--
-- Note that normally the code that deals with tables is polymorphic on the @l@
-- used so this module is only needed to be imported for the instances (except
-- perhaps in tests).
--
-- > import Ouroboros.Consensus.Cardano.Tables ()
module Ouroboros.Consensus.Cardano.Tables (
    -- * Testing
    LedgerTables (..)
  , TranslateTxOutWrapper (..)
  , composeTxOutTranslationPairs
  ) where

import           Cardano.Binary (fromCBOR, toCBOR)
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Babbage.Translation as Babbage
import qualified Cardano.Ledger.Conway.Translation as Conway
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as SL
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.SOP.Index
import           Data.SOP.Strict
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.ShelleyHFC

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

type instance Key   (LedgerState (CardanoBlock c)) = SL.TxIn c
type instance Value (LedgerState (CardanoBlock c)) = ShelleyTxOut (ShelleyBasedEras c)

instance CardanoHardForkConstraints c
      => CanSerializeLedgerTables (LedgerState (CardanoBlock c)) where
    -- The Ledger and Consensus team discussed the fact that we need to be able
    -- to reach the TxIn key for an entry from any era, regardless of the era in
    -- which it was created, therefore we need to have a "canonical"
    -- serialization that doesn't change between eras. For now we are using
    -- @'toEraCBOR' \@('ShelleyEra' c)@ as a stop-gap, but Ledger will provide a
    -- serialization function into something more efficient.
    codecLedgerTables = LedgerTables (CodecMK
                                       (Core.toEraCBOR @(ShelleyEra c))
                                       toCBOR
                                       (Core.fromEraCBOR @(ShelleyEra c))
                                       fromCBOR)

{-------------------------------------------------------------------------------
  LedgerTablesCanHardFork
-------------------------------------------------------------------------------}

instance CardanoHardForkConstraints c
      => LedgerTablesCanHardFork (CardanoEras c) where
  hardForkInjectLedgerTables =
         byron
      :* shelley IZ
      :* shelley (IS IZ)
      :* shelley (IS (IS IZ))
      :* shelley (IS (IS (IS IZ)))
      :* shelley (IS (IS (IS (IS IZ))))
      :* shelley (IS (IS (IS (IS (IS IZ)))))
      :* Nil
    where
      byron :: InjectLedgerTables (CardanoEras c) ByronBlock
      byron = InjectLedgerTables {
          applyInjectLedgerTables  = const emptyLedgerTables
        , applyDistribLedgerTables = const emptyLedgerTables
        }

      shelley ::
           forall era proto. EraCrypto era ~ c
        => Index (ShelleyBasedEras c) era
        -> InjectLedgerTables (CardanoEras c) (ShelleyBlock proto era)
      shelley idx = InjectLedgerTables {
          applyInjectLedgerTables  =
              LedgerTables . mapMK inj . getLedgerTables
        , applyDistribLedgerTables =
              LedgerTables . mapMK distrib . getLedgerTables
        }
        where
          inj :: Core.TxOut era -> ShelleyTxOut (ShelleyBasedEras c)
          inj = ShelleyTxOut
              . injectNS idx
              . TxOutWrapper

          distrib :: ShelleyTxOut (ShelleyBasedEras c) -> Core.TxOut era
          distrib = unTxOutWrapper
                  . apFn (projectNP idx shelleyTxOutTranslations)
                  . K

-- | The composed translations for each possible era; see
-- 'composeTxOutTranslationPairs' to understand why this is partial but
-- is safe in the absence of Consensus bugs.
shelleyTxOutTranslations ::
     forall c. CardanoHardForkConstraints c
  => NP
        (K (ShelleyTxOut (ShelleyBasedEras c)) -.-> TxOutWrapper)
        (ShelleyBasedEras c)
shelleyTxOutTranslations =
  hmap
    (\f -> fn $ \(K (ShelleyTxOut x)) -> f `apFn` K x)
    (composeTxOutTranslationPairs translateTxOut')
 where
  translateTxOut' :: InPairs
                      TranslateTxOutWrapper
                      (ShelleyBasedEras c)
  translateTxOut' =
      PCons (TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (TranslateTxOutWrapper Alonzo.translateTxOut)
    $ PCons (TranslateTxOutWrapper Babbage.translateTxOut)
    $ PCons (TranslateTxOutWrapper Conway.translateTxOut)
       PNil

-- | Auxiliary for convenience
--
-- We can't reuse 'Translate' because we don't have the 'EpochNo' it requires.
newtype TranslateTxOutWrapper era1 era2 =
    TranslateTxOutWrapper (Core.TxOut era1 -> Core.TxOut era2)

-- | Auxiliary @SOP@ combinator
--
-- WARNING: The functions in the result fail if the 'NS' argument's tag
-- precedes the 'NP' index, i.e. if we try to translate into previous eras.
--
-- TODO use an accumulator instead of this quadratic traversal
composeTxOutTranslationPairs ::
     (SListI eras, HasCallStack)
  => InPairs
       TranslateTxOutWrapper
       eras
  -> NP
       (K (NS TxOutWrapper eras) -.-> TxOutWrapper)
       eras
composeTxOutTranslationPairs = \case
    PNil                                  ->
      fn (unZ . unK) :* Nil
    PCons (TranslateTxOutWrapper f) inner ->
      fn ( eitherNS
              id
              (error "composeTxOutTranslationPairs: anachrony")
          . unK
         )
      :* hmap
          (\innerf -> fn $
              apFn innerf
            . K
            . eitherNS
                (Z . TxOutWrapper . f . unTxOutWrapper)
                id
            . unK)
          (composeTxOutTranslationPairs inner)
  where
    eitherNS :: (f x -> c) -> (NS f xs -> c) -> NS f (x ': xs) -> c
    eitherNS l r = \case
      Z x -> l x
      S x -> r x
