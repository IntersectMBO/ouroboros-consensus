{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
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

module Ouroboros.Consensus.Cardano.CanonicalTxIn () where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.SOP.Index
import           Data.Void
import           NoThunks.Class
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator

instance CardanoHardForkConstraints c
      => HasCanonicalTxIn (CardanoEras c) where
  newtype instance CanonicalTxIn (CardanoEras c) = CardanoTxIn {
      getCardanoTxIn :: SL.TxIn c
    }
    deriving (Show, Eq, Ord, NoThunks)

  injectCanonicalTxIn IZ       byronTxIn   = absurd byronTxIn
  injectCanonicalTxIn (IS idx) shelleyTxIn = case idx of
      IZ                               -> CardanoTxIn shelleyTxIn
      IS IZ                            -> CardanoTxIn shelleyTxIn
      IS (IS IZ)                       -> CardanoTxIn shelleyTxIn
      IS (IS (IS IZ))                  -> CardanoTxIn shelleyTxIn
      IS (IS (IS (IS IZ)))             -> CardanoTxIn shelleyTxIn
      IS (IS (IS (IS (IS IZ))))        -> CardanoTxIn shelleyTxIn
      IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

  distribCanonicalTxIn IZ _                 =
      error "distribCanonicalTxIn: Byron has no TxIns"
  distribCanonicalTxIn (IS idx) cardanoTxIn = case idx of
      IZ                               -> getCardanoTxIn cardanoTxIn
      IS IZ                            -> getCardanoTxIn cardanoTxIn
      IS (IS IZ)                       -> getCardanoTxIn cardanoTxIn
      IS (IS (IS IZ))                  -> getCardanoTxIn cardanoTxIn
      IS (IS (IS (IS IZ)))             -> getCardanoTxIn cardanoTxIn
      IS (IS (IS (IS (IS IZ))))        -> getCardanoTxIn cardanoTxIn
      IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

  encodeCanonicalTxIn   = Core.toEraCBOR @(ShelleyEra c) . getCardanoTxIn

  decodeCanonicalTxIn = CardanoTxIn <$> Core.fromEraCBOR @(ShelleyEra c)
