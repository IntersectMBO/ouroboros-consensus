{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Cardano.CanonicalTxIn () where

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import           Data.SOP.Index
import           Data.Void (Void, absurd)
import           Legacy.Cardano.Block
import           Legacy.Cardano.CanHardFork
import           NoThunks.Class
import           Ouroboros.Consensus.HardFork.Combinator.Ledger

instance LegacyCardanoHardForkConstraints c
      => HasCanonicalTxIn (LegacyCardanoEras c) where
  newtype instance CanonicalTxIn (LegacyCardanoEras c) = LegacyCardanoTxIn {
      getLegacyCardanoTxIn :: Void
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NoThunks, FromCBOR, ToCBOR)

  injectCanonicalTxIn IZ  byronTxIn   = absurd byronTxIn
  injectCanonicalTxIn (IS idx) shelleyTxIn = case idx of
      IZ                               -> absurd shelleyTxIn
      IS IZ                            -> absurd shelleyTxIn
      IS (IS IZ)                       -> absurd shelleyTxIn
      IS (IS (IS IZ))                  -> absurd shelleyTxIn
      IS (IS (IS (IS IZ)))             -> absurd shelleyTxIn
      IS (IS (IS (IS (IS IZ))))        -> absurd shelleyTxIn
      IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

  distribCanonicalTxIn _ key = absurd $ getLegacyCardanoTxIn key

  encodeCanonicalTxIn = toCBOR

  decodeCanonicalTxIn = fromCBOR
