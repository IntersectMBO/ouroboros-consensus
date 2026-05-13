{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.State.Instances
  ( -- * Serialisation support
    decodeCurrent
  , decodePast
  , encodeCurrent
  , encodePast
  ) where

import Cardano.Binary (enforceSize)
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Codec.Serialise
import Data.Coerce
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.HardFork.Combinator.State.Lift
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Prelude hiding (sequence)

{-------------------------------------------------------------------------------
  SOP class instances

  These are convenient, allowing us to treat the 'HardForkState' just like any
  other SOP type; in particular, they deal with lifting functions to 'Current'.
-------------------------------------------------------------------------------}

type instance Prod HardForkState = NP
type instance SListIN HardForkState = SListI
type instance AllN HardForkState c = All c
type instance CollapseTo HardForkState a = a

instance HAp HardForkState where
  hap np (HardForkState st) =
    HardForkState $
      hap (map_NP' (Fn . lift . apFn) np) st

instance HSequence HardForkState where
  hctraverse' = \p f (HardForkState st) ->
    HardForkState
      <$> hctraverse' p (liftM f) st
  htraverse' = hctraverse' (Proxy @Top)
  hsequence' = htraverse' unComp

instance HCollapse HardForkState where
  hcollapse = hcollapse . hmap currentState . Telescope.tip . getHardForkState

instance HTrans HardForkState HardForkState where
  htrans p t (HardForkState st) =
    HardForkState $
      htrans p (\(Current b fx) -> Current b $ t fx) st

  hcoerce ::
    forall f g xs ys.
    AllZipN (Prod HardForkState) (LiftedCoercible f g) xs ys =>
    HardForkState f xs ->
    HardForkState g ys
  hcoerce (HardForkState st) =
    HardForkState $
      htrans
        (Proxy @(LiftedCoercible f g))
        (\(Current b fx) -> Current b $ coerce fx)
        st

type instance Same HardForkState = HardForkState

{-------------------------------------------------------------------------------
  Eq, Show, NoThunks
-------------------------------------------------------------------------------}

deriving newtype instance
  (All (Compose Show (K Past)) xs, All (Compose Show (Current f)) xs) => Show (HardForkState f xs)
deriving newtype instance
  (All (Compose Eq (K Past)) xs, All (Compose Eq (Current f)) xs) => Eq (HardForkState f xs)
deriving newtype instance
  (All (Compose NoThunks (K Past)) xs, All (Compose NoThunks (Current f)) xs) =>
  NoThunks (HardForkState f xs)
deriving instance Show (f blk) => Show (Current f blk)
deriving instance Eq (f blk) => Eq (Current f blk)
deriving instance NoThunks (f blk) => NoThunks (Current f blk)

{-------------------------------------------------------------------------------
  Serialisation

  The 'Serialise' instances are primarily useful for the tests, but the general
  encoders/decoders are used by the HFC to store the ledger state.
-------------------------------------------------------------------------------}

encodeCurrent :: (f blk -> Encoding) -> Current f blk -> Encoding
encodeCurrent f Current{..} =
  mconcat
    [ encodeListLen 2
    , encode currentStart
    , f currentState
    ]

decodeCurrent :: Decoder s (f blk) -> Decoder s (Current f blk)
decodeCurrent f = do
  enforceSize "decodeCurrent" 2
  currentStart <- decode
  currentState <- f
  return Current{..}

encodePast :: Past -> Encoding
encodePast Past{..} =
  mconcat
    [ encodeListLen 2
    , encode pastStart
    , encode pastEnd
    ]

decodePast :: Decoder s Past
decodePast = do
  enforceSize "decodePast" 2
  pastStart <- decode
  pastEnd <- decode
  return Past{..}

instance Serialise (f blk) => Serialise (Current f blk) where
  encode = encodeCurrent encode
  decode = decodeCurrent decode

instance Serialise Past where
  encode = encodePast
  decode = decodePast
