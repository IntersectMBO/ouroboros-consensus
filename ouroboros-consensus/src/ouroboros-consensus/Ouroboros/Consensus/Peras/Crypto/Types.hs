{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Common crypto types used in both Peras votes and certificates
module Ouroboros.Consensus.Peras.Crypto.Types
  ( -- * Common crypto types for Peras
    PerasBoostedBlock (..)
  , PerasSeatIndex (..)
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Data.Function (on)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Ouroboros.Consensus.Block
  ( ConvertRawHash
  , HeaderHash
  , decodeRawHash
  , encodeRawHash
  )

-- * Common crypto types for Peras

-- | The hash of the block being voted for in a Peras election
newtype PerasBoostedBlock blk
  = PerasBoostedBlock
  { unPerasBoostedBlock :: HeaderHash blk
  }

instance Eq (HeaderHash blk) => Eq (PerasBoostedBlock blk) where
  (==) = (==) `on` unPerasBoostedBlock

instance Show (HeaderHash blk) => Show (PerasBoostedBlock blk) where
  show (PerasBoostedBlock bh) = "PerasBoostedBlock(" <> show bh <> ")"

instance
  (Typeable blk, ConvertRawHash blk) =>
  FromCBOR (PerasBoostedBlock blk)
  where
  fromCBOR =
    PerasBoostedBlock
      <$> decodeRawHash (Proxy @blk)

instance
  (Typeable blk, ConvertRawHash blk) =>
  ToCBOR (PerasBoostedBlock blk)
  where
  toCBOR (PerasBoostedBlock bh) =
    encodeRawHash (Proxy @blk) bh

-- | Seat index in the voting committee used for Peras
newtype PerasSeatIndex
  = PerasSeatIndex
  { unPerasSeatIndex :: Word16
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromCBOR, ToCBOR, Enum, Bounded)
