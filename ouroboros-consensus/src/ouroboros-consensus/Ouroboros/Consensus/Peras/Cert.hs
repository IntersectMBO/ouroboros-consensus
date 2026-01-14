{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Cert
  ( PerasCert (..)
  ) where

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (HeaderHash, Point)
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
import Ouroboros.Consensus.Util (ShowProxy (..))

data PerasCert blk = PerasCert
  { pcCertRound :: PerasRoundNo
  , pcCertBoostedBlock :: Point blk
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

instance Serialise (HeaderHash blk) => Serialise (PerasCert blk) where
  encode PerasCert{pcCertRound, pcCertBoostedBlock} =
    encodeListLen 2
      <> encode pcCertRound
      <> encode pcCertBoostedBlock
  decode = do
    decodeListLenOf 2
    pcCertRound <- decode
    pcCertBoostedBlock <- decode
    pure $ PerasCert{pcCertRound, pcCertBoostedBlock}

instance ShowProxy blk => ShowProxy (PerasCert blk) where
  showProxy _ = "PerasCert " <> showProxy (Proxy @blk)
