{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Which part of each Praos header is the signed one.
--
-- Orphans by necessity: 'Signed' is declared in @ouroboros-consensus@ and the
-- header types in @cardano-protocol@, so no module owning either can host
-- these. They are alone here so that the pragma covers nothing else.
module Ouroboros.Consensus.Protocol.Praos.Orphans () where

import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import qualified Cardano.Protocol.Praos.BlockHeader as PraosCodec
import Ouroboros.Consensus.Protocol.Signed (Signed)

type instance Signed (PraosCodec.Header c) = PraosCodec.HeaderBody c

type instance Signed (LeiosCodec.Header c) = LeiosCodec.HeaderBody c
