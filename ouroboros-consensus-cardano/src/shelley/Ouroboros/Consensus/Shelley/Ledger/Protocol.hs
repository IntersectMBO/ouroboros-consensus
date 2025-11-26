{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Protocol () where

import qualified Cardano.Ledger.Shelley.API as SL
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.Signed
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Config (BlockConfig (..))
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ShelleyProtocolHeader
  , pHeaderIssueNo
  , pHeaderIssuer
  , pTieBreakVRFValue
  , protocolHeaderView
  )

{-------------------------------------------------------------------------------
  Support for Transitional Praos consensus algorithm
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => BlockSupportsProtocol (ShelleyBlock proto era) where
  validateView _cfg = protocolHeaderView @proto . shelleyHeaderRaw

  tiebreakerView _ hdr@(ShelleyHeader shdr _) =
    PraosTiebreakerView
      { ptvSlotNo = blockSlot hdr
      , ptvIssuer = hdrIssuer
      , ptvIssueNo = pHeaderIssueNo shdr
      , ptvTieBreakVRF = pTieBreakVRFValue shdr
      }
   where
    hdrIssuer :: SL.VKey SL.BlockIssuer
    hdrIssuer = pHeaderIssuer shdr

  projectChainOrderConfig = shelleyVRFTiebreakerFlavor

-- TODO correct place for these two?
type instance
  Signed (Header (ShelleyBlock proto era)) =
    Signed (ShelleyProtocolHeader proto)

instance
  SignedHeader (ShelleyProtocolHeader proto) =>
  SignedHeader (Header (ShelleyBlock proto era))
  where
  headerSigned = headerSigned . shelleyHeaderRaw
