{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Protocol.Praos
  ( ShelleyHeaderView (..)
  , HasPraosExtensionHeader (..)
  ) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.VRF (certifiedOutput)
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer))
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Hashes (EraIndependentBlockBody, HASH)
import Cardano.Ledger.Slot (SlotNo (unSlotNo))
import Cardano.Protocol.Crypto (Crypto, KES)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import qualified Cardano.Protocol.Praos.BlockHeader as PraosCodec
import Cardano.Protocol.TPraos.OCert
  ( OCert (ocertKESPeriod, ocertVkHot)
  )
import qualified Cardano.Protocol.TPraos.OCert as SL
import Cardano.Slotting.Block (BlockNo)
import Data.Either (isRight)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word32)
import LeiosDemoTypes (EbAnnouncement)
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Praos.Common
  ( KnownPraosExtension (singPraosExtension)
  , MaxMajorProtVer (MaxMajorProtVer)
  , PraosExtensionHasLeios
  , SingPraosExtension (..)
  , StrictMaybeLeios (..)
  , fromCodecEbAnnouncement
  )
import Ouroboros.Consensus.Protocol.Praos.Views
import Ouroboros.Consensus.Protocol.Signed
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtoCrypto
  , ProtocolHeaderSupportsEnvelope (..)
  , ProtocolHeaderSupportsKES (..)
  , ProtocolHeaderSupportsProtocol (..)
  , ShelleyHash (ShelleyHash)
  , ShelleyProtocol
  , ShelleyProtocolHeader
  )
import Ouroboros.Consensus.Shelley.Protocol.EnvelopeChecks
  ( EnvelopeError
  , EnvelopeHeaderView (..)
  , envelopeCheck
  )

{-------------------------------------------------------------------------------
  The header as the Shelley block layer needs it
-------------------------------------------------------------------------------}

-- | View of the block header required by the Shelley block layer.
--
-- The counterpart to 'BaseHeaderView', which is what the 'ConsensusProtocol'
-- instance requires: this is what the @ProtocolHeaderSupports*@ classes require,
-- ie the header's identity and the body it claims. Keeping the two separate
-- keeps each narrow, and lets the extension-dependence of the upstream header
-- types be discharged in one place per view.
--
-- Unindexed by @pext@, unlike 'BaseHeaderView': every field here is uniform
-- across the extensions, an extension without Leios simply reporting 'False'
-- and 'SNothing' for the last two.
data ShelleyHeaderView = ShelleyHeaderView
  { shvHash :: !ShelleyHash
  , shvSize :: !Int
  -- ^ Over the bytes the header was decoded from; see 'headerSize'.
  , shvBlockNo :: !BlockNo
  , shvBodyHash :: !(Hash.Hash HASH EraIndependentBlockBody)
  , shvBodySize :: !Word32
  , shvProtVer :: !ProtVer
  , shvLeiosContainsCert :: !Bool
  -- ^ Whether the block's body carries a Leios certificate, ie whether it is a
  -- CertRB.
  , shvLeiosEbAnnouncement :: !(StrictMaybe EbAnnouncement)
  -- ^ The endorser block this header announces.
  }

-- | Project an extension's header into the two views that read it.
--
-- Two projections rather than one because the views want different things:
-- 'BaseHeaderView' carries the signed body itself, and 'ShelleyHeaderView' the
-- block number, body hash and body size, which that one does not.
class HasPraosExtensionHeader pext where
  headerToView ::
    Crypto c => ShelleyProtocolHeader (BasePraos pext c) -> BaseHeaderView pext c
  headerToShelleyView ::
    Crypto c => ShelleyProtocolHeader (BasePraos pext c) -> ShelleyHeaderView

instance HasPraosExtensionHeader PextNone where
  headerToView hdr =
    HeaderView
      { hvPrevHash = PraosCodec.hbPrev body
      , hvVK = PraosCodec.hbVk body
      , hvVrfVK = PraosCodec.hbVrfVk body
      , hvVrfRes = PraosCodec.hbVrfRes body
      , hvOCert = PraosCodec.hbOCert body
      , hvSlotNo = PraosCodec.hbSlotNo body
      , hvLeios = SNothingLeios
      , hvSigned = body
      , hvSignature = PraosCodec.headerSig hdr
      }
   where
    body = PraosCodec.headerBody hdr

  headerToShelleyView hdr =
    ShelleyHeaderView
      { shvHash = ShelleyHash $ PraosCodec.headerHash hdr
      , shvSize = PraosCodec.headerSize hdr
      , shvBlockNo = PraosCodec.hbBlockNo body
      , shvBodyHash = PraosCodec.hbBodyHash body
      , shvBodySize = PraosCodec.hbBodySize body
      , shvProtVer = PraosCodec.hbProtVer body
      , shvLeiosContainsCert = False
      , shvLeiosEbAnnouncement = SNothing
      }
   where
    body = PraosCodec.headerBody hdr

instance HasPraosExtensionHeader PextLeios where
  headerToView hdr =
    HeaderView
      { hvPrevHash = LeiosCodec.hbPrev body
      , hvVK = LeiosCodec.hbVk body
      , hvVrfVK = LeiosCodec.hbVrfVk body
      , hvVrfRes = LeiosCodec.hbVrfRes body
      , hvOCert = LeiosCodec.hbOCert body
      , hvSlotNo = LeiosCodec.hbSlotNo body
      , hvLeios =
          SJustLeios
            ( LeiosCodec.hbBlockBodyContainsLeiosCert body
            , fromCodecEbAnnouncement <$> LeiosCodec.hbEbAnnouncement body
            )
      , hvSigned = body
      , hvSignature = LeiosCodec.headerSig hdr
      }
   where
    body = LeiosCodec.headerBody hdr

  headerToShelleyView hdr =
    ShelleyHeaderView
      { shvHash = ShelleyHash $ LeiosCodec.headerHash hdr
      , shvSize = LeiosCodec.headerSize hdr
      , shvBlockNo = LeiosCodec.hbBlockNo body
      , shvBodyHash = LeiosCodec.hbBodyHash body
      , shvBodySize = LeiosCodec.hbBodySize body
      , shvProtVer = LeiosCodec.hbProtVer body
      , shvLeiosContainsCert = LeiosCodec.hbBlockBodyContainsLeiosCert body
      , shvLeiosEbAnnouncement =
          fromCodecEbAnnouncement <$> LeiosCodec.hbEbAnnouncement body
      }
   where
    body = LeiosCodec.headerBody hdr

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

type instance ProtoCrypto (BasePraos pext c) = c

type instance ShelleyProtocolHeader (BasePraos PextNone c) = PraosCodec.Header c

type instance ShelleyProtocolHeader (BasePraos PextLeios c) = LeiosCodec.Header c

instance
  ( PraosCrypto c
  , HasPraosExtensionHeader pext
  ) =>
  ProtocolHeaderSupportsEnvelope (BasePraos pext c)
  where
  pHeaderHash = shvHash . headerToShelleyView @pext
  pHeaderPrevHash = hvPrevHash . headerToView @pext
  pHeaderBodyHash = shvBodyHash . headerToShelleyView @pext
  pHeaderSlot = hvSlotNo . headerToView @pext
  pHeaderBlock = shvBlockNo . headerToShelleyView @pext
  pHeaderSize = fromIntegral . shvSize . headerToShelleyView @pext
  pHeaderBlockSize = fromIntegral . shvBodySize . headerToShelleyView @pext
  pHeaderLeiosContainsCert = shvLeiosContainsCert . headerToShelleyView @pext
  pHeaderLeiosEbAnnouncement = shvLeiosEbAnnouncement . headerToShelleyView @pext

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    envelopeCheck maxpv ccd $
      EnvelopeHeaderView
        { ehvProtVer = m
        , ehvHeaderSize = shvSize shv
        , ehvBodySize = shvBodySize shv
        }
   where
    shv = headerToShelleyView @pext hdr
    MaxMajorProtVer maxpv = praosMaxMajorPV (praosParams cfg)
    ProtVer m _ = plvProtocolVersion lv
    ccd =
      ChainChecksPParams
        { ccMaxBHSize = plvMaxHeaderSize lv
        , ccMaxBBSize = plvMaxBodySize lv
        , ccProtocolVersion = plvProtocolVersion lv
        }

instance
  ( PraosCrypto c
  , KnownPraosExtension pext
  , HasPraosExtensionHeader pext
  ) =>
  ProtocolHeaderSupportsKES (BasePraos pext c)
  where
  type ProtoHasLeios (BasePraos pext c) = PraosExtensionHasLeios pext

  configSlotsPerKESPeriod cfg = praosSlotsPerKESPeriod $ praosParams cfg

  verifyHeaderIntegrity slotsPerKESPeriod hdr =
    withSignableDict $
      isRight $
        KES.verifySignedKES () ocertVkHot t (hvSigned hv) (hvSignature hv)
   where
    hv = headerToView @pext hdr

    -- Each branch refines @pext@, so the body type reduces to one 'PraosCrypto'
    -- covers; it cannot be discharged for an abstract @pext@.
    withSignableDict :: (KES.Signable (KES c) (BaseHeaderBody pext c) => r) -> r
    withSignableDict k = case singPraosExtension (Proxy @pext) of
      SingPextNone -> k
      SingPextLeios -> k

    SL.OCert
      { ocertVkHot
      , ocertKESPeriod = SL.KESPeriod startOfKesPeriod
      } = hvOCert hv

    currentKesPeriod =
      fromIntegral $
        unSlotNo (hvSlotNo hv) `div` slotsPerKESPeriod

    t
      | currentKesPeriod >= startOfKesPeriod =
          currentKesPeriod - startOfKesPeriod
      | otherwise =
          0

  -- TODO
  mkHeader = error "TODO mkHeader for BasePraos"

  protocolStateLeiosInfo _ cs =
    case praosStateLeiosAnnouncement cs of
      SNothingLeios -> Nothing
      SJustLeios SNothing -> Nothing
      SJustLeios (SJust ann) -> Just (ann, praosStateLastSlot cs)

instance
  ( PraosCrypto c
  , HasPraosExtensionHeader pext
  ) =>
  ProtocolHeaderSupportsProtocol (BasePraos pext c)
  where
  type CannotForgeError (BasePraos pext c) = PraosCannotForge c

  protocolHeaderView = headerToView @pext
  pHeaderIssuer = hvVK . headerToView @pext
  pHeaderIssueNo = SL.ocertN . hvOCert . headerToView @pext

  -- This is the "unified" VRF value, prior to range extension which yields e.g.
  -- the leader VRF value used for slot election.
  --
  -- In the future, we might want to use a dedicated range-extended VRF value
  -- here instead.
  pTieBreakVRFValue = certifiedOutput . hvVrfRes . headerToView @pext

type instance Signed (PraosCodec.Header c) = PraosCodec.HeaderBody c

instance PraosCrypto c => SignedHeader (PraosCodec.Header c) where
  headerSigned = PraosCodec.headerBody

type instance Signed (LeiosCodec.Header c) = LeiosCodec.HeaderBody c

instance PraosCrypto c => SignedHeader (LeiosCodec.Header c) where
  headerSigned = LeiosCodec.headerBody

-- Concrete per extension, rather than one instance over @pext@: the
-- superclasses need 'Typeable' and 'SignedHeader' of the header type, which
-- only reduce once the extension is known.
instance PraosCrypto c => ShelleyProtocol (BasePraos PextNone c)

instance PraosCrypto c => ShelleyProtocol (BasePraos PextLeios c)
