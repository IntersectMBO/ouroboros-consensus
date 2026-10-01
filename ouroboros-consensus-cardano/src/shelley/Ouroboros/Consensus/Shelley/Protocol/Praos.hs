{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Protocol.Praos
  ( ShelleyHeaderView (..)
  , praosHeaderToView
  , praosHeaderToShelleyView
  , leiosHeaderToView
  , leiosHeaderToShelleyView
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
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert
  ( OCert (ocertKESPeriod, ocertVkHot)
  )
import qualified Cardano.Protocol.TPraos.OCert as SL
import Cardano.Slotting.Block (BlockNo)
import Control.Monad.Except (Except)
import Data.Either (isRight)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Word (Word32, Word64)
import LeiosDemoTypes (EbAnnouncement)
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Praos.Common
  ( MaxMajorProtVer (MaxMajorProtVer)
  , fromCodecEbAnnouncement
  , toCodecEbAnnouncement
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
  , default_pHeaderLeiosContainsCert
  , default_pHeaderLeiosEbAnnouncement
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
-- The counterpart to 'HeaderView', which is what the 'ConsensusProtocol'
-- instance requires: this is what the @ProtocolHeaderSupports*@ classes
-- require, ie the header's identity and the body it claims. Keeping the two
-- separate keeps each narrow.
--
-- Uniform across the protocols, unlike 'HeaderView': a protocol whose header
-- has no Leios fields simply reports 'False' and 'SNothing' for the last two.
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

{-------------------------------------------------------------------------------
  Projections, two per header type

  Two projections per header rather than one because the views want different
  things: 'HeaderView' carries the signed body itself, and 'ShelleyHeaderView'
  the block number, body hash and body size, which that one does not.
-------------------------------------------------------------------------------}

praosHeaderToView :: Crypto c => PraosCodec.Header c -> PraosHeaderView c
praosHeaderToView hdr =
  HeaderView
    { hvPrevHash = PraosCodec.hbPrev body
    , hvVK = PraosCodec.hbVk body
    , hvVrfVK = PraosCodec.hbVrfVk body
    , hvVrfRes = PraosCodec.hbVrfRes body
    , hvOCert = PraosCodec.hbOCert body
    , hvSlotNo = PraosCodec.hbSlotNo body
    , hvSigned = body
    , hvSignature = PraosCodec.headerSig hdr
    }
 where
  body = PraosCodec.headerBody hdr

praosHeaderToShelleyView :: Crypto c => PraosCodec.Header c -> ShelleyHeaderView
praosHeaderToShelleyView hdr =
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

leiosHeaderToView :: Crypto c => LeiosCodec.Header c -> LeiosHeaderView c
leiosHeaderToView hdr =
  LeiosHeaderView
    { lhvBase =
        HeaderView
          { hvPrevHash = LeiosCodec.hbPrev body
          , hvVK = LeiosCodec.hbVk body
          , hvVrfVK = LeiosCodec.hbVrfVk body
          , hvVrfRes = LeiosCodec.hbVrfRes body
          , hvOCert = LeiosCodec.hbOCert body
          , hvSlotNo = LeiosCodec.hbSlotNo body
          , hvSigned = body
          , hvSignature = LeiosCodec.headerSig hdr
          }
    , lhvContainsCert = LeiosCodec.hbBlockBodyContainsLeiosCert body
    , lhvAnnouncement = fromCodecEbAnnouncement <$> LeiosCodec.hbEbAnnouncement body
    }
 where
  body = LeiosCodec.headerBody hdr

leiosHeaderToShelleyView :: Crypto c => LeiosCodec.Header c -> ShelleyHeaderView
leiosHeaderToShelleyView hdr =
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
  Instances, two of each
-------------------------------------------------------------------------------}

type instance ProtoCrypto (Praos c) = c

type instance ProtoCrypto (PraosWithLeios c) = c

type instance ShelleyProtocolHeader (Praos c) = PraosCodec.Header c

type instance ShelleyProtocolHeader (PraosWithLeios c) = LeiosCodec.Header c

-- | The envelope checks, over whichever 'ShelleyHeaderView' projection and
-- whichever ledger view this protocol has. Both instances below are this with
-- their own two arguments.
praosEnvelopeChecks ::
  (ShelleyProtocolHeader proto -> ShelleyHeaderView) ->
  MaxMajorProtVer ->
  PraosLedgerView ->
  ShelleyProtocolHeader proto ->
  Except EnvelopeError ()
praosEnvelopeChecks toShelleyView (MaxMajorProtVer maxpv) lv hdr =
  envelopeCheck maxpv ccd $
    EnvelopeHeaderView
      { ehvProtVer = m
      , ehvHeaderSize = shvSize shv
      , ehvBodySize = shvBodySize shv
      }
 where
  shv = toShelleyView hdr
  ProtVer m _ = plvProtocolVersion lv
  ccd =
    ChainChecksPParams
      { ccMaxBHSize = plvMaxHeaderSize lv
      , ccMaxBBSize = plvMaxBodySize lv
      , ccProtocolVersion = plvProtocolVersion lv
      }

instance PraosCrypto c => ProtocolHeaderSupportsEnvelope (Praos c) where
  pHeaderHash = shvHash . praosHeaderToShelleyView
  pHeaderPrevHash = hvPrevHash . praosHeaderToView
  pHeaderBodyHash = shvBodyHash . praosHeaderToShelleyView
  pHeaderSlot = hvSlotNo . praosHeaderToView
  pHeaderBlock = shvBlockNo . praosHeaderToShelleyView
  pHeaderSize = fromIntegral . shvSize . praosHeaderToShelleyView
  pHeaderBlockSize = fromIntegral . shvBodySize . praosHeaderToShelleyView
  pHeaderLeiosContainsCert = default_pHeaderLeiosContainsCert
  pHeaderLeiosEbAnnouncement = default_pHeaderLeiosEbAnnouncement

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    praosEnvelopeChecks
      praosHeaderToShelleyView
      (praosMaxMajorPV (praosParams cfg))
      lv
      hdr

instance PraosCrypto c => ProtocolHeaderSupportsEnvelope (PraosWithLeios c) where
  pHeaderHash = shvHash . leiosHeaderToShelleyView
  pHeaderPrevHash = hvPrevHash . lhvBase . leiosHeaderToView
  pHeaderBodyHash = shvBodyHash . leiosHeaderToShelleyView
  pHeaderSlot = hvSlotNo . lhvBase . leiosHeaderToView
  pHeaderBlock = shvBlockNo . leiosHeaderToShelleyView
  pHeaderSize = fromIntegral . shvSize . leiosHeaderToShelleyView
  pHeaderBlockSize = fromIntegral . shvBodySize . leiosHeaderToShelleyView
  pHeaderLeiosContainsCert = shvLeiosContainsCert . leiosHeaderToShelleyView
  pHeaderLeiosEbAnnouncement = shvLeiosEbAnnouncement . leiosHeaderToShelleyView

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    praosEnvelopeChecks
      leiosHeaderToShelleyView
      (praosMaxMajorPV (praosParams (praosConfigOfLeios cfg)))
      (pwlvBase lv)
      hdr

-- | Whether the KES signature over @body@ checks out.
--
-- @body@ is whatever the protocol signs; all this needs of it is a 'Signable'
-- dictionary, which each instance below has concretely.
praosVerifyHeaderIntegrity ::
  (Crypto c, KES.Signable (KES c) body) =>
  Word64 ->
  HeaderView body c ->
  Bool
praosVerifyHeaderIntegrity slotsPerKESPeriod hv =
  isRight $
    KES.verifySignedKES () ocertVkHot t (hvSigned hv) (hvSignature hv)
 where
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

-- | The Praos part of the header body, shared by both 'mkHeader's.
praosHeaderBody ::
  SlotNo ->
  BlockNo ->
  PrevHash ->
  Hash.Hash HASH EraIndependentBlockBody ->
  Int ->
  ProtVer ->
  PraosToSign c ->
  PraosCodec.HeaderBody c
praosHeaderBody
  slotNo
  blockNo
  prevHash
  bbHash
  sz
  protVer
  PraosToSign
    { praosToSignIssuerVK
    , praosToSignVrfVK
    , praosToSignVrfRes
    , praosToSignOCert
    } =
    PraosCodec.HeaderBody
      { PraosCodec.hbBlockNo = blockNo
      , PraosCodec.hbSlotNo = slotNo
      , PraosCodec.hbPrev = prevHash
      , PraosCodec.hbVk = praosToSignIssuerVK
      , PraosCodec.hbVrfVk = praosToSignVrfVK
      , PraosCodec.hbVrfRes = praosToSignVrfRes
      , PraosCodec.hbBodySize = fromIntegral sz
      , PraosCodec.hbBodyHash = bbHash
      , PraosCodec.hbOCert = praosToSignOCert
      , PraosCodec.hbProtVer = protVer
      }

instance PraosCrypto c => ProtocolHeaderSupportsKES (Praos c) where
  type HeaderExtras (Praos c) = ()

  configSlotsPerKESPeriod cfg = praosSlotsPerKESPeriod $ praosParams cfg

  verifyHeaderIntegrity slotsPerKESPeriod =
    praosVerifyHeaderIntegrity slotsPerKESPeriod . praosHeaderToView

  mkHeader hk cbl il slotNo blockNo prevHash bbHash sz protVer () = do
    PraosFields{praosSignature, praosToSign} <-
      forgePraosFields hk cbl il $
        praosHeaderBody slotNo blockNo prevHash bbHash sz protVer
    pure $ PraosCodec.Header praosToSign praosSignature

instance PraosCrypto c => ProtocolHeaderSupportsKES (PraosWithLeios c) where
  type HeaderExtras (PraosWithLeios c) = (Bool, StrictMaybe EbAnnouncement)

  configSlotsPerKESPeriod =
    praosSlotsPerKESPeriod . praosParams . praosConfigOfLeios

  verifyHeaderIntegrity slotsPerKESPeriod =
    praosVerifyHeaderIntegrity slotsPerKESPeriod . lhvBase . leiosHeaderToView

  mkHeader hk cbl il slotNo blockNo prevHash bbHash sz protVer (containsCert, mbAnn) = do
    PraosFields{praosSignature, praosToSign} <-
      forgePraosFields hk cbl il $ \ts ->
        extendHeaderBodyWithLeios
          (praosHeaderBody slotNo blockNo prevHash bbHash sz protVer ts)
          containsCert
          (toCodecEbAnnouncement <$> mbAnn)
    pure $ LeiosCodec.Header praosToSign praosSignature

  protocolStateLeiosInfo _ cs =
    case pwlsLeiosAnnouncement cs of
      SNothing -> Nothing
      SJust announced ->
        Just (announcedEb announced, praosStateLastSlot (pwlsPraos cs))

instance PraosCrypto c => ProtocolHeaderSupportsProtocol (Praos c) where
  type CannotForgeError (Praos c) = PraosCannotForge c

  protocolHeaderView = praosHeaderToView
  pHeaderIssuer = hvVK . praosHeaderToView
  pHeaderIssueNo = SL.ocertN . hvOCert . praosHeaderToView

  -- This is the "unified" VRF value, prior to range extension which yields e.g.
  -- the leader VRF value used for slot election.
  --
  -- In the future, we might want to use a dedicated range-extended VRF value
  -- here instead.
  pTieBreakVRFValue = certifiedOutput . hvVrfRes . praosHeaderToView

instance PraosCrypto c => ProtocolHeaderSupportsProtocol (PraosWithLeios c) where
  type CannotForgeError (PraosWithLeios c) = PraosCannotForge c

  protocolHeaderView = leiosHeaderToView
  pHeaderIssuer = hvVK . lhvBase . leiosHeaderToView
  pHeaderIssueNo = SL.ocertN . hvOCert . lhvBase . leiosHeaderToView
  pTieBreakVRFValue = certifiedOutput . hvVrfRes . lhvBase . leiosHeaderToView

type instance Signed (PraosCodec.Header c) = PraosCodec.HeaderBody c

instance PraosCrypto c => SignedHeader (PraosCodec.Header c) where
  headerSigned = PraosCodec.headerBody

type instance Signed (LeiosCodec.Header c) = LeiosCodec.HeaderBody c

instance PraosCrypto c => SignedHeader (LeiosCodec.Header c) where
  headerSigned = LeiosCodec.headerBody

instance PraosCrypto c => ShelleyProtocol (Praos c)

instance PraosCrypto c => ShelleyProtocol (PraosWithLeios c)
