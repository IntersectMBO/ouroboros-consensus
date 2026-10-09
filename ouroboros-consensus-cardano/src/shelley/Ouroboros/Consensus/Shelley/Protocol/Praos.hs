{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Protocol.Praos () where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.VRF (certifiedOutput)
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer), StrictMaybe)
import Cardano.Ledger.Binary (getVersion32)
import Cardano.Ledger.Block (BlockHeaderVersionInfo (..), EbReferencesAnnouncement)
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Hashes (EraIndependentBlockBody, HASH)
import Cardano.Ledger.Slot (SlotNo (unSlotNo))
import Cardano.Protocol.Crypto (Crypto, KES)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import Cardano.Protocol.Praos.BlockHeader
  ( Header (..)
  , HeaderBody (..)
  , headerHash
  , headerSize
  )
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert
  ( OCert (ocertKESPeriod, ocertVkHot)
  )
import qualified Cardano.Protocol.TPraos.OCert as SL
import Cardano.Slotting.Block (BlockNo)
import Control.Monad.Except (Except)
import Data.Either (isRight)
import Data.Word (Word32, Word64)
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Praos.Common
  ( MaxMajorProtVer (MaxMajorProtVer)
  , PraosCanBeLeader
  )
import Ouroboros.Consensus.Protocol.Praos.Views
import Ouroboros.Consensus.Protocol.Praos2
  ( ConsensusConfig (..)
  , LeiosCrypto
  , LeiosOnly (..)
  , Praos2
  )
import Ouroboros.Consensus.Protocol.Signed
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtoCrypto
  , ProtocolHeaderSupportsEnvelope (..)
  , ProtocolHeaderSupportsKES (..)
  , ProtocolHeaderSupportsProtocol (..)
  , ShelleyHash (ShelleyHash)
  , ShelleyProtocol
  )
import Ouroboros.Consensus.Shelley.Protocol.EnvelopeChecks
  ( EnvelopeError
  , EnvelopeHeaderView (..)
  , envelopeCheck
  )

type instance ProtoCrypto (Praos c) = c

instance PraosCrypto c => ProtocolHeaderSupportsEnvelope (Praos c) where
  pHeaderHash hdr = ShelleyHash $ headerHash hdr
  pHeaderPrevHash (Header body _) = hbPrev body
  pHeaderBodyHash (Header body _) = hbBodyHash body
  pHeaderSlot (Header body _) = hbSlotNo body
  pHeaderBlock (Header body _) = hbBlockNo body
  pHeaderSize hdr = fromIntegral $ headerSize hdr
  pHeaderBlockSize (Header body _) = fromIntegral $ hbBodySize body

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    envelopeChecksPolyPraos cfg lv (headerSize hdr) (hbBodySize body)
   where
    Header body _ = hdr

-- | 'envelopeChecks' for every Praos.
envelopeChecksPolyPraos ::
  ConsensusConfig (Praos c) ->
  PolyPraosLedgerView proto ->
  -- | Size of the header
  Int ->
  -- | Size of the block body
  Word32 ->
  Except EnvelopeError ()
envelopeChecksPolyPraos cfg lv hdrSize bodySize =
  envelopeCheck maxpv ccd $
    EnvelopeHeaderView
      { ehvProtVer = m
      , ehvHeaderSize = hdrSize
      , ehvBodySize = bodySize
      }
 where
  MaxMajorProtVer maxpv = praosMaxMajorPV (praosParams cfg)
  ProtVer m _ = plvProtocolVersion lv
  ccd =
    ChainChecksPParams
      { ccMaxBHSize = plvMaxHeaderSize lv
      , ccMaxBBSize = plvMaxBodySize lv
      , ccProtocolVersion = plvProtocolVersion lv
      }

instance PraosCrypto c => ProtocolHeaderSupportsKES (Praos c) where
  configSlotsPerKESPeriod cfg = praosSlotsPerKESPeriod $ praosParams cfg
  verifyHeaderIntegrity slotsPerKESPeriod =
    verifyHeaderIntegrityPolyPraos slotsPerKESPeriod . protocolHeaderView
  mkHeader _ hk cbl il slotNo blockNo prevHash bbHash sz protVer (PraosLacksLeios ()) =
    mkHeaderPolyPraos hk cbl il slotNo blockNo prevHash bbHash sz protVer id Header

-- | 'verifyHeaderIntegrity' for every Praos.
verifyHeaderIntegrityPolyPraos ::
  PolyPraosCrypto proto c =>
  Word64 ->
  PolyPraosValidateView proto c ->
  Bool
verifyHeaderIntegrityPolyPraos slotsPerKESPeriod hv =
  isRight $ KES.verifySignedKES () ocertVkHot t (hvSigned hv) (hvSignature hv)
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

-- | 'mkHeader' for every Praos.
--
-- The fields the protocols share are filled in here; the caller says how its own
-- header body extends that, since only it knows the extra fields, and how to
-- assemble the signed header.
mkHeaderPolyPraos ::
  (Crypto c, KES.Signable (KES c) body, Monad m) =>
  HotKey c m ->
  PraosCanBeLeader c ->
  PraosIsLeader c ->
  SlotNo ->
  BlockNo ->
  PrevHash ->
  Hash.Hash HASH EraIndependentBlockBody ->
  Int ->
  ProtVer ->
  -- | How this protocol's header body extends the shared one
  (HeaderBody c -> body) ->
  -- | How this protocol assembles the signed header
  (body -> KES.SignedKES (KES c) body -> hdr) ->
  m hdr
mkHeaderPolyPraos hk cbl il slotNo blockNo prevHash bbHash sz protVer extend mkHdr = do
  PraosFields{praosSignature, praosToSign} <- forgePraosFields hk cbl il mkBhBodyBytes
  pure $ mkHdr praosToSign praosSignature
 where
  mkBhBodyBytes
    PraosToSign
      { praosToSignIssuerVK
      , praosToSignVrfVK
      , praosToSignVrfRes
      , praosToSignOCert
      } =
      extend $
        HeaderBody
          { hbBlockNo = blockNo
          , hbSlotNo = slotNo
          , hbPrev = prevHash
          , hbVk = praosToSignIssuerVK
          , hbVrfVk = praosToSignVrfVK
          , hbVrfRes = praosToSignVrfRes
          , hbBodySize = fromIntegral sz
          , hbBodyHash = bbHash
          , hbOCert = praosToSignOCert
          , hbProtVer = protVer
          }

instance PraosCrypto c => ProtocolHeaderSupportsProtocol (Praos c) where
  type CannotForgeError (Praos c) = PraosCannotForge c
  protocolHeaderView Header{headerBody, headerSig} =
    HeaderView
      { hvPrevHash = hbPrev headerBody
      , hvVK = hbVk headerBody
      , hvVrfVK = hbVrfVk headerBody
      , hvVrfRes = hbVrfRes headerBody
      , hvOCert = hbOCert headerBody
      , hvSlotNo = hbSlotNo headerBody
      , hvLeios = PraosLacksLeios ()
      , hvSigned = headerBody
      , hvSignature = headerSig
      }
  pHeaderIssuer = hbVk . headerBody
  pHeaderIssueNo = SL.ocertN . hbOCert . headerBody

  -- This is the "unified" VRF value, prior to range extension which yields e.g.
  -- the leader VRF value used for slot election.
  --
  -- In the future, we might want to use a dedicated range-extended VRF value
  -- here instead.
  pTieBreakVRFValue = certifiedOutput . hbVrfRes . headerBody

instance PraosCrypto c => SignedHeader (Header c) where
  headerSigned = headerBody

instance PraosCrypto c => ShelleyProtocol (Praos c)

{-------------------------------------------------------------------------------
  Praos2
-------------------------------------------------------------------------------}

type instance ProtoCrypto (Praos2 c) = c

instance LeiosCrypto c => ProtocolHeaderSupportsEnvelope (Praos2 c) where
  pHeaderHash hdr = ShelleyHash $ LeiosCodec.headerHash hdr
  pHeaderPrevHash = LeiosCodec.hbPrev . LeiosCodec.headerBody
  pHeaderBodyHash = LeiosCodec.hbBodyHash . LeiosCodec.headerBody
  pHeaderSlot = LeiosCodec.hbSlotNo . LeiosCodec.headerBody
  pHeaderBlock = LeiosCodec.hbBlockNo . LeiosCodec.headerBody
  pHeaderSize hdr = fromIntegral $ LeiosCodec.headerSize hdr
  pHeaderBlockSize = fromIntegral . LeiosCodec.hbBodySize . LeiosCodec.headerBody

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    envelopeChecksPolyPraos
      (leiosPraosConfig cfg)
      lv
      (LeiosCodec.headerSize hdr)
      (LeiosCodec.hbBodySize (LeiosCodec.headerBody hdr))

instance LeiosCrypto c => ProtocolHeaderSupportsKES (Praos2 c) where
  configSlotsPerKESPeriod cfg = praosSlotsPerKESPeriod $ praosParams $ leiosPraosConfig cfg
  verifyHeaderIntegrity slotsPerKESPeriod =
    verifyHeaderIntegrityPolyPraos slotsPerKESPeriod . protocolHeaderView
  mkHeader
    era
    hk
    cbl
    il
    slotNo
    blockNo
    prevHash
    bbHash
    sz
    protVer
    (Praos2HasLeios (containsCert, mbAnn)) =
      mkHeaderPolyPraos
        hk
        cbl
        il
        slotNo
        blockNo
        prevHash
        bbHash
        sz
        protVer
        (\pb -> extendHeaderBodyWithLeios pb containsCert mbAnn)
        (LeiosCodec.mkHeader era)

-- | The Leios header body is the Praos one plus the Leios fields.
--
-- The version info has the protocol version's wire format: the highest
-- supported major version, and the self-reported software tag.
extendHeaderBodyWithLeios ::
  HeaderBody c ->
  -- | Whether the block body carries a Leios certificate
  Bool ->
  StrictMaybe EbReferencesAnnouncement ->
  LeiosCodec.HeaderBody c
extendHeaderBodyWithLeios pb containsCert ann =
  LeiosCodec.HeaderBody
    { LeiosCodec.hbBlockNo = hbBlockNo pb
    , LeiosCodec.hbSlotNo = hbSlotNo pb
    , LeiosCodec.hbPrev = hbPrev pb
    , LeiosCodec.hbVk = hbVk pb
    , LeiosCodec.hbVrfVk = hbVrfVk pb
    , LeiosCodec.hbVrfRes = hbVrfRes pb
    , LeiosCodec.hbBodySize = hbBodySize pb
    , LeiosCodec.hbBodyHash = hbBodyHash pb
    , LeiosCodec.hbOCert = hbOCert pb
    , LeiosCodec.hbVersionInfo = BlockHeaderVersionInfo (getVersion32 major) minor
    , LeiosCodec.hbBlockBodyContainsLeiosCert = containsCert
    , LeiosCodec.hbEbReferencesAnnouncement = ann
    }
 where
  ProtVer major minor = hbProtVer pb

instance LeiosCrypto c => ProtocolHeaderSupportsProtocol (Praos2 c) where
  type CannotForgeError (Praos2 c) = PraosCannotForge c
  protocolHeaderView header =
    HeaderView
      { hvPrevHash = LeiosCodec.hbPrev body
      , hvVK = LeiosCodec.hbVk body
      , hvVrfVK = LeiosCodec.hbVrfVk body
      , hvVrfRes = LeiosCodec.hbVrfRes body
      , hvOCert = LeiosCodec.hbOCert body
      , hvSlotNo = LeiosCodec.hbSlotNo body
      , hvLeios =
          Praos2HasLeios
            ( LeiosCodec.hbBlockBodyContainsLeiosCert body
            , LeiosCodec.hbEbReferencesAnnouncement body
            )
      , hvSigned = body
      , hvSignature = LeiosCodec.headerSig header
      }
   where
    body = LeiosCodec.headerBody header
  pHeaderIssuer = LeiosCodec.hbVk . LeiosCodec.headerBody
  pHeaderIssueNo = SL.ocertN . LeiosCodec.hbOCert . LeiosCodec.headerBody
  pTieBreakVRFValue = certifiedOutput . LeiosCodec.hbVrfRes . LeiosCodec.headerBody

instance LeiosCrypto c => SignedHeader (LeiosCodec.Header c) where
  headerSigned = LeiosCodec.headerBody

instance LeiosCrypto c => ShelleyProtocol (Praos2 c)
