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

module Ouroboros.Consensus.Shelley.Protocol.Praos () where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.VRF (certifiedOutput)
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer))
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Hashes (EraIndependentBlockBody, HASH)
import Cardano.Ledger.Slot (SlotNo (unSlotNo))
import Cardano.Protocol.Crypto (Crypto)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import qualified Cardano.Protocol.Praos.BlockHeader as PraosCodec
import Cardano.Protocol.TPraos.BlockHeader (PrevHash)
import Cardano.Protocol.TPraos.OCert
  ( OCert (ocertKESPeriod, ocertVkHot)
  )
import qualified Cardano.Protocol.TPraos.OCert as SL
import Cardano.Slotting.Block (BlockNo)
import Data.Either (isRight)
import Data.Maybe.Strict (StrictMaybe (..))
import Control.Monad.Except (Except)
import Data.Word (Word32, Word64)
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Leios (ConsensusConfig (..), EitherLeiosF (..), LeiosCrypto, PraosWithLeios)
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
  , default_pHeaderLeiosContainsCert
  , default_pHeaderLeiosEbAnnouncement
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

{-------------------------------------------------------------------------------
  The header as the protocol reads it
-------------------------------------------------------------------------------}

protocolHeaderView_Praos ::
  Crypto c =>
  PraosCodec.Header c -> BasePraosValidateView (Praos c) c
protocolHeaderView_Praos hdr =
  HeaderView
    { hvPrevHash = PraosCodec.hbPrev body
    , hvVK = PraosCodec.hbVk body
    , hvVrfVK = PraosCodec.hbVrfVk body
    , hvVrfRes = PraosCodec.hbVrfRes body
    , hvOCert = PraosCodec.hbOCert body
    , hvSlotNo = PraosCodec.hbSlotNo body
    , hvLeios = PraosLeiosLeft ()
    , hvSigned = body
    , hvSignature = PraosCodec.headerSig hdr
    }
 where
  body = PraosCodec.headerBody hdr

protocolHeaderView_PraosWithLeios ::
  Crypto c =>
  LeiosCodec.Header c -> BasePraosValidateView (PraosWithLeios c) c
protocolHeaderView_PraosWithLeios hdr =
  HeaderView
    { hvPrevHash = LeiosCodec.hbPrev body
    , hvVK = LeiosCodec.hbVk body
    , hvVrfVK = LeiosCodec.hbVrfVk body
    , hvVrfRes = LeiosCodec.hbVrfRes body
    , hvOCert = LeiosCodec.hbOCert body
    , hvSlotNo = LeiosCodec.hbSlotNo body
    , hvLeios =
        LeiosLeiosRight
          ( LeiosCodec.hbBlockBodyContainsLeiosCert body
          , fromCodecEbAnnouncement <$> LeiosCodec.hbEbAnnouncement body
          )
    , hvSigned = body
    , hvSignature = LeiosCodec.headerSig hdr
    }
 where
  body = LeiosCodec.headerBody hdr


{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}
type instance ProtoCrypto (Praos c) = c

type instance ProtoCrypto (PraosWithLeios c) = c

-- | 'envelopeChecks' for any Praos; the config and the header type are what differ.
envelopeChecks_BasePraos ::
  MaxMajorProtVer ->
  BasePraosLedgerView proto ->
  -- | Size of the header, over the bytes it was decoded from
  Int ->
  -- | Size of the block body
  Word32 ->
  Except EnvelopeError ()
envelopeChecks_BasePraos (MaxMajorProtVer maxpv) lv headerSize bodySize =
  envelopeCheck maxpv ccd $
    EnvelopeHeaderView
      { ehvProtVer = m
      , ehvHeaderSize = headerSize
      , ehvBodySize = bodySize
      }
 where
  ProtVer m _ = plvProtocolVersion lv
  ccd =
    ChainChecksPParams
      { ccMaxBHSize = plvMaxHeaderSize lv
      , ccMaxBBSize = plvMaxBodySize lv
      , ccProtocolVersion = plvProtocolVersion lv
      }

-- | 'verifyHeaderIntegrity' for any Praos.
verifyHeaderIntegrity_BasePraos ::
  BasePraosCrypto proto c =>
  Word64 ->
  BasePraosValidateView proto c ->
  Bool
verifyHeaderIntegrity_BasePraos slotsPerKESPeriod hv =
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

-- | The header body to sign, given the fields only forging supplies.
--
-- The fields the protocols share are filled in here; the caller says how its own
-- body extends that, since only it knows the extra fields.
mkHeader_BasePraos ::
  SlotNo ->
  BlockNo ->
  PrevHash ->
  Hash.Hash HASH EraIndependentBlockBody ->
  Int ->
  ProtVer ->
  -- | How this protocol's header body extends the shared one
  (PraosCodec.HeaderBody c -> body) ->
  PraosToSign c ->
  body
mkHeader_BasePraos
  slotNo
  blockNo
  prevHash
  bbHash
  sz
  protVer
  extend
  PraosToSign
    { praosToSignIssuerVK
    , praosToSignVrfVK
    , praosToSignVrfRes
    , praosToSignOCert
    } =
    extend $
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

{-------------------------------------------------------------------------------
  Praos
-------------------------------------------------------------------------------}

instance PraosCrypto c => ProtocolHeaderSupportsEnvelope (Praos c) where
  pHeaderHash hdr = ShelleyHash $ PraosCodec.headerHash hdr
  pHeaderPrevHash (PraosCodec.Header body _) = PraosCodec.hbPrev body
  pHeaderBodyHash (PraosCodec.Header body _) = PraosCodec.hbBodyHash body
  pHeaderSlot (PraosCodec.Header body _) = PraosCodec.hbSlotNo body
  pHeaderBlock (PraosCodec.Header body _) = PraosCodec.hbBlockNo body
  pHeaderSize hdr = fromIntegral $ PraosCodec.headerSize hdr
  pHeaderBlockSize (PraosCodec.Header body _) = fromIntegral $ PraosCodec.hbBodySize body
  pHeaderLeiosContainsCert = default_pHeaderLeiosContainsCert
  pHeaderLeiosEbAnnouncement = default_pHeaderLeiosEbAnnouncement

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr@(PraosCodec.Header body _) =
    envelopeChecks_BasePraos
      (praosMaxMajorPV (praosParams cfg))
      lv
      (PraosCodec.headerSize hdr)
      (PraosCodec.hbBodySize body)

instance PraosCrypto c => ProtocolHeaderSupportsKES (Praos c) where
  configSlotsPerKESPeriod cfg = praosSlotsPerKESPeriod $ praosParams cfg

  verifyHeaderIntegrity slotsPerKESPeriod =
    verifyHeaderIntegrity_BasePraos slotsPerKESPeriod . protocolHeaderView_Praos

  mkHeader hk cbl il slotNo blockNo prevHash bbHash sz protVer PraosLeiosLeft{} = do
    PraosFields{praosSignature, praosToSign} <-
      forgePraosFields hk cbl il (mkHeader_BasePraos slotNo blockNo prevHash bbHash sz protVer id)
    pure $ PraosCodec.Header praosToSign praosSignature

  -- Praos announces no endorser blocks.
  protocolStateLeiosInfo _ _ = Nothing

instance PraosCrypto c => ProtocolHeaderSupportsProtocol (Praos c) where
  type CannotForgeError (Praos c) = PraosCannotForge c

  protocolHeaderView = protocolHeaderView_Praos
  pHeaderIssuer = hvVK . protocolHeaderView_Praos
  pHeaderIssueNo = SL.ocertN . hvOCert . protocolHeaderView_Praos

  -- This is the "unified" VRF value, prior to range extension which yields e.g.
  -- the leader VRF value used for slot election.
  --
  -- In the future, we might want to use a dedicated range-extended VRF value
  -- here instead.
  pTieBreakVRFValue = certifiedOutput . hvVrfRes . protocolHeaderView_Praos

{-------------------------------------------------------------------------------
  PraosWithLeios
-------------------------------------------------------------------------------}

instance LeiosCrypto c => ProtocolHeaderSupportsEnvelope (PraosWithLeios c) where
  pHeaderHash hdr = ShelleyHash $ LeiosCodec.headerHash hdr
  pHeaderPrevHash (LeiosCodec.Header body _) = LeiosCodec.hbPrev body
  pHeaderBodyHash (LeiosCodec.Header body _) = LeiosCodec.hbBodyHash body
  pHeaderSlot (LeiosCodec.Header body _) = LeiosCodec.hbSlotNo body
  pHeaderBlock (LeiosCodec.Header body _) = LeiosCodec.hbBlockNo body
  pHeaderSize hdr = fromIntegral $ LeiosCodec.headerSize hdr
  pHeaderBlockSize (LeiosCodec.Header body _) = fromIntegral $ LeiosCodec.hbBodySize body
  pHeaderLeiosContainsCert (LeiosCodec.Header body _) =
    LeiosCodec.hbBlockBodyContainsLeiosCert body
  pHeaderLeiosEbAnnouncement (LeiosCodec.Header body _) =
    fromCodecEbAnnouncement <$> LeiosCodec.hbEbAnnouncement body

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr@(LeiosCodec.Header body _) =
    envelopeChecks_BasePraos
      (praosMaxMajorPV (praosParams (leiosPraosConfig cfg)))
      lv
      (LeiosCodec.headerSize hdr)
      (LeiosCodec.hbBodySize body)

instance LeiosCrypto c => ProtocolHeaderSupportsKES (PraosWithLeios c) where
  configSlotsPerKESPeriod cfg =
    praosSlotsPerKESPeriod $ praosParams $ leiosPraosConfig cfg

  verifyHeaderIntegrity slotsPerKESPeriod =
    verifyHeaderIntegrity_BasePraos slotsPerKESPeriod . protocolHeaderView_PraosWithLeios

  mkHeader
    hk
    cbl
    il
    slotNo
    blockNo
    prevHash
    bbHash
    sz
    protVer
    (LeiosLeiosRight (containsCert, mbAnn)) = do
      PraosFields{praosSignature, praosToSign} <-
        forgePraosFields hk cbl il $
          mkHeader_BasePraos slotNo blockNo prevHash bbHash sz protVer $ \pb ->
            extendHeaderBodyWithLeios pb containsCert (toCodecEbAnnouncement <$> mbAnn)
      pure $ LeiosCodec.Header praosToSign praosSignature

  protocolStateLeiosInfo _ cs =
    case praosStateLeiosAnnouncement cs of
      LeiosLeiosRight SNothing -> Nothing
      LeiosLeiosRight (SJust announced) ->
        Just (announcedEb announced, praosStateLastSlot cs)

instance LeiosCrypto c => ProtocolHeaderSupportsProtocol (PraosWithLeios c) where
  type CannotForgeError (PraosWithLeios c) = PraosCannotForge c

  protocolHeaderView = protocolHeaderView_PraosWithLeios
  pHeaderIssuer = hvVK . protocolHeaderView_PraosWithLeios
  pHeaderIssueNo = SL.ocertN . hvOCert . protocolHeaderView_PraosWithLeios
  pTieBreakVRFValue = certifiedOutput . hvVrfRes . protocolHeaderView_PraosWithLeios

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance PraosCrypto c => SignedHeader (PraosCodec.Header c) where
  headerSigned = PraosCodec.headerBody

instance LeiosCrypto c => SignedHeader (LeiosCodec.Header c) where
  headerSigned = LeiosCodec.headerBody

instance PraosCrypto c => ShelleyProtocol (Praos c)

instance LeiosCrypto c => ShelleyProtocol (PraosWithLeios c)
