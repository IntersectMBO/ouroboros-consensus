{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Protocol.Praos () where

import Cardano.Crypto.VRF (certifiedOutput)
import Cardano.Protocol.Crypto (Crypto)
import Cardano.Protocol.Praos.BlockHeader
  ( Header (..)
  , HeaderBody (..)
  , headerHash
  , headerSize
  )
import qualified Cardano.Protocol.TPraos.OCert as SL
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Praos.Common (protoMaxMajorPV)
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
  , defaultHeaderContainsLeiosCert
  )
import Ouroboros.Consensus.Shelley.Protocol.EnvelopeChecks
  ( EnvelopeError
  , praosEnvelopeCheck
  )

type instance ProtoCrypto (Praos c) = c

type instance ShelleyProtocolHeader (Praos c) = Header c

instance PraosCrypto c => ProtocolHeaderSupportsEnvelope (Praos c) where
  pHeaderHash hdr = ShelleyHash $ headerHash hdr
  pHeaderPrevHash (Header body _) = hbPrev body
  pHeaderBodyHash (Header body _) = hbBodyHash body
  pHeaderSlot (Header body _) = hbSlotNo body
  pHeaderBlock (Header body _) = hbBlockNo body
  pHeaderSize hdr = fromIntegral $ headerSize hdr
  pHeaderBlockSize (Header body _) = fromIntegral $ hbBodySize body
  pHeaderContainsLeiosCert = defaultHeaderContainsLeiosCert

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    praosEnvelopeCheck
      (protoMaxMajorPV cfg)
      lv
      (headerSize hdr)
      (hbBodySize body)
   where
    Header body _ = hdr

-- | What the protocol reads off a Praos header.
praosHeaderToView :: Crypto c => Header c -> HeaderView c
praosHeaderToView Header{headerBody, headerSig} =
  HeaderView'
    { hvPrevHash = hbPrev headerBody
    , hvVK = hbVk headerBody
    , hvVrfVK = hbVrfVk headerBody
    , hvVrfRes = hbVrfRes headerBody
    , hvOCert = hbOCert headerBody
    , hvSlotNo = hbSlotNo headerBody
    , hvSigned = headerBody
    , hvSignature = headerSig
    }

instance PraosCrypto c => ProtocolHeaderSupportsKES (Praos c) where
  configSlotsPerKESPeriod cfg = praosSlotsPerKESPeriod $ praosParams cfg
  verifyHeaderIntegrity slotsPerKESPeriod =
    praosVerifyHeaderIntegrity slotsPerKESPeriod . praosHeaderToView
  mkHeader hk cbl il slotNo blockNo prevHash bbHash sz protVer = do
    PraosFields{praosSignature, praosToSign} <- forgePraosFields hk cbl il mkBhBodyBytes
    pure $ Header praosToSign praosSignature
   where
    mkBhBodyBytes
      PraosToSign
        { praosToSignIssuerVK
        , praosToSignVrfVK
        , praosToSignVrfRes
        , praosToSignOCert
        } =
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
  protocolHeaderView = praosHeaderToView
  pHeaderIssuer = hbVk . headerBody
  pHeaderIssueNo = SL.ocertN . hbOCert . headerBody

  -- This is the "unified" VRF value, prior to range extension which yields e.g.
  -- the leader VRF value used for slot election.
  --
  -- In the future, we might want to use a dedicated range-extended VRF value
  -- here instead.
  pTieBreakVRFValue = certifiedOutput . hbVrfRes . headerBody

type instance Signed (Header c) = HeaderBody c
instance PraosCrypto c => SignedHeader (Header c) where
  headerSigned = headerBody

instance PraosCrypto c => ShelleyProtocol (Praos c)
