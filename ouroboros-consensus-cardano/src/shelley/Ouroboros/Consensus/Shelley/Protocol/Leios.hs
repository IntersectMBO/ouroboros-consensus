{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The Shelley block layer's view of a Leios header, the counterpart to
-- "Ouroboros.Consensus.Shelley.Protocol.Praos".
module Ouroboros.Consensus.Shelley.Protocol.Leios () where

import Cardano.Crypto.VRF (certifiedOutput)
import Cardano.Ledger.BaseTypes
  ( getVersion32
  , pvMajor
  , pvMinor
  )
import Cardano.Ledger.Block
  ( BlockHeaderVersionInfo (..)
  )
import Cardano.Ledger.MemoBytes (mkMemoized)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import qualified Cardano.Protocol.TPraos.OCert as SL
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Ouroboros.Consensus.Protocol.Leios
  ( ConsensusConfig (..)
  , Leios
  , LeiosCrypto
  , LeiosHeaderView
  )
import Ouroboros.Consensus.Protocol.Praos
  ( PraosCannotForge
  , PraosFields (..)
  , PraosToSign (..)
  , forgePraosFields
  , praosVerifyHeaderIntegrity
  )
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
  )
import Ouroboros.Consensus.Shelley.Protocol.EnvelopeChecks
  ( EnvelopeError
  , praosEnvelopeCheck
  )
-- The overlay delegates to the Praos instances, which are orphans.
import Ouroboros.Consensus.Shelley.Protocol.Praos ()

type instance ProtoCrypto (Leios c) = c

type instance ShelleyProtocolHeader (Leios c) = Leios.Header c

-- | What the protocol reads off a Leios header.
leiosHeaderToView :: Leios.Header c -> LeiosHeaderView c
leiosHeaderToView hdr =
  HeaderView'
    { hvPrevHash = Leios.hbPrev body
    , hvVK = Leios.hbVk body
    , hvVrfVK = Leios.hbVrfVk body
    , hvVrfRes = Leios.hbVrfRes body
    , hvOCert = Leios.hbOCert body
    , hvSlotNo = Leios.hbSlotNo body
    , hvSigned = body
    , hvSignature = Leios.headerSig hdr
    }
 where
  body = Leios.headerBody hdr

instance LeiosCrypto c => ProtocolHeaderSupportsEnvelope (Leios c) where
  pHeaderHash hdr = ShelleyHash $ Leios.headerHash hdr
  pHeaderPrevHash = Leios.hbPrev . Leios.headerBody
  pHeaderBodyHash = Leios.hbBodyHash . Leios.headerBody
  pHeaderSlot = Leios.hbSlotNo . Leios.headerBody
  pHeaderBlock = Leios.hbBlockNo . Leios.headerBody
  pHeaderSize = fromIntegral . Leios.headerSize
  pHeaderBlockSize = fromIntegral . Leios.hbBodySize . Leios.headerBody
  pHeaderContainsLeiosCert = Leios.hbBlockBodyContainsLeiosCert . Leios.headerBody

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    praosEnvelopeCheck
      (protoMaxMajorPV cfg)
      lv
      (Leios.headerSize hdr)
      (Leios.hbBodySize (Leios.headerBody hdr))

instance LeiosCrypto c => ProtocolHeaderSupportsKES (Leios c) where
  configSlotsPerKESPeriod = configSlotsPerKESPeriod . leiosPraosConfig

  verifyHeaderIntegrity slotsPerKESPeriod =
    praosVerifyHeaderIntegrity slotsPerKESPeriod . leiosHeaderToView

  mkHeader hk cbl il slotNo blockNo prevHash bbHash sz protVer = do
    PraosFields{praosSignature, praosToSign} <- forgePraosFields hk cbl il mkLeiosHeaderBody
    -- TODO: update mkHeader to take a protVer
    pure $ mkMemoized (pvMajor protVer) $ Leios.HeaderRaw praosToSign praosSignature
   where
    mkLeiosHeaderBody
      PraosToSign
        { praosToSignIssuerVK
        , praosToSignVrfVK
        , praosToSignVrfRes
        , praosToSignOCert
        } =
        Leios.HeaderBody
          { Leios.hbBlockNo = blockNo
          , Leios.hbSlotNo = slotNo
          , Leios.hbPrev = prevHash
          , Leios.hbVk = praosToSignIssuerVK
          , Leios.hbVrfVk = praosToSignVrfVK
          , Leios.hbVrfRes = praosToSignVrfRes
          , Leios.hbBodySize = fromIntegral sz
          , Leios.hbBodyHash = bbHash
          , Leios.hbOCert = praosToSignOCert
          , Leios.hbVersionInfo = versionInfo
          , Leios.hbBlockBodyContainsLeiosCert = False -- FIXME: Fill this in when forging
          , Leios.hbEbReferencesAnnouncement = SNothing -- FIXME: Fill this in when forging
          }

    versionInfo =
      BlockHeaderVersionInfo
        { bhviHighestSupportedMajorVersion = getVersion32 (pvMajor protVer)
        , bhviSelfReportedSoftwareTag = pvMinor protVer
        }

instance LeiosCrypto c => ProtocolHeaderSupportsProtocol (Leios c) where
  type CannotForgeError (Leios c) = PraosCannotForge c

  protocolHeaderView = leiosHeaderToView

  pHeaderIssuer = Leios.hbVk . Leios.headerBody
  pHeaderIssueNo = SL.ocertN . Leios.hbOCert . Leios.headerBody
  pTieBreakVRFValue = certifiedOutput . Leios.hbVrfRes . Leios.headerBody

type instance Signed (Leios.Header c) = Leios.HeaderBody c

instance LeiosCrypto c => SignedHeader (Leios.Header c) where
  headerSigned = Leios.headerBody

instance LeiosCrypto c => ShelleyProtocol (Leios c)
