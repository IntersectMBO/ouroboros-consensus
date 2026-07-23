{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Protocol.Praos () where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.VRF (certifiedOutput)
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer))
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Slot (SlotNo (unSlotNo))
import Cardano.Protocol.TPraos.OCert
  ( OCert (ocertKESPeriod, ocertVkHot)
  )
import qualified Cardano.Protocol.TPraos.OCert as SL
import Data.Either (isRight)
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Praos.Common
  ( MaxMajorProtVer (MaxMajorProtVer)
  )
import Ouroboros.Consensus.Protocol.Praos.Header
  ( Header (..)
  , HeaderBody (..)
  , hbLeiosContainsCert
  , headerHash
  , headerSize
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
  pHeaderLeiosContainsCert (Header body _) = hbLeiosContainsCert body

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    envelopeCheck maxpv ccd $
      EnvelopeHeaderView
        { ehvProtVer = m
        , ehvHeaderSize = headerSize hdr
        , ehvBodySize = hbBodySize body
        }
   where
    Header body _ = hdr
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
  verifyHeaderIntegrity slotsPerKESPeriod header =
    isRight $ KES.verifySignedKES () ocertVkHot t headerBody headerSig
   where
    Header{headerBody, headerSig} = header
    SL.OCert
      { ocertVkHot
      , ocertKESPeriod = SL.KESPeriod startOfKesPeriod
      } = hbOCert headerBody

    currentKesPeriod =
      fromIntegral $
        unSlotNo (hbSlotNo headerBody) `div` slotsPerKESPeriod

    t
      | currentKesPeriod >= startOfKesPeriod =
          currentKesPeriod - startOfKesPeriod
      | otherwise =
          0
  mkHeader hk cbl il slotNo blockNo prevHash bbHash sz protVer mLeiosExt = do
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
          , hbLeiosExt = mLeiosExt
          }

  protocolStateLeiosInfo _ cs =
    case praosStateLeiosAnnouncement cs of
      SNothing -> Nothing
      SJust ann -> Just (ann, praosStateLastSlot cs)

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

type instance Signed (Header c) = HeaderBody c
instance PraosCrypto c => SignedHeader (Header c) where
  headerSigned = headerBody

instance PraosCrypto c => ShelleyProtocol (Praos c)
