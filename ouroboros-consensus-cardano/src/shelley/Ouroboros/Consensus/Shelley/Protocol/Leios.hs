{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances tying the Leios consensus protocol to Shelley-based blocks.
module Ouroboros.Consensus.Shelley.Protocol.Leios () where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.VRF (certifiedOutput)
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer))
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Slot (SlotNo (unSlotNo))
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
  ( Header (..)
  , HeaderBody (..)
  , headerHash
  , headerSize
  )
import Cardano.Protocol.TPraos.OCert
  ( OCert (ocertKESPeriod, ocertVkHot)
  )
import qualified Cardano.Protocol.TPraos.OCert as SL
import Data.Either (isRight)
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Ouroboros.Consensus.Protocol.Leios
import Ouroboros.Consensus.Protocol.Praos
  ( PraosCannotForge
  , PraosFields (..)
  , PraosParams (praosMaxMajorPV, praosSlotsPerKESPeriod)
  , PraosToSign (..)
  , forgePraosFields
  )
import Ouroboros.Consensus.Protocol.Praos.Common
  ( MaxMajorProtVer (MaxMajorProtVer)
  )
import Ouroboros.Consensus.Protocol.Praos.Views (PraosLedgerView (..))
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

type instance ProtoCrypto (Leios c) = c

type instance ShelleyProtocolHeader (Leios c) = Leios.Header c

instance LeiosCrypto c => ProtocolHeaderSupportsEnvelope (Leios c) where
  pHeaderHash hdr = ShelleyHash $ Leios.headerHash hdr
  pHeaderPrevHash (Leios.Header body _) = Leios.hbPrev body
  pHeaderBodyHash (Leios.Header body _) = Leios.hbBodyHash body
  pHeaderSlot (Leios.Header body _) = Leios.hbSlotNo body
  pHeaderBlock (Leios.Header body _) = Leios.hbBlockNo body
  pHeaderSize hdr = fromIntegral $ Leios.headerSize hdr
  pHeaderBlockSize (Leios.Header body _) = fromIntegral $ Leios.hbBodySize body

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    envelopeCheck maxpv ccd $
      EnvelopeHeaderView
        { ehvProtVer = m
        , ehvHeaderSize = Leios.headerSize hdr
        , ehvBodySize = Leios.hbBodySize body
        }
   where
    Leios.Header body _ = hdr
    MaxMajorProtVer maxpv = praosMaxMajorPV (leiosPraosParams cfg)
    ProtVer m _ = plvProtocolVersion lv
    ccd =
      ChainChecksPParams
        { ccMaxBHSize = plvMaxHeaderSize lv
        , ccMaxBBSize = plvMaxBodySize lv
        , ccProtocolVersion = plvProtocolVersion lv
        }

instance LeiosCrypto c => ProtocolHeaderSupportsKES (Leios c) where
  configSlotsPerKESPeriod cfg = praosSlotsPerKESPeriod $ leiosPraosParams cfg
  verifyHeaderIntegrity slotsPerKESPeriod header =
    isRight $ KES.verifySignedKES () ocertVkHot t headerBody headerSig
   where
    Leios.Header{Leios.headerBody, Leios.headerSig} = header
    SL.OCert
      { ocertVkHot
      , ocertKESPeriod = SL.KESPeriod startOfKesPeriod
      } = Leios.hbOCert headerBody

    currentKesPeriod =
      fromIntegral $
        unSlotNo (Leios.hbSlotNo headerBody) `div` slotsPerKESPeriod

    t
      | currentKesPeriod >= startOfKesPeriod =
          currentKesPeriod - startOfKesPeriod
      | otherwise =
          0
  mkHeader hk cbl il slotNo blockNo prevHash bbHash sz protVer = do
    PraosFields{praosSignature, praosToSign} <- forgePraosFields hk cbl il mkBhBodyBytes
    pure $ Leios.Header praosToSign praosSignature
   where
    mkBhBodyBytes
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
          , Leios.hbProtVer = protVer
          , Leios.hbBlockBodyContainsLeiosCert = False
          , Leios.hbEbAnnouncement = SNothing
          }

instance LeiosCrypto c => ProtocolHeaderSupportsProtocol (Leios c) where
  type CannotForgeError (Leios c) = PraosCannotForge c
  protocolHeaderView Leios.Header{Leios.headerBody, Leios.headerSig} =
    LeiosHeaderView
      { lhvPrevHash = Leios.hbPrev headerBody
      , lhvVK = Leios.hbVk headerBody
      , lhvVrfVK = Leios.hbVrfVk headerBody
      , lhvVrfRes = Leios.hbVrfRes headerBody
      , lhvOCert = Leios.hbOCert headerBody
      , lhvSlotNo = Leios.hbSlotNo headerBody
      , lhvSigned = headerBody
      , lhvSignature = headerSig
      }
  pHeaderIssuer = Leios.hbVk . Leios.headerBody
  pHeaderIssueNo = SL.ocertN . Leios.hbOCert . Leios.headerBody

  pTieBreakVRFValue = certifiedOutput . Leios.hbVrfRes . Leios.headerBody

type instance Signed (Leios.Header c) = Leios.HeaderBody c
instance LeiosCrypto c => SignedHeader (Leios.Header c) where
  headerSigned = Leios.headerBody

instance LeiosCrypto c => ShelleyProtocol (Leios c)
