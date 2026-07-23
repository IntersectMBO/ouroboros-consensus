{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Protocol.TPraos () where

import qualified Cardano.Crypto.KES as SL
import Cardano.Crypto.VRF (certifiedOutput)
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer))
import Cardano.Ledger.Hashes (originalBytesSize)
import Cardano.Protocol.TPraos.API (PraosCrypto)
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL
import Cardano.Protocol.TPraos.OCert (ocertKESPeriod, ocertVkHot)
import qualified Cardano.Protocol.TPraos.OCert as SL
import Cardano.Slotting.Slot (unSlotNo)
import Data.Either (isRight)
import Data.Word (Word32)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Protocol.Signed
  ( Signed
  , SignedHeader (headerSigned)
  )
import Ouroboros.Consensus.Protocol.TPraos
  ( MaxMajorProtVer (MaxMajorProtVer)
  , TPraos
  , TPraosCannotForge
  , TPraosFields (..)
  , TPraosToSign (..)
  , forgeTPraosFields
  , tpraosMaxMajorPV
  , tpraosParams
  , tpraosSlotsPerKESPeriod
  )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtoCrypto
  , ProtocolHeaderSupportsEnvelope (..)
  , ProtocolHeaderSupportsKES (..)
  , ProtocolHeaderSupportsProtocol (..)
  , ShelleyHash (..)
  , ShelleyProtocol
  , ShelleyProtocolHeader
  , default_pHeaderLeiosContainsCert
  )
import Ouroboros.Consensus.Shelley.Protocol.EnvelopeChecks
  ( EnvelopeError
  , EnvelopeHeaderView (..)
  , envelopeCheck
  )

type instance ProtoCrypto (TPraos c) = c

type instance ShelleyProtocolHeader (TPraos c) = SL.BHeader c

instance PraosCrypto c => ProtocolHeaderSupportsEnvelope (TPraos c) where
  pHeaderHash = ShelleyHash . SL.unHashHeader . SL.bhHash
  pHeaderPrevHash = SL.bheaderPrev . SL.bhbody
  pHeaderBodyHash = SL.bhash . SL.bhbody
  pHeaderSlot = SL.bheaderSlotNo . SL.bhbody
  pHeaderBlock = SL.bheaderBlockNo . SL.bhbody
  pHeaderSize = fromIntegral . originalBytesSize
  pHeaderBlockSize = fromIntegral @Word32 @Natural . SL.bsize . SL.bhbody
  pHeaderLeiosContainsCert = default_pHeaderLeiosContainsCert

  type EnvelopeCheckError _ = EnvelopeError

  envelopeChecks cfg lv hdr =
    envelopeCheck maxPV ccd $
      EnvelopeHeaderView
        { ehvProtVer = m
        , ehvHeaderSize = originalBytesSize hdr
        , ehvBodySize = SL.bsize bhb
        }
   where
    bhb = SL.bhbody hdr
    ccd = SL.tplvChainChecks lv
    ProtVer m _ = SL.bprotver bhb
    MaxMajorProtVer maxPV = tpraosMaxMajorPV (tpraosParams cfg)

instance PraosCrypto c => ProtocolHeaderSupportsKES (TPraos c) where
  configSlotsPerKESPeriod cfg = tpraosSlotsPerKESPeriod $ tpraosParams cfg
  verifyHeaderIntegrity slotsPerKESPeriod hdr =
    isRight $ SL.verifySignedKES () ocertVkHot t hdrBody hdrSignature
   where
    SL.BHeader hdrBody hdrSignature = hdr
    SL.OCert
      { ocertVkHot
      , ocertKESPeriod = SL.KESPeriod startOfKesPeriod
      } = SL.bheaderOCert hdrBody

    currentKesPeriod =
      fromIntegral $
        unSlotNo (SL.bheaderSlotNo $ SL.bhbody hdr) `div` slotsPerKESPeriod

    t
      | currentKesPeriod >= startOfKesPeriod =
          currentKesPeriod - startOfKesPeriod
      | otherwise =
          0

  mkHeader hotKey canBeLeader isLeader curSlot curNo prevHash bbHash actualBodySize protVer _mLeios = do
    TPraosFields{tpraosSignature, tpraosToSign} <-
      forgeTPraosFields hotKey canBeLeader isLeader mkBhBody
    pure $ SL.BHeader tpraosToSign tpraosSignature
   where
    mkBhBody toSign =
      SL.BHBody
        { SL.bheaderPrev = prevHash
        , SL.bheaderVk = tpraosToSignIssuerVK
        , SL.bheaderVrfVk = tpraosToSignVrfVK
        , SL.bheaderSlotNo = curSlot
        , SL.bheaderBlockNo = curNo
        , SL.bheaderEta = tpraosToSignEta
        , SL.bheaderL = tpraosToSignLeader
        , SL.bsize = fromIntegral actualBodySize
        , SL.bhash = bbHash
        , SL.bheaderOCert = tpraosToSignOCert
        , SL.bprotver = protVer
        }
     where
      TPraosToSign
        { tpraosToSignIssuerVK
        , tpraosToSignVrfVK
        , tpraosToSignEta
        , tpraosToSignLeader
        , tpraosToSignOCert
        } = toSign

instance PraosCrypto c => ProtocolHeaderSupportsProtocol (TPraos c) where
  type CannotForgeError (TPraos c) = TPraosCannotForge c

  protocolHeaderView = id
  pHeaderIssuer = SL.bheaderVk . SL.bhbody
  pHeaderIssueNo = SL.ocertN . SL.bheaderOCert . SL.bhbody

  -- As this is the leader VRF value, which is used for slot election in the
  -- first place, it gives an advantage to smaller pools in a multi-leader slot.
  -- This was not an intentional decision, see
  -- https://github.com/IntersectMBO/ouroboros-network/issues/4051 for a more
  -- detailed discussion.
  pTieBreakVRFValue = certifiedOutput . SL.bheaderL . SL.bhbody

type instance Signed (SL.BHeader c) = SL.BHBody c

instance PraosCrypto c => SignedHeader (SL.BHeader c) where
  headerSigned = SL.bhbody

instance PraosCrypto c => ShelleyProtocol (TPraos c)
