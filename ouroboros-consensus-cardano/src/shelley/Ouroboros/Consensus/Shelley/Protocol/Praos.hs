{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Protocol.Praos (PraosEnvelopeError (..)) where

import qualified Cardano.Crypto.KES as KES
import           Cardano.Crypto.VRF (certifiedOutput)
import           Cardano.Ledger.BaseTypes (ProtVer (ProtVer), Version)
import           Cardano.Ledger.BHeaderView
import           Cardano.Ledger.Keys (hashKey)
import           Cardano.Ledger.Slot (SlotNo (unSlotNo))
import           Cardano.Protocol.TPraos.OCert
                     (OCert (ocertKESPeriod, ocertVkHot))
import qualified Cardano.Protocol.TPraos.OCert as SL
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Data.Either (isRight)
import           Data.Word (Word16, Word32)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (MaxMajorProtVer (MaxMajorProtVer))
import           Ouroboros.Consensus.Protocol.Praos.Header (Header (..),
                     HeaderBody (..), headerHash, headerSize)
import           Ouroboros.Consensus.Protocol.Praos.Views
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto,
                     ProtocolHeaderSupportsEnvelope (..),
                     ProtocolHeaderSupportsKES (..),
                     ProtocolHeaderSupportsLedger (..),
                     ProtocolHeaderSupportsProtocol (..),
                     ShelleyHash (ShelleyHash), ShelleyProtocol,
                     ShelleyProtocolHeader)


type instance ProtoCrypto (Praos c) = c

type instance ShelleyProtocolHeader (Praos c) = Header c

data PraosEnvelopeError
  = ObsoleteNode Version Version
    -- ^ This is a subtle case.
    --
    -- This node is explicitly rejecting the header, but the header isn't
    -- necessarily _directly_ at fault.
    --
    -- This rejection specifically happens when the ticked ledger state being
    -- used to validate this header contains a protocol major version (the
    -- first 'Version') that exceeds the maximum major protocol version allowed
    -- for this era this specific node's configuration (the second 'Version').
    -- The only thing the header did "wrong" was extend such a ledger state.
    --
    -- Note that the ChainSync client ensures that that ledger state is ticked
    -- starting from one of the latest k+1 ledger states on the node's current
    -- chain (modulo STM scheduling).
    --
    -- For Cardano and for now at least, this max major prot ver is typically
    -- hardcoded in the source code (subject only to whether or not the
    -- run-time config files enable "experimental" eras).
    --
    -- Hence, most likely, the appropriate rectifying action is for the node
    -- operator to update their node software and/or config; hence the name
    -- 'ObsoleteNode'. (Or if they're intentionally testing an experimental
    -- era, they forgot to set the appropriate config flag.)
    --
    -- TODO Would it be more intuitive to instead enforce this when validating
    -- the block that results in a ledger state with a major prot ver that
    -- violates the config's limit? Would the errors the user sees be more or
    -- less helpful? Etc.
    --
    -- TODO (cont'd) It's not even obviously that specific ledger
    -- state's/block's fault, since the protocol version is the consequence of
    -- on-chain governance. Is it the voters' fault? Is the fault of the first
    -- block that was after the voting deadline? So "extending the ledger state
    -- that resulting from ticking after applying the block after the epoch
    -- that extended the ancestor block that was after the voting deadline that
    -- ..." is merely one step more removed. And this 'envelopeChecks' approach
    -- does avoid the surprise (since the rejection doesn't even depend on the
    -- block's non-header content either) where the header could be validated
    -- but its underlying block could not. See
    -- <https://github.com/IntersectMBO/ouroboros-consensus/issues/325>.
  | HeaderSizeTooLarge Int Word16
  | BlockSizeTooLarge Word32 Word32
  deriving (Eq, Generic, Show)

instance NoThunks PraosEnvelopeError

instance PraosCrypto c => ProtocolHeaderSupportsEnvelope (Praos c) where
  pHeaderHash hdr = ShelleyHash $ headerHash hdr
  pHeaderPrevHash (Header body _) = hbPrev body
  pHeaderBodyHash (Header body _) = hbBodyHash body
  pHeaderSlot (Header body _) = hbSlotNo body
  pHeaderBlock (Header body _) = hbBlockNo body
  pHeaderSize hdr = fromIntegral $ headerSize hdr
  pHeaderBlockSize (Header body _) = fromIntegral $ hbBodySize body

  type EnvelopeCheckError _ = PraosEnvelopeError

  envelopeChecks cfg lv hdr = do
    unless (m <= maxpv) $ throwError (ObsoleteNode m maxpv)
    unless (bhviewHSize bhv <= fromIntegral @Word16 @Int maxHeaderSize) $
      throwError $
        HeaderSizeTooLarge (bhviewHSize bhv) maxHeaderSize
    unless (bhviewBSize bhv <= maxBodySize) $
      throwError $
        BlockSizeTooLarge (bhviewBSize bhv) maxBodySize
    where
      pp = praosParams cfg
      (MaxMajorProtVer maxpv) = praosMaxMajorPV pp
      (ProtVer m _) = lvProtocolVersion lv
      maxHeaderSize = lvMaxHeaderSize lv
      maxBodySize = lvMaxBodySize lv
      bhv = mkHeaderView hdr

instance PraosCrypto c => ProtocolHeaderSupportsKES (Praos c) where
  configSlotsPerKESPeriod cfg = praosSlotsPerKESPeriod $ praosParams cfg
  verifyHeaderIntegrity slotsPerKESPeriod header =
    isRight $ KES.verifySignedKES () ocertVkHot t headerBody headerSig
    where
      Header {headerBody, headerSig} = header
      SL.OCert
        { ocertVkHot,
          ocertKESPeriod = SL.KESPeriod startOfKesPeriod
        } = hbOCert headerBody

      currentKesPeriod =
        fromIntegral $
          unSlotNo (hbSlotNo headerBody) `div` slotsPerKESPeriod

      t
        | currentKesPeriod >= startOfKesPeriod =
            currentKesPeriod - startOfKesPeriod
        | otherwise =
            0
  mkHeader hk cbl il slotNo blockNo prevHash bbHash sz protVer = do
    PraosFields {praosSignature, praosToSign} <- forgePraosFields hk cbl il mkBhBodyBytes
    pure $ Header praosToSign praosSignature
    where
      mkBhBodyBytes
        PraosToSign
          { praosToSignIssuerVK,
            praosToSignVrfVK,
            praosToSignVrfRes,
            praosToSignOCert
          } =
          HeaderBody
            { hbBlockNo = blockNo,
              hbSlotNo = slotNo,
              hbPrev = prevHash,
              hbVk = praosToSignIssuerVK,
              hbVrfVk = praosToSignVrfVK,
              hbVrfRes = praosToSignVrfRes,
              hbBodySize = fromIntegral sz,
              hbBodyHash = bbHash,
              hbOCert = praosToSignOCert,
              hbProtVer = protVer
            }

instance PraosCrypto c => ProtocolHeaderSupportsProtocol (Praos c) where
  type CannotForgeError (Praos c) = PraosCannotForge c
  protocolHeaderView Header {headerBody, headerSig} =
    HeaderView
      { hvPrevHash = hbPrev headerBody,
        hvVK = hbVk headerBody,
        hvVrfVK = hbVrfVk headerBody,
        hvVrfRes = hbVrfRes headerBody,
        hvOCert = hbOCert headerBody,
        hvSlotNo = hbSlotNo headerBody,
        hvSigned = headerBody,
        hvSignature = headerSig
      }
  pHeaderIssuer = hbVk . headerBody
  pHeaderIssueNo = SL.ocertN . hbOCert . headerBody

  -- This is the "unified" VRF value, prior to range extension which yields e.g.
  -- the leader VRF value used for slot election.
  --
  -- In the future, we might want to use a dedicated range-extended VRF value
  -- here instead.
  pTieBreakVRFValue = certifiedOutput . hbVrfRes . headerBody

instance PraosCrypto c => ProtocolHeaderSupportsLedger (Praos c) where
  mkHeaderView hdr@Header {headerBody} =
    BHeaderView
      { bhviewID = hashKey $ hbVk headerBody,
        bhviewBSize = hbBodySize headerBody,
        bhviewHSize = headerSize hdr,
        bhviewBHash = hbBodyHash headerBody,
        bhviewSlot = hbSlotNo headerBody
      }

type instance Signed (Header c) = HeaderBody c
instance PraosCrypto c => SignedHeader (Header c) where
  headerSigned = headerBody

instance PraosCrypto c => ShelleyProtocol (Praos c)
