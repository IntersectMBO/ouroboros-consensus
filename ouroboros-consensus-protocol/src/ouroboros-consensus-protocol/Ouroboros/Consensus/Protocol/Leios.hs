{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Leios: an overlay on Praos.
--
-- Leios does not replace Praos, it runs on top of it. The ranking blocks are
-- Praos blocks, chosen by the same leader schedule, signed by the same KES
-- keys, and extending the same nonce-carrying chain-dep state. What Leios adds
-- is endorser blocks alongside that chain, and two header fields with which a
-- ranking block announces one and certifies its predecessor's.
--
-- So this module is mostly re-use, and deliberately so:
--
-- * the configuration is Praos's, wrapped only because 'ConsensusConfig' is a
--   data family;
-- * the chain-dep state is 'PraosState', and the ledger view is
--   'Views.PraosLedgerView';
-- * 'protocolSecurityParam', 'checkIsLeader' and 'tickChainDepState' are
--   Praos's methods, called directly --- none of them reads a header;
-- * the KES and VRF checks are the very functions
--   "Ouroboros.Consensus.Protocol.Praos" exports.
--
-- What genuinely differs is the header body that gets signed: extending the
-- header changes the bytes the signature covers, so 'ValidateView' is a
-- 'Views.HeaderView' over the Leios body rather than the Praos one. That is
-- the only reason 'updateChainDepState' and 'reupdateChainDepState' are
-- written out here instead of delegating --- they are the two methods that
-- take a 'ValidateView'.
--
-- Named after the ledger's @Cardano.Protocol.Leios.*@ modules, which is where
-- the Leios header lives.
module Ouroboros.Consensus.Protocol.Leios
  ( Leios
  , LeiosCrypto
  , AnnouncedBy (..)
  , LeiosState (..)
  , ConsensusConfig (..)
  , Ticked (..)
  , LeiosValidateView
  ) where

import Cardano.Binary (Decoder, FromCBOR (..), ToCBOR (..), enforceSize)
import qualified Cardano.Crypto.KES as KES
import Cardano.Ledger.BaseTypes
  ( StrictMaybe (SNothing)
  , maybeToStrictMaybe
  , strictMaybeToMaybe
  )
import Cardano.Ledger.Block (EbReferencesAnnouncement)
import Cardano.Ledger.Core (fromEraCBOR, toEraCBOR)
import Cardano.Ledger.Keys (KeyHash, hashKey)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Protocol.Crypto (KES, StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (decode, encode))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Praos
  ( ConsensusConfig (..)
  , Praos
  , PraosCrypto
  , PraosIsLeader
  , PraosParams (..)
  , PraosState (..)
  , PraosValidationErr
  , Ticked (..)
  , reupdatePraosState
  , validateKESSignature
  , validateVRFSignature
  )
import Ouroboros.Consensus.Protocol.Praos.Common
  ( HasMaxMajorProtVer (..)
  , PraosCanBeLeader
  , PraosProtocolSupportsNode (..)
  , PraosTiebreakerView
  )
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Util.Versioned
  ( VersionDecoder (Decode)
  , decodeVersion
  , encodeVersion
  )

-- | Praos extended with Leios.
data Leios c

-- | What a Leios header needs of the crypto.
--
-- 'PraosCrypto' because the overlay delegates to Praos's own
-- 'ConsensusProtocol' instance and wraps its configuration, both of which
-- demand it. The one addition is the Leios header body, which is a different
-- object to sign.
class
  ( PraosCrypto c
  , KES.Signable (KES c) (Leios.HeaderBody c)
  ) =>
  LeiosCrypto c

instance LeiosCrypto StandardCrypto

-- | Leios configures nothing of its own, so it reuses Praos's configuration
-- whole.
--
-- A @newtype@ rather than a reuse of the very same type because
-- 'ConsensusConfig' is a data family, which is what lets
-- 'protocolSecurityParam' and friends infer the protocol from their argument.
newtype instance ConsensusConfig (Leios c) = LeiosConfig
  { leiosPraosConfig :: ConsensusConfig (Praos c)
  }
  deriving Generic

instance LeiosCrypto c => NoThunks (ConsensusConfig (Leios c))

instance HasMaxMajorProtVer (Leios c) where
  protoMaxMajorPV = protoMaxMajorPV . leiosPraosConfig

-- | An endorser-block announcement, and who announced it.
--
-- The issuer and the announcing header's slot ('praosStateLastSlot') together
-- give the election, which is what lets a certificate name what it certifies.
data AnnouncedBy = AnnouncedBy
  { announcedByIssuer :: !(KeyHash SL.BlockIssuer)
  , announcedEbReferences :: !EbReferencesAnnouncement
  }
  deriving (Generic, Show, Eq)

instance NoThunks AnnouncedBy

-- | 'PraosState' and the announcement a certificate is validated against.
data LeiosState = LeiosState
  { leiosStatePraos :: !PraosState
  , leiosStateAnnouncement :: !(StrictMaybe AnnouncedBy)
  -- ^ What the most recently applied header announced. Overwritten by every
  -- header, so one that announces nothing clears it: only the immediately
  -- preceding announcement can be certified.
  }
  deriving (Generic, Show, Eq)

instance NoThunks LeiosState

instance ToCBOR LeiosState where
  toCBOR = encode

instance FromCBOR LeiosState where
  fromCBOR = decode

-- | A format of its own, which merely also starts counting: nothing relates it
-- to 'PraosState'\'s versions, whose encoding it nests unchanged.
instance Serialise LeiosState where
  encode (LeiosState praos ann) =
    encodeVersion 0 $
      mconcat
        [ CBOR.encodeListLen 2
        , encode praos
        , toCBOR (strictMaybeToMaybe ann)
        ]

  decode = decodeVersion [(0, Decode dec)]
   where
    dec :: forall s. Decoder s LeiosState
    dec = do
      enforceSize "LeiosState" 2
      LeiosState <$> decode <*> (maybeToStrictMaybe <$> fromCBOR)

-- | The era only picks a serialisation version, and neither field's encoding
-- varies by one.
instance ToCBOR AnnouncedBy where
  toCBOR (AnnouncedBy issuer ann) =
    CBOR.encodeListLen 2 <> toCBOR issuer <> toEraCBOR @ShelleyEra ann

instance FromCBOR AnnouncedBy where
  fromCBOR = do
    enforceSize "AnnouncedBy" 2
    AnnouncedBy <$> fromCBOR <*> fromEraCBOR @ShelleyEra

data instance Ticked LeiosState = TickedLeiosState
  { tickedLeiosStateChainDepState :: LeiosState
  , tickedLeiosStateLedgerView :: Views.PraosLedgerView
  }

instance ChainDepStateSupportsPeras LeiosState where
  getEpochNonce = getEpochNonce . leiosStatePraos

instance ChainDepStateSupportsPeras (Ticked LeiosState) where
  getEpochNonce = getEpochNonce . tickedLeiosStateChainDepState

-- | The base protocol's ticked state, as it sits inside this one's.
--
-- What lets 'Leios' hand its state to a 'Praos' method.
basePraosTicked :: Ticked LeiosState -> Ticked PraosState
basePraosTicked tcs =
  TickedPraosState
    { tickedPraosStateChainDepState =
        leiosStatePraos (tickedLeiosStateChainDepState tcs)
    , tickedPraosStateLedgerView = tickedLeiosStateLedgerView tcs
    }

-- | What the protocol reads off a Leios header.
type LeiosValidateView c = Views.LeiosHeaderView c

instance LeiosCrypto c => ConsensusProtocol (Leios c) where
  type ChainDepState (Leios c) = LeiosState
  type IsLeader (Leios c) = PraosIsLeader c
  type CanBeLeader (Leios c) = PraosCanBeLeader c
  type TiebreakerView (Leios c) = PraosTiebreakerView c
  type LedgerView (Leios c) = Views.PraosLedgerView
  type ValidationErr (Leios c) = PraosValidationErr c
  type ValidateView (Leios c) = LeiosValidateView c

  -- These read nothing from the header, so the base protocol's methods apply;
  -- they are handed the 'PraosState' this one carries.
  protocolSecurityParam = protocolSecurityParam @(Praos c) . leiosPraosConfig

  checkIsLeader cfg cbl slot tcs =
    checkIsLeader @(Praos c) (leiosPraosConfig cfg) cbl slot (basePraosTicked tcs)

  tickChainDepState cfg lv slot st =
    TickedLeiosState
      { tickedLeiosStateChainDepState =
          st
            { leiosStatePraos =
                tickedPraosStateChainDepState $
                  tickChainDepState @(Praos c)
                    (leiosPraosConfig cfg)
                    lv
                    slot
                    (leiosStatePraos st)
            }
      , tickedLeiosStateLedgerView = lv
      }

  -- These take the 'ValidateView', so they cannot delegate: a Leios header
  -- signs the Leios body. Both checks are indifferent to which body that is,
  -- beyond needing it to be signable, so they are the very functions 'Praos'
  -- calls.
  updateChainDepState cfg b slot tcs = do
    validateKESSignature praosCfg lv (praosStateOCertCounters cs) b
    validateVRFSignature (praosStateEpochNonce cs) lv praosLeaderF b
    pure $ reupdateChainDepState cfg b slot tcs
   where
    praosCfg@(PraosConfig PraosParams{praosLeaderF} _) = leiosPraosConfig cfg
    lv = tickedLeiosStateLedgerView tcs
    cs = leiosStatePraos (tickedLeiosStateChainDepState tcs)

  reupdateChainDepState cfg b slot tcs =
    LeiosState
      { leiosStatePraos =
          reupdatePraosState
            (leiosPraosConfig cfg)
            b
            slot
            (leiosStatePraos (tickedLeiosStateChainDepState tcs))
      , leiosStateAnnouncement =
          AnnouncedBy (hashKey (Views.hvVK b))
            <$> Leios.hbEbReferencesAnnouncement (Views.hvSigned b)
      }

instance LeiosCrypto c => PraosProtocolSupportsNode (Leios c) where
  type PraosProtocolSupportsNodeCrypto (Leios c) = c
  getPraosNonces _prx = getPraosNonces (Proxy @(Praos c)) . leiosStatePraos
  getOpCertCounters _prx = getOpCertCounters (Proxy @(Praos c)) . leiosStatePraos

-- | Crossing into Leios carries the Praos state over whole; the announcement
-- starts empty, since no header of the protocol being left could have carried
-- one.
instance TranslateProto (Praos c) (Leios c) where
  translateLedgerView _ = id
  translateChainDepState _ praos =
    LeiosState{leiosStatePraos = praos, leiosStateAnnouncement = SNothing}

-- | Composed out of the two translations either side of it, rather than
-- repeating the projections.
instance TranslateProto (TPraos c) (Leios c) where
  translateLedgerView _ =
    translateLedgerView (Proxy @(Praos c, Leios c))
      . translateLedgerView (Proxy @(TPraos c, Praos c))

  translateChainDepState _ =
    translateChainDepState (Proxy @(Praos c, Leios c))
      . translateChainDepState (Proxy @(TPraos c, Praos c))
