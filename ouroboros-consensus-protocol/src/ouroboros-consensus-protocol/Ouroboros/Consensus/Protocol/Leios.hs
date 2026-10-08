{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Leios: an overlay on Praos.
--
-- Ranking blocks are Praos blocks, so most of this delegates to
-- "Ouroboros.Consensus.Protocol.Praos". What differs is the header body the KES
-- signature covers.
module Ouroboros.Consensus.Protocol.Leios
  ( Leios
  , LeiosCrypto
  , AnnouncedBy (..)
  , LeiosState (..)
  , ConsensusConfig (..)
  , Ticked (..)
  , LeiosHeaderView
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
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as SL
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

-- | 'PraosCrypto', plus signing the Leios header body.
class
  ( PraosCrypto c
  , KES.Signable (KES c) (Leios.HeaderBody c)
  ) =>
  LeiosCrypto c

instance LeiosCrypto StandardCrypto

-- | Praos's configuration, wrapped only because 'ConsensusConfig' is a data
-- family.
newtype instance ConsensusConfig (Leios c) = LeiosConfig
  { leiosPraosConfig :: ConsensusConfig (Praos c)
  }
  deriving Generic

instance LeiosCrypto c => NoThunks (ConsensusConfig (Leios c))

instance HasMaxMajorProtVer (Leios c) where
  protoMaxMajorPV = protoMaxMajorPV . leiosPraosConfig

-- | 'PraosState' and the announcement a certificate is validated against.
data LeiosState = LeiosState
  { leiosStatePraos :: !PraosState
  , leiosStateAnnouncement :: !(StrictMaybe AnnouncedBy)
  -- ^ What the most recently applied header announced; one announcing nothing
  -- clears it.
  }
  deriving (Generic, Show, Eq)

instance NoThunks LeiosState

instance ToCBOR LeiosState where
  toCBOR = encode

instance FromCBOR LeiosState where
  fromCBOR = decode

-- | Versioned independently of the 'PraosState' encoding it nests.
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

-- | The era only supplies a serialisation version, and neither field's encoding
-- varies by version, so 'ShelleyEra' pins the lowest one, as 'PraosState' does.
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

-- | The 'Views.HeaderView'' that signs the Leios header body.
type LeiosHeaderView crypto = Views.HeaderView' (Leios.HeaderBody crypto) crypto

instance LeiosCrypto c => ConsensusProtocol (Leios c) where
  type ChainDepState (Leios c) = LeiosState
  type IsLeader (Leios c) = PraosIsLeader c
  type CanBeLeader (Leios c) = PraosCanBeLeader c
  type TiebreakerView (Leios c) = PraosTiebreakerView c
  type LedgerView (Leios c) = Views.PraosLedgerView
  type ValidationErr (Leios c) = PraosValidationErr c
  type ValidateView (Leios c) = LeiosHeaderView c

  -- None of these reads a header, so Praos's methods apply.
  protocolSecurityParam = protocolSecurityParam @(Praos c) . leiosPraosConfig

  checkIsLeader cfg cbl slot tcs =
    checkIsLeader @(Praos c) (leiosPraosConfig cfg) cbl slot $
      TickedPraosState
        { tickedPraosStateChainDepState = leiosStatePraos (tickedLeiosStateChainDepState tcs)
        , tickedPraosStateLedgerView = tickedLeiosStateLedgerView tcs
        }

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
  -- signs the Leios body.
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

-- | The announcement starts empty: no Praos header could have carried one.
instance TranslateProto (Praos c) (Leios c) where
  translateLedgerView _ = id
  translateChainDepState _ praos =
    LeiosState{leiosStatePraos = praos, leiosStateAnnouncement = SNothing}

instance TranslateProto (TPraos c) (Leios c) where
  translateLedgerView _ =
    translateLedgerView (Proxy @(Praos c, Leios c))
      . translateLedgerView (Proxy @(TPraos c, Praos c))

  translateChainDepState _ =
    translateChainDepState (Proxy @(Praos c, Leios c))
      . translateChainDepState (Proxy @(TPraos c, Praos c))
