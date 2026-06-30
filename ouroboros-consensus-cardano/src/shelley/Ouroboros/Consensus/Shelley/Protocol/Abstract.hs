{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Commonality between multiple protocols.
--
--   Everything in this module is indexed on the protocol (or the crypto),
--   rather than on the block type. This allows it to be imported in
--   @Ouroboros.Consensus.Shelley.Ledger.Block@.
module Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( ProtoCrypto
  , ProtocolHeaderSupportsEnvelope (..)
  , default_pHeaderLeiosContainsCert
  , ProtocolHeaderSupportsKES (..)
  , ProtocolHeaderSupportsProtocol (..)
  , ShelleyHash (..)
  , ShelleyProtocol
  , ShelleyProtocolHeader
  ) where

import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.VRF (OutputVRF)
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.Hashes
  ( EraIndependentBlockBody
  , EraIndependentBlockHeader
  , HASH
  )
import Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import Cardano.Protocol.Crypto (Crypto, VRF)
import Cardano.Protocol.TPraos.BHeader (PrevHash)
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (SlotNo, WithOrigin)
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import Control.Monad.Except (Except)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import LeiosDemoTypes (EbAnnouncement)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Protocol.Abstract
  ( CanBeLeader
  , ChainDepState
  , ConsensusConfig
  , ConsensusProtocol
  , IsLeader
  , LedgerView
  , ValidateView
  )
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import Ouroboros.Consensus.Protocol.Praos.Common (HasMaxMajorProtVer)
import Ouroboros.Consensus.Protocol.Signed (SignedHeader)
import Ouroboros.Consensus.Util.Condense (Condense (..))

{-------------------------------------------------------------------------------
  Crypto
-------------------------------------------------------------------------------}

type family ProtoCrypto proto :: Type

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ShelleyHash = ShelleyHash
  { unShelleyHash :: Hash.Hash HASH EraIndependentBlockHeader
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, NFData)
  deriving anyclass NoThunks

deriving newtype instance ToCBOR ShelleyHash

deriving newtype instance FromCBOR ShelleyHash

instance Serialise ShelleyHash where
  encode = toCBOR
  decode = fromCBOR

instance Condense ShelleyHash where
  condense = show . unShelleyHash

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

-- | Shelley header, determined by the associated protocol.
type family ShelleyProtocolHeader proto = (sh :: Type) | sh -> proto

-- | Indicates that the header (determined by the protocol) supports " Envelope
-- " functionality. Envelope functionality refers to the minimal functionality
-- required to construct a chain.
class
  ( Eq (EnvelopeCheckError proto)
  , NoThunks (EnvelopeCheckError proto)
  , Show (EnvelopeCheckError proto)
  ) =>
  ProtocolHeaderSupportsEnvelope proto
  where
  pHeaderHash :: ShelleyProtocolHeader proto -> ShelleyHash
  pHeaderPrevHash :: ShelleyProtocolHeader proto -> PrevHash
  pHeaderBodyHash :: ShelleyProtocolHeader proto -> Hash.Hash HASH EraIndependentBlockBody
  pHeaderSlot :: ShelleyProtocolHeader proto -> SlotNo
  pHeaderBlock :: ShelleyProtocolHeader proto -> BlockNo
  pHeaderSize :: ShelleyProtocolHeader proto -> Natural
  pHeaderBlockSize :: ShelleyProtocolHeader proto -> Natural

  -- | Whether the header records that this block's body carries a Leios
  -- certificate (i.e. it is a "CertRB"). Praos reads 'hbLeiosContainsCert';
  -- protocols/headers without Leios support (e.g. TPraos) use
  -- 'default_pHeaderLeiosContainsCert'. Used by 'blockMatchesHeader' to check
  -- the header/body envelope.
  pHeaderLeiosContainsCert :: ShelleyProtocolHeader proto -> Bool

  type EnvelopeCheckError proto :: Type

  -- | Carry out any protocol-specific envelope checks. For example, this might
  -- check things like maximum header size.
  envelopeChecks ::
    ConsensusConfig proto ->
    LedgerView proto ->
    ShelleyProtocolHeader proto ->
    Except (EnvelopeCheckError proto) ()

-- | The 'pHeaderLeiosContainsCert' for protocols/headers without Leios support:
-- a header that cannot carry a Leios certificate never claims to. Instances
-- that have no Leios support should define
-- @'pHeaderLeiosContainsCert' = 'default_pHeaderLeiosContainsCert'@ explicitly.
default_pHeaderLeiosContainsCert :: ShelleyProtocolHeader proto -> Bool
default_pHeaderLeiosContainsCert = const False

-- | `ProtocolHeaderSupportsKES` describes functionality common to protocols
--    using key evolving signature schemes. This includes verifying the header
--    integrity (e.g. validating the KES signature), as well as constructing the
--    header (made specific to KES-using protocols through the need to handle
--    the hot key).
class ProtocolHeaderSupportsKES proto where
  -- | Extract the "slots per KES period" value from the protocol config.
  --
  --   Note that we do not require `ConsensusConfig` in 'verifyHeaderIntegrity'
  --   since that function is also invoked with 'StorageConfig'.
  configSlotsPerKESPeriod :: ConsensusConfig proto -> Word64

  -- | Verify that the signature on a header is correct and valid.
  verifyHeaderIntegrity ::
    -- | Slots per KES period
    Word64 ->
    ShelleyProtocolHeader proto ->
    Bool

  mkHeader ::
    forall crypto m.
    (Crypto crypto, Monad m, crypto ~ ProtoCrypto proto) =>
    HotKey crypto m ->
    CanBeLeader proto ->
    IsLeader proto ->
    -- | Slot no
    SlotNo ->
    -- | Block no
    BlockNo ->
    -- | Hash of the previous block
    PrevHash ->
    -- | Hash of the block body to include in the header
    Hash.Hash HASH EraIndependentBlockBody ->
    -- | Size of the block body
    Int ->
    -- | Protocol version
    ProtVer ->
    -- | Optional Leios EB announcement. Only used by Praos.
    Maybe EbAnnouncement ->
    -- | Whether this block's body carries a Leios certificate (i.e. it is a
    -- "CertRB"). Only used by Praos, where it is recorded in the header
    -- ('hbLeiosContainsCert') so a CertRB is recognisable from its header
    -- alone and the header/body envelope can be checked.
    Bool ->
    m (ShelleyProtocolHeader proto)

  -- | Extract the most recently announced (and not yet certified) Leios EB
  -- from the protocol's chain-dep state, alongside the slot in which it
  -- was announced. 'Nothing' for protocols that do not carry Leios info
  -- (TPraos); 'Just' for Praos when an announcement has been seen on
  -- chain. Used at forge time to decide whether to embed a Leios
  -- certificate in the block body.
  protocolStateLeiosInfo ::
    proxy proto ->
    ChainDepState proto ->
    Maybe (EbAnnouncement, WithOrigin SlotNo)
  protocolStateLeiosInfo _ _ = Nothing

-- | ProtocolHeaderSupportsProtocol` provides support for the concrete
--   block header to support the `ConsensusProtocol` itself.
class ProtocolHeaderSupportsProtocol proto where
  type CannotForgeError proto :: Type

  protocolHeaderView ::
    ShelleyProtocolHeader proto -> ValidateView proto

  pHeaderIssuer ::
    ShelleyProtocolHeader proto -> VKey BlockIssuer
  pHeaderIssueNo ::
    ShelleyProtocolHeader proto -> Word64

  -- | A VRF value in the header, used to choose between otherwise equally
  -- preferable chains.
  pTieBreakVRFValue ::
    ShelleyProtocolHeader proto -> OutputVRF (VRF (ProtoCrypto proto))

{-------------------------------------------------------------------------------
  Key constraints
-------------------------------------------------------------------------------}

class
  ( ConsensusProtocol proto
  , Typeable (ShelleyProtocolHeader proto)
  , ProtocolHeaderSupportsEnvelope proto
  , ProtocolHeaderSupportsKES proto
  , ProtocolHeaderSupportsProtocol proto
  , Serialise (ChainDepState proto)
  , SignedHeader (ShelleyProtocolHeader proto)
  , HasMaxMajorProtVer proto
  ) =>
  ShelleyProtocol proto
