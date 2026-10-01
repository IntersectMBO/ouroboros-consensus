{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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
-- header changes the bytes the signature covers, so 'LeiosHeaderView' is a
-- 'HeaderView' over the Leios body rather than the Praos one. That is the only
-- reason 'updateChainDepState' and 'reupdateChainDepState' are written out here
-- instead of delegating --- they are the two methods that take a
-- 'ValidateView'.
--
-- Named after the ledger's @Cardano.Protocol.Leios.*@ modules, which is where
-- the Leios header lives.
module Ouroboros.Consensus.Protocol.Leios
  ( Leios
  , LeiosCrypto
  , ConsensusConfig (..)
  , LeiosHeaderView
  ) where

import qualified Cardano.Crypto.KES as KES
import Cardano.Protocol.Crypto (KES, StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
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

-- | The 'Views.HeaderView'' of Praos with Leios, which signs the Leios header
-- body: the Praos fields plus the two Leios ones.
type LeiosHeaderView crypto = Views.HeaderView' (Leios.HeaderBody crypto) crypto

instance LeiosCrypto c => ConsensusProtocol (Leios c) where
  -- TODO Track the announcement a certificate is validated against, which
  -- 'PraosState' does not carry.
  type ChainDepState (Leios c) = PraosState
  type IsLeader (Leios c) = PraosIsLeader c
  type CanBeLeader (Leios c) = PraosCanBeLeader c
  type TiebreakerView (Leios c) = PraosTiebreakerView c
  type LedgerView (Leios c) = Views.PraosLedgerView
  type ValidationErr (Leios c) = PraosValidationErr c
  type ValidateView (Leios c) = LeiosHeaderView c

  -- These read nothing from the header, and this protocol's chain-dep state and
  -- ledger view are Praos's, so the base protocol's methods apply unchanged.
  protocolSecurityParam = protocolSecurityParam @(Praos c) . leiosPraosConfig
  checkIsLeader = checkIsLeader @(Praos c) . leiosPraosConfig
  tickChainDepState = tickChainDepState @(Praos c) . leiosPraosConfig

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
    lv = tickedPraosStateLedgerView tcs
    cs = tickedPraosStateChainDepState tcs

  reupdateChainDepState cfg b slot tcs =
    reupdatePraosState
      (leiosPraosConfig cfg)
      b
      slot
      (tickedPraosStateChainDepState tcs)

instance LeiosCrypto c => PraosProtocolSupportsNode (Leios c) where
  type PraosProtocolSupportsNodeCrypto (Leios c) = c
  getPraosNonces _prx = getPraosNonces (Proxy @(Praos c))
  getOpCertCounters _prx = getOpCertCounters (Proxy @(Praos c))

-- | Crossing into Leios carries everything over: the chain-dep state and the
-- ledger view are the very same types.
instance TranslateProto (Praos c) (Leios c) where
  translateLedgerView _ = id
  translateChainDepState _ = id

-- | Composed out of the two translations either side of it, rather than
-- repeating the projections.
instance TranslateProto (TPraos c) (Leios c) where
  translateLedgerView _ = translateLedgerView (Proxy @(TPraos c, Praos c))
  translateChainDepState _ = translateChainDepState (Proxy @(TPraos c, Praos c))
