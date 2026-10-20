{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Various things common to iterations of the Praos protocol.
module Ouroboros.Consensus.Protocol.Praos.Common
  ( MaxMajorProtVer (..)
  , HasMaxMajorProtVer (..)
  , PraosCanBeLeader (..)
  , PraosTiebreakerView (..)
  , VRFTiebreakerFlavor (..)
  , PraosReasonForSwitch (..)

    -- * node support
  , PraosCredentialsSource (..)
  , KESEnvelopeError (..)
  , KESFileLoadError (..)
  , KESFileLoadReason (..)
  , PraosNonces (..)
  , PraosProtocolSupportsNode (..)
  , instantiatePraosCredentials
  ) where

import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.KES.EnvelopeLocked
  ( KESEnvelopeError (..)
  , LockedKESEnvelope (..)
  , finalizeLockedKESEnvelope
  , readLockedKESEnvelope
  )
import Cardano.Crypto.KES.Locked (deserialiseSignKeyKESFromLockedBuf)
import Cardano.Crypto.Libsodium.Memory (withMLockedForeignPtr)
import Cardano.Crypto.VRF
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.KESAgent.KES.Crypto as Agent
import Cardano.Ledger.BaseTypes (Nonce)
import qualified Cardano.Ledger.BaseTypes as SL
import Cardano.Ledger.Binary (FromCBOR, ToCBOR)
import Cardano.Ledger.Keys (DSIGN, KeyHash, KeyRole (BlockIssuer))
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (Crypto, KES, VRF)
import qualified Cardano.Protocol.TPraos.OCert as OCert
import Cardano.Slotting.Slot (SlotNo)
import Control.Monad (unless, when)
import qualified Control.Tracer as Tracer
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Praos.AgentClient
import Ouroboros.Consensus.Util.IOLike (Exception, MonadCatch (..), MonadThrow (..))

-- | The maximum major protocol version.
--
-- This refers to the largest __ledger__ version that this node supports.
--
-- Once the ledger protocol version (as reported by the ledger state)
-- exceeds this version we will consider all blocks invalid. This is
-- called the "obsolete node check" (see the 'ObsoleteNode' error
-- constructor).
--
-- Major ledger protocol versions are used to trigger both intra and
-- inter era hard forks, which can potentially change the set of
-- ledger rules that are applied.
--
-- Minor ledger protocol versions were intended to signal soft forks
-- but they're currently unused, and they're irrelevant for the
-- consensus logic.
--
-- For Cardano mainnet, the Shelley era has major protocol version
-- __2__.  For more details, see [this
-- table](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md)
newtype MaxMajorProtVer = MaxMajorProtVer
  { getMaxMajorProtVer :: SL.Version
  }
  deriving (Eq, Show, Generic)
  deriving newtype (NoThunks, ToCBOR, FromCBOR)

class HasMaxMajorProtVer proto where
  protoMaxMajorPV :: ConsensusConfig proto -> MaxMajorProtVer

-- | View of the tip of a header fragment for deciding between chains of equal
-- length.
data PraosTiebreakerView c = PraosTiebreakerView
  { ptvSlotNo :: SlotNo
  , ptvIssuer :: SL.VKey BlockIssuer
  , ptvIssueNo :: Word64
  , ptvTieBreakVRF :: VRF.OutputVRF (VRF c)
  }
  deriving (Show, Eq, Generic, NoThunks)

data PraosReasonForSwitch c
  = HigherOCert (Comparing Word64)
  | VRFTiebreak (Comparing (OutputVRF (VRF c)))
  deriving Show

-- | When to compare the VRF tiebreakers.
data VRFTiebreakerFlavor
  = -- | Always compare the VRF tiebreakers. This is the behavior of all eras
    -- before Conway. Once mainnet has transitioned to Conway, we can remove
    -- this option. (The honest /historical/ Ouroboros chain cannot rely on
    -- tiebreakers to win, so /retroactively/ disabling the tiebreaker won't
    -- matter.)
    UnrestrictedVRFTiebreaker
  | -- | Only compare the VRF tiebreakers when the slot numbers differ by at
    -- most the given number of slots.
    --
    -- The main motivation is as follows:
    --
    -- When two blocks A and B with the same block number differ in their slot
    -- number by more than Δ (the maximum message delay from Praos), say
    -- @slot(A) + Δ < slot(B)@, the issuer of B should have been able to mint a
    -- block with a block number higher than A (eg by minting on top of A) under
    -- normal circumstances. The reason for this not being the case might have
    -- been due to A being sent very late, or due to the issuer of B ignoring A
    -- (intentionally, or due to poor configuration/resource provision). In any
    -- case, we do not want to allow the block that was diffused later to still
    -- win by having a better VRF tiebreaker. This makes it less likely for
    -- properly configured pools to lose blocks because of poorly configured
    -- pools.
    RestrictedVRFTiebreaker SlotNo
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- Used to implement the 'Ord' and 'ChainOrder' instances for Praos.
comparePraos ::
  VRFTiebreakerFlavor ->
  PraosTiebreakerView c ->
  PraosTiebreakerView c ->
  ShouldSwitch (PraosReasonForSwitch c)
comparePraos tiebreakerFlavor ours cand =
  case ( issueNoArmed ours cand
       , (compare `on` ptvIssueNo) ours cand
       , vrfArmed ours cand
       , (compare `on` Down . ptvTieBreakVRF) ours cand
       ) of
    -- IssueNo armed: compare OCert issue numbers
    (True, LT, _, _) -> ShouldSwitch (HigherOCert (Comparing (ptvIssueNo ours) (ptvIssueNo cand)))
    (True, GT, _, _) -> ShouldNotSwitch GT
    -- IssueNo armed and equal issue numbers, VRF armed: use VRF tiebreaker
    (True, EQ, True, GT) -> ShouldNotSwitch GT -- ours has better VRF
    (True, EQ, True, EQ) -> ShouldNotSwitch EQ
    (True, EQ, True, LT) -> ShouldSwitch (VRFTiebreak (Comparing (ptvTieBreakVRF ours) (ptvTieBreakVRF cand)))
    -- IssueNo armed and equal, VRF not armed: no tiebreaker
    (True, EQ, False, _) -> ShouldNotSwitch EQ
    -- IssueNo not armed, VRF armed: use VRF tiebreaker
    (False, _, True, GT) -> ShouldNotSwitch GT -- ours has better VRF
    (False, _, True, EQ) -> ShouldNotSwitch EQ
    (False, _, True, LT) -> ShouldSwitch (VRFTiebreak (Comparing (ptvTieBreakVRF ours) (ptvTieBreakVRF cand)))
    -- IssueNo not armed, VRF not armed: no tiebreaker
    (False, _, False, _) -> ShouldNotSwitch EQ
 where
  -- Only compare the issue numbers when the issuers and slots are identical.
  -- Note that this case implies the VRFs also coincide.
  issueNoArmed v1 v2 =
    ptvSlotNo v1 == ptvSlotNo v2
      && ptvIssuer v1 == ptvIssuer v2

  -- Whether to do a VRF comparison.
  vrfArmed v1 v2 = case tiebreakerFlavor of
    UnrestrictedVRFTiebreaker -> True
    RestrictedVRFTiebreaker maxDist ->
      slotDist (ptvSlotNo v1) (ptvSlotNo v2) <= maxDist

  slotDist :: SlotNo -> SlotNo -> SlotNo
  slotDist s t
    -- slot numbers are unsigned, so have to take care with subtraction
    | s >= t = s - t
    | otherwise = t - s

-- | We order between chains of equal length as follows:
--
-- 1. If the tip of each chain was issued by the same agent and they have the
--    same slot number, prefer the chain whose tip has the highest ocert issue
--    number.
--
-- 2. By a VRF value from the chain tip, with lower values preferred. See
--    @pTieBreakVRFValue@ for which one is used.
--
-- IMPORTANT: This is not a complete picture of the Praos chain order, do also
-- consult the documentation of 'ChainOrder'.
instance Crypto c => Ord (PraosTiebreakerView c) where
  compare x y =
    case comparePraos UnrestrictedVRFTiebreaker x y of
      ShouldSwitch{} -> LT
      ShouldNotSwitch o -> o

-- | IMPORTANT: This is not a 'SimpleChainOrder'; rather, there are
-- 'PraosTiebreakerView's @a, b@ such that @a < b@, but @'not' $
-- 'preferCandidate' cfg a b@, namely for @cfg = 'RestrictedVRFTiebreaker'@.
--
-- === Rules
--
-- Concretely, we have @'preferCandidate' cfg ours cand@ (where @ours@ and
-- @cand@ have equal length) based on the following lexicographical criteria:
--
-- 1. If the tip of each chain was issued by the same agent and had the same
--    slot number, then we prefer the candidate if it has a higher ocert issue
--    number.
--
--     Note that this condition is equivalent to the VRFs being identical, as
--     the VRF is a deterministic function of the issuer VRF key, the slot and
--     the epoch nonce, and VRFs are collision-resistant.
--
-- 2. Depending on the 'VRFTiebreakerFlavor':
--
--     * If 'UnrestrictedVRFTiebreaker': Compare via a VRF value from the chain
--       tip, with lower values preferred. See @pTieBreakVRFValue@ for which one
--       is used.
--
--     * If @'RestrictedVRFTiebreaker' maxDist@: Only do the VRF comparison (as
--       in the previous step) if the slot numbers differ by at most @maxDist@.
--
-- === Non-transitivity of 'RestrictedVRFTiebreaker'
--
-- When using @cfg = 'RestrictedVRFTiebreaker' maxDist@, the chain order is not
-- transitive. As an example, suppose @maxDist = 5@ and consider three
-- 'PraosTiebreakerView's with pairwise different issuers and, as well as
--
-- +------+---+---+---+
-- |      | a | b | c |
-- +======+===+===+===+
-- | Slot | 0 | 3 | 6 |
-- +------+---+---+---+
-- | VRF  | 3 | 2 | 1 |
-- +------+---+---+---+
--
-- Then we have @'preferCandidate' cfg a b@ and @'preferCandidate' b c@, but
-- __not__ @'preferCandidate' a c@ (despite @a < c@).
--
-- === Rationale for the rules
--
-- 1. Consider the scenario where the hot key of a block issuer was compromised,
--    and the attacker is now minting blocks using that identity. The actual
--    block issuer can use their cold key to issue a new hot key with a higher
--    opcert issue number and set up a new pool. Due to this tiebreaker rule,
--    the blocks minted by that pool will take precedence (allowing the actual
--    block issuer to decide on eg the block contents and the predecessor) over
--    blocks with the same block and slot number minted by the attacker, and
--    they will end up on the honest chain quickly, which means that the
--    adversary can't extend any chain containing such a block as it would
--    violate the monotonicity requirement on opcert issue numbers.
--
--     See "3.7 Block Validity and Operational Key Certificates" in "Design
--     Specification for Delegation and Incentives in Cardano" by Kant et al for
--     more context.
--
-- 2. The main motivation to do VRF comparisons is to avoid the "Frankfurt
--    problem":
--
--     With only the first two rules for the chain order, almost all blocks with
--     equal block number are equally preferrable. Consider two block issuers
--     minting blocks in very nearby slots. As we never change our selection
--     from one chain to an equally preferrable one, the first block to arrive
--     at another pool is the one to be adopted, and will be extended the next
--     time the pool is elected if no blocks with a higher block number arrive
--     in the meantime. We observed that this effectively incentivizes block
--     producers to concentrate geographically (historically, in Frankfurt) in
--     order to minimize their diffusion times. This works against the goal of
--     geographic decentralisation.
--
--     Also, with the VRF tiebreaker, a block with a somewhat lower propagation
--     speed has a random chance to be selected instead of the one that arrived
--     first by pools before the next block is forged.
--
--     See 'VRFTiebreakerFlavor' for more context on the exact conditions under
--     which the VRF comparison takes place.
instance Crypto c => ChainOrder (PraosTiebreakerView c) where
  type ChainOrderConfig (PraosTiebreakerView c) = VRFTiebreakerFlavor
  type ReasonForSwitch (PraosTiebreakerView c) = PraosReasonForSwitch c
  preferCandidate cfg ours cand = comparePraos cfg ours cand

data PraosCanBeLeader c = PraosCanBeLeader
  { praosCanBeLeaderColdVerKey :: !(SL.VKey BlockIssuer)
  -- ^ Stake pool cold key or genesis stakeholder delegate cold key.
  , praosCanBeLeaderSignKeyVRF :: !(SignKeyVRF (VRF c))
  , praosCanBeLeaderCredentialsSource :: !(PraosCredentialsSource c)
  -- ^ How to obtain KES credentials (ocert + sign key)
  }
  deriving Generic

instance
  (NoThunks (SignKeyVRF (VRF c)), NoThunks (KES.UnsoundPureSignKeyKES (KES c)), Crypto c) =>
  NoThunks (PraosCanBeLeader c)

-- | Defines a method for obtaining Praos credentials (opcert + KES signing
-- key).
data PraosCredentialsSource c where
  -- | Pass an opcert and sign key directly. This uses
  -- 'KES.UnsoundPureSignKeyKES', which does not provide mlocking guarantees,
  -- violating the rule that KES secrets must never be stored on disk, but
  -- allows the sign key to be loaded from a local file. This method is
  -- provided for backwards compatibility.
  PraosCredentialsUnsound ::
    OCert.OCert c -> KES.UnsoundPureSignKeyKES (KES c) -> PraosCredentialsSource c
  -- | Connect to a KES agent listening on a service socket at the given path.
  PraosCredentialsAgent ::
    Agent.DSIGN (ACrypto c) ~ DSIGN => FilePath -> PraosCredentialsSource c
  -- | Pass an opcert and the path of a KES signing key file: a text envelope
  -- of type @KesSigningKey_\<algorithm\>@ (what @cardano-cli@ writes, the
  -- key at evolution 0) or @KesSigningKeyWithPeriod_\<algorithm\>@ (an
  -- evolved key together with the absolute KES period it is valid for, as
  -- persisted by the node). The file is read when the credentials are
  -- instantiated, straight into mlocked memory; no copy of the key ever
  -- reaches the GHC heap. The key must be the certificate's hot key and must
  -- really be at the evolution the file claims; the 'HotKey.HotKey' then
  -- starts at that evolution. Carrying a path rather than a key keeps the
  -- key's ownership with the hot key, which erases it on every evolution and
  -- on finalisation.
  --
  -- The optional 'KESPersistHook' is invoked by the hot key after every
  -- evolution with a serialised copy of the evolved key (see
  -- 'HotKey.KESPersistCallback'); the node uses it to persist the evolved key
  -- back to the file, so that a restart does not start from a stale, earlier
  -- evolution.
  PraosCredentialsFile ::
    OCert.OCert c -> FilePath -> Maybe KESPersistHook -> PraosCredentialsSource c

instance (NoThunks (KES.UnsoundPureSignKeyKES (KES c)), Crypto c) => NoThunks (PraosCredentialsSource c) where
  wNoThunks ctxt = \case
    PraosCredentialsUnsound oca k ->
      allNoThunks
        [ noThunks ctxt oca
        , noThunks ctxt k
        ]
    PraosCredentialsAgent fp -> noThunks ctxt fp
    PraosCredentialsFile oca fp hook ->
      allNoThunks
        [ noThunks ctxt oca
        , noThunks ctxt fp
        , noThunks ctxt hook
        ]

  showTypeOf _ = "PraosCredentialsSource"

-- | Why 'instantiatePraosCredentials' rejected a 'PraosCredentialsFile'.
-- Never carries file contents.
data KESFileLoadReason
  = -- | The file's envelope could not be used, see 'KESEnvelopeError'.
    KESFileEnvelope !KESEnvelopeError
  | -- | The verification key derived from the file's signing key is not the
    -- hot key named in the operational certificate, or the key does not sign
    -- for that hot key at any evolution.
    KESFileKeyMismatch
  | -- | The absolute KES period a period-carrying file claims lies before the
    -- start period of the operational certificate: the file's period and the
    -- certificate's start period.
    KESFilePeriodBeforeOCert !Word64 !Word64
  | -- | The absolute KES period a period-carrying file claims lies at or
    -- beyond the end of the certificate's range: the evolution it amounts to
    -- (the period minus the certificate's start period) and the maximum
    -- number of evolutions.
    KESFileEvolutionTooHigh !Word64 !Word64
  | -- | The evolution the file claims is not the one the key is at: the
    -- claimed evolution and the evolution at which the key actually signs
    -- for the certificate's hot key. A sum-composition KES key signs valid
    -- signatures at exactly one evolution, so trusting the claim would make
    -- the node forge blocks the verifiers reject.
    KESFileStructureMismatch !Word64 !Word64
  deriving (Show, Eq)

-- | Thrown by 'instantiatePraosCredentials' for a 'PraosCredentialsFile'. It
-- is the only exception type the file case introduces; reading the file
-- itself can still throw the usual 'IOError's (for instance when the file
-- does not exist).
data KESFileLoadError = KESFileLoadError
  { kesFilePath :: FilePath
  , kesFileReason :: KESFileLoadReason
  }
  deriving Show

instance Exception KESFileLoadError

instantiatePraosCredentials ::
  forall m c.
  KESAgentContext c m =>
  Word64 ->
  Tracer.Tracer m KESAgentClientTrace ->
  PraosCredentialsSource c ->
  m (HotKey.HotKey c m)
instantiatePraosCredentials maxKESEvolutions _ (PraosCredentialsUnsound ocert skUnsound) = do
  sk <- KES.unsoundPureSignKeyKESToSoundSignKeyKES skUnsound
  let startPeriod :: OCert.KESPeriod
      startPeriod = OCert.ocertKESPeriod ocert

  HotKey.mkHotKey
    ocert
    sk
    startPeriod
    maxKESEvolutions
instantiatePraosCredentials maxKESEvolutions tr (PraosCredentialsAgent path) = do
  HotKey.mkDynamicHotKey
    maxKESEvolutions
    (Just $ runKESAgentClient tr path)
instantiatePraosCredentials maxKESEvolutions _ (PraosCredentialsFile ocert path persistHook) =
  withKESKeyFileLocked path $ \filePtr fileLen -> do
    -- Parse the envelope in place. Only offsets, lengths and the period leave
    -- the file buffer; the key bytes come back in a locked buffer of their
    -- own.
    envelope <-
      readLockedKESEnvelope algorithmName filePtr fileLen
        >>= either (throwLoad . KESFileEnvelope) pure

    -- Build the key from those bytes, then erase them.
    sk <-
      ( do
          let keyLen = lkeKeyLen envelope
          when (keyLen /= expectedKeyLen) $
            throwLoad (KESFileEnvelope (EnvelopeBadLength expectedKeyLen keyLen))
          withMLockedForeignPtr (lkeKey envelope) $ \key ->
            deserialiseSignKeyKESFromLockedBuf @(KES c) key keyLen
      )
        `finally` finalizeLockedKESEnvelope envelope

    -- From here on the key is live in locked memory: erase it on every
    -- failure, since nobody else will.
    (`onException` KES.forgetSignKeyKES sk) $ do
      vk <- KES.deriveVerKeyKES sk
      when (vk /= vkHot) $ throwLoad KESFileKeyMismatch

      -- The evolution the file claims: 0 for the type cardano-cli writes,
      -- the stored period relative to the certificate's start otherwise.
      claimed <- case lkePeriod envelope of
        Nothing -> pure 0
        Just period -> do
          when (period < start) $
            throwLoad (KESFilePeriodBeforeOCert period start)
          let evolution = period - start
          when (evolution >= maxKESEvolutions) $
            throwLoad (KESFileEvolutionTooHigh evolution maxKESEvolutions)
          pure evolution

      -- Cross-check the claim against the key itself. A sum-composition KES
      -- key holds the leaf key of exactly one evolution; signing at any
      -- other evolution selects the wrong subtree and does not verify. So
      -- the claim is right iff a signature at the claimed evolution
      -- verifies against the hot key. If it does not, find the evolution
      -- that does, so the error can say where the key really is.
      signsAtClaimed <- signsAt sk claimed
      unless signsAtClaimed $ do
        actual <- firstM (signsAt sk) [e | e <- [0 .. maxKESEvolutions - 1], e /= claimed]
        throwLoad $ maybe KESFileKeyMismatch (KESFileStructureMismatch claimed) actual

      HotKey.mkHotKeyAtEvolutionWithPersist
        (runKESPersistHook <$> persistHook)
        (fromIntegral claimed)
        ocert
        sk
        (OCert.ocertKESPeriod ocert)
        maxKESEvolutions
 where
  throwLoad :: forall a. KESFileLoadReason -> m a
  throwLoad = throwIO . KESFileLoadError path

  algorithmName :: String
  algorithmName = KES.algorithmNameKES (Proxy @(KES c))

  expectedKeyLen :: Int
  expectedKeyLen = fromIntegral (KES.signKeySizeKES (Proxy @(KES c)))

  vkHot :: KES.VerKeyKES (KES c)
  vkHot = OCert.ocertVkHot ocert

  -- The certificate's start period, in the width of the file's period.
  start :: Word64
  start = fromIntegral (OCert.unKESPeriod (OCert.ocertKESPeriod ocert))

  -- Does a signature made with the key at this evolution verify against the
  -- certificate's hot key? The message is fixed and public; the signature
  -- is discarded.
  signsAt :: KES.SignKeyKES (KES c) -> Word64 -> m Bool
  signsAt sk evolution = do
    let e = fromIntegral evolution
    sig <- KES.signKES () e probeMessage sk
    pure (KES.verifyKES () vkHot e probeMessage sig == Right ())

  probeMessage :: ByteString
  probeMessage = BC.pack "ouroboros-consensus KES signing key file: structural check"

  firstM :: forall a. (a -> m Bool) -> [a] -> m (Maybe a)
  firstM _ [] = pure Nothing
  firstM f (x : xs) = f x >>= \ok -> if ok then pure (Just x) else firstM f xs

-- | See 'PraosProtocolSupportsNode'
data PraosNonces = PraosNonces
  { candidateNonce :: !Nonce
  , epochNonce :: !Nonce
  , evolvingNonce :: !Nonce
  , labNonce :: !Nonce
  -- ^ Nonce constructed from the hash of the Last Applied Block
  , previousLabNonce :: !Nonce
  -- ^ Nonce corresponding to the LAB nonce of the last block of the previous
  -- epoch
  }

-- | The node has Praos-aware code that inspects nonces in order to support
-- some Cardano API queries that are crucial to the user exprience
--
-- The interface being used for that has grown and needs review, but we're
-- adding to it here under time pressure. See
-- <https://github.com/IntersectMBO/cardano-node/issues/3864>
class ConsensusProtocol p => PraosProtocolSupportsNode p where
  type PraosProtocolSupportsNodeCrypto p

  getPraosNonces :: proxy p -> ChainDepState p -> PraosNonces

  getOpCertCounters :: proxy p -> ChainDepState p -> Map (KeyHash BlockIssuer) Word64
