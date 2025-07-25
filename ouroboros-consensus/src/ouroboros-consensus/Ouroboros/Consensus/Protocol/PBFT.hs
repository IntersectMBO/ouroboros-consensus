{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 908
-- GHC is a bit pickier for data family instances, but trying to remove this
-- one forces us to reorganize the Protocol.* modules. TODO eventually.
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

module Ouroboros.Consensus.Protocol.PBFT
  ( PBft
  , PBftCanBeLeader (..)
  , PBftFields (..)
  , PBftIsLeader (..)
  , PBftLedgerView (..)
  , PBftParams (..)
  , PBftTiebreakerView (..)
  , PBftSignatureThreshold (..)
  , mkPBftTiebreakerView
  , pbftWindowExceedsThreshold
  , pbftWindowSize

    -- * Forging
  , forgePBftFields

    -- * Classes
  , PBftCrypto (..)
  , PBftMockCrypto
  , PBftMockVerKeyHash (..)
  , PBftValidateView (..)
  , pbftValidateBoundary
  , pbftValidateRegular

    -- * CannotForge
  , PBftCannotForge (..)
  , pbftCheckCanForge

    -- * Type instances
  , ConsensusConfig (..)
  , Ticked (..)

    -- * Exported for tracing errors
  , PBftValidationErr (..)
  ) where

import Cardano.Crypto.DSIGN.Class
import Cardano.Ledger.BaseTypes (unNonZero)
import Codec.Serialise (Serialise (..))
import qualified Control.Exception as Exn
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.ResourceRegistry ()
import Data.Bifunctor (first)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.PBFT.Crypto
import Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import Ouroboros.Consensus.Protocol.Signed
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  Fields that PBFT requires present in a block
-------------------------------------------------------------------------------}

data PBftFields c toSign = PBftFields
  { pbftIssuer :: VerKeyDSIGN (PBftDSIGN c)
  -- ^ The actual issuer of a block
  , pbftGenKey :: VerKeyDSIGN (PBftDSIGN c)
  -- ^ The stakeholder on whose behalf the block is being issued
  , pbftSignature :: SignedDSIGN (PBftDSIGN c) toSign
  }
  deriving Generic

deriving instance PBftCrypto c => Show (PBftFields c toSign)
deriving instance PBftCrypto c => Eq (PBftFields c toSign)

instance (PBftCrypto c, Typeable toSign) => NoThunks (PBftFields c toSign)

-- use generic instance

-- | Part of the header that we validate
data PBftValidateView c
  = -- | Regular block
    --
    -- Regular blocks are signed, and so we need to validate them.
    -- We also need to know the slot number of the block
    forall signed.
    Signable (PBftDSIGN c) signed =>
    PBftValidateRegular
      (PBftFields c signed)
      signed
      (ContextDSIGN (PBftDSIGN c))
  | -- | Boundary block (EBB)
    --
    -- EBBs are not signed and they do not affect the consensus state.
    PBftValidateBoundary

-- | Convenience constructor for 'PBftValidateView' for regular blocks
pbftValidateRegular ::
  ( SignedHeader hdr
  , Signable (PBftDSIGN c) (Signed hdr)
  ) =>
  ContextDSIGN (PBftDSIGN c) ->
  (hdr -> PBftFields c (Signed hdr)) ->
  (hdr -> PBftValidateView c)
pbftValidateRegular contextDSIGN getFields hdr =
  PBftValidateRegular
    (getFields hdr)
    (headerSigned hdr)
    contextDSIGN

-- | Convenience constructor for 'PBftValidateView' for boundary blocks
pbftValidateBoundary :: hdr -> PBftValidateView c
pbftValidateBoundary _hdr = PBftValidateBoundary

-- | Part of the header required for chain selection between chains with an
-- equal block number at their tip.
--
-- EBBs share a block number with regular blocks, and so for chain selection
-- we need to know if a block is an EBB or not (because a chain ending on an
-- EBB with a particular block number is longer than a chain on a regular
-- block with that same block number).
newtype PBftTiebreakerView = PBftTiebreakerView
  { pbftTiebreakerViewIsEBB :: IsEBB
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks
  deriving ChainOrder via SimpleChainOrder PBftTiebreakerView

mkPBftTiebreakerView :: GetHeader blk => Header blk -> PBftTiebreakerView
mkPBftTiebreakerView hdr =
  PBftTiebreakerView
    { pbftTiebreakerViewIsEBB = headerToIsEBB hdr
    }

instance Ord PBftTiebreakerView where
  compare (PBftTiebreakerView lIsEBB) (PBftTiebreakerView rIsEBB) =
    -- If the block numbers are the same, check if one of them is an EBB.
    -- An EBB has the same block number as the block before it, so the
    -- chain ending with an EBB is actually longer than the one ending
    -- with a regular block.
    score lIsEBB `compare` score rIsEBB
   where
    score :: IsEBB -> Int
    score IsEBB = 1
    score IsNotEBB = 0

{-------------------------------------------------------------------------------
  Block forging
-------------------------------------------------------------------------------}

forgePBftFields ::
  forall c toSign.
  ( PBftCrypto c
  , Signable (PBftDSIGN c) toSign
  ) =>
  -- | Construct DSIGN context given 'pbftGenKey'
  (VerKeyDSIGN (PBftDSIGN c) -> ContextDSIGN (PBftDSIGN c)) ->
  IsLeader (PBft c) ->
  toSign ->
  PBftFields c toSign
forgePBftFields contextDSIGN PBftIsLeader{..} toSign =
  Exn.assert (issuer == deriveVerKeyDSIGN pbftIsLeaderSignKey) $
    PBftFields
      { pbftIssuer = issuer
      , pbftGenKey = genKey
      , pbftSignature = signature
      }
 where
  issuer = dlgCertDlgVerKey pbftIsLeaderDlgCert
  genKey = dlgCertGenVerKey pbftIsLeaderDlgCert
  ctxtDSIGN = contextDSIGN genKey
  signature = signedDSIGN ctxtDSIGN toSign pbftIsLeaderSignKey

{-------------------------------------------------------------------------------
  Information PBFT requires from the ledger
-------------------------------------------------------------------------------}

newtype PBftLedgerView c = PBftLedgerView
  { pbftDelegates :: Bimap (PBftVerKeyHash c) (PBftVerKeyHash c)
  -- ^ ProtocolParameters: map from genesis to delegate keys.
  }
  deriving Generic

deriving instance PBftCrypto c => NoThunks (PBftLedgerView c)

-- use generic instance

deriving instance Eq (PBftVerKeyHash c) => Eq (PBftLedgerView c)
deriving instance Show (PBftVerKeyHash c) => Show (PBftLedgerView c)

instance
  (Serialise (PBftVerKeyHash c), Ord (PBftVerKeyHash c)) =>
  Serialise (PBftLedgerView c)
  where
  encode (PBftLedgerView ds) = encode (Bimap.toList ds)
  decode = PBftLedgerView . Bimap.fromList <$> decode

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | Permissive BFT
--
-- As defined in https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
data PBft c

-- | Signature threshold. This represents the proportion of blocks in a
-- @pbftSignatureWindow@-sized window which may be signed by any single key.
newtype PBftSignatureThreshold = PBftSignatureThreshold
  { getPBftSignatureThreshold :: Double
  }
  deriving (Eq, Show, Generic, NoThunks)

-- | Protocol parameters
data PBftParams = PBftParams
  { pbftSecurityParam :: !SecurityParam
  -- ^ Security parameter
  --
  -- Although the protocol proper does not have such a security parameter,
  -- we insist on it.
  , pbftNumNodes :: !NumCoreNodes
  -- ^ Number of core nodes
  , pbftSignatureThreshold :: !PBftSignatureThreshold
  -- ^ Signature threshold
  --
  -- This bounds the proportion of the latest 'pbftSecurityParam'-many
  -- blocks which is allowed to be signed by any single key. The protocol
  -- proper is parameterized over the size of this window of recent blocks,
  -- but this implementation follows the specification by fixing that
  -- parameter to the ambient security parameter @k@.
  }
  deriving (Generic, NoThunks, Show)

-- | If we are a core node (i.e. a block producing node) we know which core
-- node we are, and we have the operational key pair and delegation certificate.
data PBftCanBeLeader c = PBftCanBeLeader
  { pbftCanBeLeaderCoreNodeId :: !CoreNodeId
  , pbftCanBeLeaderSignKey :: !(SignKeyDSIGN (PBftDSIGN c))
  , pbftCanBeLeaderDlgCert :: !(PBftDelegationCert c)
  }
  deriving Generic

instance PBftCrypto c => NoThunks (PBftCanBeLeader c)

-- | Information required to produce a block.
data PBftIsLeader c = PBftIsLeader
  { pbftIsLeaderSignKey :: !(SignKeyDSIGN (PBftDSIGN c))
  , pbftIsLeaderDlgCert :: !(PBftDelegationCert c)
  }
  deriving Generic

instance PBftCrypto c => NoThunks (PBftIsLeader c)

-- | (Static) node configuration
newtype instance ConsensusConfig (PBft c) = PBftConfig
  { pbftParams :: PBftParams
  }
  deriving (Generic, NoThunks)

-- Ticking has no effect on the PBFtState, but we do need the ledger view
data instance Ticked (PBftState c) = TickedPBftState
  { getPBftLedgerView :: LedgerView (PBft c)
  , getTickedPBftState :: PBftState c
  }

instance PBftCrypto c => ConsensusProtocol (PBft c) where
  type ValidationErr (PBft c) = PBftValidationErr c
  type ValidateView (PBft c) = PBftValidateView c
  type TiebreakerView (PBft c) = PBftTiebreakerView

  -- \| We require two things from the ledger state:
  --
  --   - Protocol parameters, for the signature window and threshold.
  --   - The delegation map.
  type LedgerView (PBft c) = PBftLedgerView c
  type IsLeader (PBft c) = PBftIsLeader c
  type ChainDepState (PBft c) = PBftState c
  type CanBeLeader (PBft c) = PBftCanBeLeader c

  protocolSecurityParam = pbftSecurityParam . pbftParams

  checkIsLeader
    PBftConfig{pbftParams}
    PBftCanBeLeader{..}
    (SlotNo n)
    _tickedChainDepState =
      -- We are the slot leader based on our node index, and the current
      -- slot number. Our node index depends which genesis key has delegated
      -- to us, see 'genesisKeyCoreNodeId'.
      if n `mod` numCoreNodes == i
        then
          Just
            PBftIsLeader
              { pbftIsLeaderSignKey = pbftCanBeLeaderSignKey
              , pbftIsLeaderDlgCert = pbftCanBeLeaderDlgCert
              }
        else
          Nothing
     where
      PBftParams{pbftNumNodes = NumCoreNodes numCoreNodes} = pbftParams
      CoreNodeId i = pbftCanBeLeaderCoreNodeId

  tickChainDepState _ lv _ = TickedPBftState lv

  updateChainDepState
    cfg
    toValidate
    slot
    (TickedPBftState (PBftLedgerView dms) state) =
      case toValidate of
        PBftValidateBoundary ->
          return state
        PBftValidateRegular PBftFields{..} signed contextDSIGN -> do
          -- Check that the issuer signature verifies, and that it's a delegate of a
          -- genesis key, and that genesis key hasn't voted too many times.
          case verifySignedDSIGN
            contextDSIGN
            pbftIssuer
            signed
            pbftSignature of
            Right () -> return ()
            Left err -> throwError $ PBftInvalidSignature (Text.pack err)

          -- FIXME confirm that non-strict inequality is ok in general.
          -- It's here because EBBs have the same slot as the first block of their
          -- epoch.
          unless (NotOrigin slot >= S.lastSignedSlot state) $
            throwError PBftInvalidSlot

          case Bimap.lookupR (hashVerKey pbftIssuer) dms of
            Nothing ->
              throwError $
                PBftNotGenesisDelegate
                  (hashVerKey pbftIssuer)
                  (PBftLedgerView dms)
            Just gk -> do
              let state' = append cfg params (slot, gk) state
              case pbftWindowExceedsThreshold params state' gk of
                Left n -> throwError $ PBftExceededSignThreshold gk n
                Right () -> return $! state'
     where
      params = pbftWindowParams cfg

  reupdateChainDepState
    cfg
    toValidate
    slot
    (TickedPBftState (PBftLedgerView dms) state) =
      case toValidate of
        PBftValidateBoundary -> state
        PBftValidateRegular PBftFields{pbftIssuer} _ _ ->
          case Bimap.lookupR (hashVerKey pbftIssuer) dms of
            Nothing ->
              error $
                show $
                  PBftNotGenesisDelegate
                    (hashVerKey pbftIssuer)
                    (PBftLedgerView dms)
            Just gk -> do
              let state' = append cfg params (slot, gk) state
              case pbftWindowExceedsThreshold params state' gk of
                Left n -> error $ show $ PBftExceededSignThreshold gk n
                Right () -> state'
     where
      params = pbftWindowParams cfg

{-------------------------------------------------------------------------------
  Internal: thin wrapper on top of 'PBftState'
-------------------------------------------------------------------------------}

-- | Parameters for the window check
data PBftWindowParams = PBftWindowParams
  { windowSize :: S.WindowSize
  -- ^ Window size
  , threshold :: Word64
  -- ^ Threshold (maximum number of slots anyone is allowed to sign)
  }

-- | Compute window check parameters from the node config
pbftWindowParams :: ConsensusConfig (PBft c) -> PBftWindowParams
pbftWindowParams PBftConfig{..} =
  PBftWindowParams
    { windowSize = winSize
    , threshold =
        floor $
          getPBftSignatureThreshold pbftSignatureThreshold * fromIntegral winSize
    }
 where
  PBftParams{..} = pbftParams
  winSize = pbftWindowSize pbftSecurityParam

-- | Window size used by PBFT
--
-- We set the window size to be equal to k.
pbftWindowSize :: SecurityParam -> S.WindowSize
pbftWindowSize (SecurityParam k) = S.WindowSize $ unNonZero k

-- | Does the number of blocks signed by this key exceed the threshold?
--
-- Returns @Just@ the number of blocks signed if exceeded.
pbftWindowExceedsThreshold ::
  PBftCrypto c =>
  PBftWindowParams ->
  PBftState c ->
  PBftVerKeyHash c ->
  Either Word64 ()
pbftWindowExceedsThreshold PBftWindowParams{..} st gk =
  if numSigned > threshold
    then Left numSigned
    else Right ()
 where
  numSigned = S.countSignedBy st gk

append ::
  PBftCrypto c =>
  ConsensusConfig (PBft c) ->
  PBftWindowParams ->
  (SlotNo, PBftVerKeyHash c) ->
  PBftState c ->
  PBftState c
append PBftConfig{} PBftWindowParams{..} =
  S.append windowSize . uncurry S.PBftSigner

{-------------------------------------------------------------------------------
  PBFT specific types
-------------------------------------------------------------------------------}

-- | NOTE: this type is stored in the state, so it must be in normal form to
-- avoid space leaks.
data PBftValidationErr c
  = PBftInvalidSignature !Text
  | PBftNotGenesisDelegate !(PBftVerKeyHash c) !(PBftLedgerView c)
  | -- | We record how many slots this key signed
    PBftExceededSignThreshold !(PBftVerKeyHash c) !Word64
  | PBftInvalidSlot
  deriving (Generic, NoThunks)

deriving instance PBftCrypto c => Show (PBftValidationErr c)
deriving instance PBftCrypto c => Eq (PBftValidationErr c)

{-------------------------------------------------------------------------------
  CannotForge
-------------------------------------------------------------------------------}

-- | Expresses that, whilst we believe ourselves to be a leader for this slot,
-- we are nonetheless unable to forge a block.
data PBftCannotForge c
  = -- | We cannot forge a block because we are not the current delegate of the
    -- genesis key we have a delegation certificate from.
    PBftCannotForgeInvalidDelegation !(PBftVerKeyHash c)
  | -- | We cannot lead because delegates of the genesis key we have a
    -- delegation from have already forged the maximum number of blocks in this
    -- signing window.
    PBftCannotForgeThresholdExceeded !Word64
  deriving Generic

deriving instance PBftCrypto c => Show (PBftCannotForge c)

instance PBftCrypto c => NoThunks (PBftCannotForge c)

-- use generic instance

pbftCheckCanForge ::
  forall c.
  PBftCrypto c =>
  ConsensusConfig (PBft c) ->
  PBftCanBeLeader c ->
  SlotNo ->
  Ticked (PBftState c) ->
  Either (PBftCannotForge c) ()
pbftCheckCanForge cfg PBftCanBeLeader{..} slot tickedChainDepState =
  case Bimap.lookupR dlgKeyHash dms of
    Nothing -> Left $ PBftCannotForgeInvalidDelegation dlgKeyHash
    Just gk ->
      first PBftCannotForgeThresholdExceeded $
        pbftWindowExceedsThreshold params (append cfg params (slot, gk) cds) gk
 where
  params = pbftWindowParams cfg

  dlgKeyHash :: PBftVerKeyHash c
  dlgKeyHash = hashVerKey . dlgCertDlgVerKey $ pbftCanBeLeaderDlgCert

  TickedPBftState (PBftLedgerView dms) cds = tickedChainDepState

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance PBftCrypto c => Condense (PBftFields c toSign) where
  condense PBftFields{..} = condense pbftSignature
