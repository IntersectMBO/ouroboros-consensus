{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.HardFork.Combinator.Basics
  ( -- * Hard fork protocol, block, and ledger state
    HardForkBlock (..)
  , HardForkProtocol
  , LedgerState (..)

    -- * Config
  , BlockConfig (..)
  , CodecConfig (..)
  , ConsensusConfig (..)
  , HardForkLedgerConfig (..)
  , StorageConfig (..)

    -- ** Functions on config
  , completeConsensusConfig'
  , completeConsensusConfig''
  , completeLedgerConfig'
  , completeLedgerConfig''
  , distribLedgerConfig
  , distribTopLevelConfig

    -- ** Convenience re-exports
  , EpochInfo
  , Except
  ) where

import Cardano.Slotting.EpochInfo
import Control.Monad ((<=<))
import Data.Kind (Type)
import Data.SOP (K (..), unI)
import Data.SOP.Constraint
import Data.SOP.Functors
import Data.SOP.Strict
import Data.Typeable
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  )
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork
  ( CanHardFork
  )
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
  ( SingleEraBlock
  , proxySingle
  )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import qualified Ouroboros.Consensus.HardFork.Combinator.State.Infra as State
import Ouroboros.Consensus.HardFork.Combinator.State.Instances ()
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsPeras
  ( ALedgerStateSupportsPeras (..)
  )
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe (unsafePerasBLSPrivateKeyFromEnv)
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import qualified Ouroboros.Consensus.Peras.Voting.V1 as V1
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (ShowProxy)
import Type.Reflection (someTypeRep)
import Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
  Hard fork protocol, block, and ledger state
-------------------------------------------------------------------------------}

data HardForkProtocol (xs :: [Type])

newtype HardForkBlock xs = HardForkBlock
  { getHardForkBlock :: OneEraBlock xs
  }
  deriving Show

instance Typeable xs => ShowProxy (HardForkBlock xs)

type instance BlockProtocol (HardForkBlock xs) = HardForkProtocol xs
type instance HeaderHash (HardForkBlock xs) = OneEraHash xs

newtype instance LedgerState (HardForkBlock xs) mk = HardForkLedgerState
  { hardForkLedgerStatePerEra :: HardForkState (Flip LedgerState mk) xs
  }

deriving stock instance
  (ShowMK mk, CanHardFork xs) =>
  Show (LedgerState (HardForkBlock xs) mk)
deriving stock instance
  (EqMK mk, CanHardFork xs) =>
  Eq (LedgerState (HardForkBlock xs) mk)
deriving newtype instance
  (NoThunksMK mk, CanHardFork xs) =>
  NoThunks (LedgerState (HardForkBlock xs) mk)

{-------------------------------------------------------------------------------
  Protocol config
-------------------------------------------------------------------------------}

data instance ConsensusConfig (HardForkProtocol xs) = HardForkConsensusConfig
  { hardForkConsensusConfigK :: !(SecurityParam)
  -- ^ The value of @k@ cannot change at hard fork boundaries
  , hardForkConsensusConfigShape :: !(History.Shape xs)
  -- ^ The shape of the hard fork
  --
  -- We require this in the consensus config because consensus might need
  -- access to 'EpochInfo', and in order to compute that, we need the
  -- 'EraParams' of all eras.
  , hardForkConsensusConfigPerEra :: !(PerEraConsensusConfig xs)
  -- ^ Config for each era
  }
  deriving stock Generic
  deriving anyclass NoThunks

{-------------------------------------------------------------------------------
  Block config
-------------------------------------------------------------------------------}

newtype instance BlockConfig (HardForkBlock xs) = HardForkBlockConfig
  { hardForkBlockConfigPerEra :: PerEraBlockConfig xs
  }
  deriving newtype NoThunks

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

newtype instance CodecConfig (HardForkBlock xs) = HardForkCodecConfig
  { hardForkCodecConfigPerEra :: PerEraCodecConfig xs
  }
  deriving newtype NoThunks

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

newtype instance StorageConfig (HardForkBlock xs) = HardForkStorageConfig
  { hardForkStorageConfigPerEra :: PerEraStorageConfig xs
  }
  deriving newtype NoThunks

{-------------------------------------------------------------------------------
  Ledger config
-------------------------------------------------------------------------------}

data HardForkLedgerConfig xs = HardForkLedgerConfig
  { hardForkLedgerConfigShape :: !(History.Shape xs)
  , hardForkLedgerConfigPerEra :: !(PerEraLedgerConfig xs)
  }
  deriving Generic

deriving instance Show (PerEraLedgerConfig xs) => Show (HardForkLedgerConfig xs)
instance CanHardFork xs => NoThunks (HardForkLedgerConfig xs)

type instance LedgerCfg LedgerState (HardForkBlock xs) = HardForkLedgerConfig xs

{-------------------------------------------------------------------------------
  Operations on config
-------------------------------------------------------------------------------}

completeLedgerConfig' ::
  forall blk.
  HasPartialLedgerConfig blk =>
  EpochInfo (Except PastHorizonException) ->
  WrapPartialLedgerConfig blk ->
  LedgerConfig blk
completeLedgerConfig' ei =
  completeLedgerConfig (Proxy @blk) ei
    . unwrapPartialLedgerConfig

completeLedgerConfig'' ::
  forall blk.
  HasPartialLedgerConfig blk =>
  EpochInfo (Except PastHorizonException) ->
  WrapPartialLedgerConfig blk ->
  WrapLedgerConfig blk
completeLedgerConfig'' ei =
  WrapLedgerConfig
    . completeLedgerConfig (Proxy @blk) ei
    . unwrapPartialLedgerConfig

completeConsensusConfig' ::
  forall blk.
  HasPartialConsensusConfig (BlockProtocol blk) =>
  EpochInfo (Except PastHorizonException) ->
  WrapPartialConsensusConfig blk ->
  ConsensusConfig (BlockProtocol blk)
completeConsensusConfig' ei =
  completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
    . unwrapPartialConsensusConfig

completeConsensusConfig'' ::
  forall blk.
  HasPartialConsensusConfig (BlockProtocol blk) =>
  EpochInfo (Except PastHorizonException) ->
  WrapPartialConsensusConfig blk ->
  WrapConsensusConfig blk
completeConsensusConfig'' ei =
  WrapConsensusConfig
    . completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
    . unwrapPartialConsensusConfig

distribLedgerConfig ::
  CanHardFork xs =>
  EpochInfo (Except PastHorizonException) ->
  LedgerConfig (HardForkBlock xs) ->
  NP WrapLedgerConfig xs
distribLedgerConfig ei cfg =
  hcmap
    proxySingle
    (completeLedgerConfig'' ei)
    (getPerEraLedgerConfig $ hardForkLedgerConfigPerEra cfg)

distribTopLevelConfig ::
  All SingleEraBlock xs =>
  EpochInfo (Except PastHorizonException) ->
  TopLevelConfig (HardForkBlock xs) ->
  NP TopLevelConfig xs
distribTopLevelConfig ei tlc =
  hcpure
    proxySingle
    ( fn_5
        ( \cfgConsensus cfgLedger cfgBlock cfgCodec cfgStorage ->
            mkTopLevelConfig
              (completeConsensusConfig' ei cfgConsensus)
              (completeLedgerConfig' ei cfgLedger)
              cfgBlock
              cfgCodec
              cfgStorage
              -- topLevelConfigCheckpoints is only used in validateEnvelope,
              -- where it comes from the TopLevelConfig of the HardForkBlock.
              --
              -- The checkpoints of the underlying blocks are not used.
              emptyCheckpointsMap
        )
    )
    `hap` ( getPerEraConsensusConfig $
              hardForkConsensusConfigPerEra (configConsensus tlc)
          )
    `hap` ( getPerEraLedgerConfig $
              hardForkLedgerConfigPerEra (configLedger tlc)
          )
    `hap` ( getPerEraBlockConfig $
              hardForkBlockConfigPerEra (configBlock tlc)
          )
    `hap` ( getPerEraCodecConfig $
              hardForkCodecConfigPerEra (configCodec tlc)
          )
    `hap` ( getPerEraStorageConfig $
              hardForkStorageConfigPerEra (configStorage tlc)
          )

{-------------------------------------------------------------------------------
  LedgerSupportsPeras
-------------------------------------------------------------------------------}

instance CanHardFork xs => ALedgerStateSupportsPeras (LedgerState (HardForkBlock xs) mk) where
  getPoolDistr =
    hcollapse
      . hcmap proxySingle (K . getPoolDistr . unFlip)
      . State.tip
      . hardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

-- TODO: we need to change the binary representation of votes and certs to carry
-- era-specific/versionning information, to allow future evolutions
instance
  ( StandardHash (HardForkBlock xs)
  , CanHardFork xs
  ) =>
  BlockSupportsPeras (HardForkBlock xs)
  where
  type PerasVote (HardForkBlock xs) = V1.PerasVote (HardForkBlock xs)
  type PerasCert (HardForkBlock xs) = V1.PerasCert (HardForkBlock xs)
  type PerasError (HardForkBlock xs) = V1.PerasError (HardForkBlock xs)
  type PerasCrypto (HardForkBlock xs) = BLS.PerasBLSCrypto
  type PerasVotingCommitteeScheme (HardForkBlock xs) = V1.PerasVotingCommitteeScheme

  getPerasCertInBlock =
    hcollapse
      . hcmap
        proxySingle
        (K . (unsafeCastPerasCertV1 <=< getPerasCertInBlock) . unI)
      . getOneEraBlock
      . getHardForkBlock

  readPerasPrivateKeyFromEnv _proxy = unsafePerasBLSPrivateKeyFromEnv

  blockDoesReallySupportsPeras _proxy = True

-- [TODO PERAS CERTS IN BLOCKS] this is a nasty hack
unsafeCastPerasCertV1 ::
  forall x xs.
  ( Typeable xs
  , Typeable (PerasCert x)
  ) =>
  PerasCert x ->
  Maybe (V1.PerasCert (HardForkBlock xs))
unsafeCastPerasCertV1 cert = do
  let xCertRep = someTypeRep (Proxy @(PerasCert x))
  let xsCertRep = someTypeRep (Proxy @(V1.PerasCert (HardForkBlock xs)))
  if typeRepTyCon xCertRep == typeRepTyCon xsCertRep
    then Just (unsafeCoerce cert)
    else Nothing

{-------------------------------------------------------------------------------
  ConvertRawHash
-------------------------------------------------------------------------------}

instance CanHardFork xs => ConvertRawHash (HardForkBlock xs) where
  toShortRawHash _ = getOneEraHash
  fromShortRawHash _ = OneEraHash
  hashSize _ = getSameValue hashSizes
   where
    hashSizes :: NP (K Word32) xs
    hashSizes = hcpure proxySingle hashSizeOne

    hashSizeOne :: forall blk. SingleEraBlock blk => K Word32 blk
    hashSizeOne = K $ hashSize (Proxy @blk)
