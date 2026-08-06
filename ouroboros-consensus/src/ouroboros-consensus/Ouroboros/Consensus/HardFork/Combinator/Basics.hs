{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- TODO: figure out how to move the 'IsPerasVote'/'IsPerasCert' next to the
-- definition of 'OneEraPerasVote' and 'OneEraPerasCert'.
{-# OPTIONS_GHC -Wno-orphans #-}

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
  , projectHFCPerasContext
  , injectHFCPerasEpochContext
  , projectHFCBoundedPerasEpochContext
  , injectHFCBoundedPerasEpochContext
  , castHFCPerasEpochContextResolverAtIndex
  , injectHFCPerasEpochContextResolver
  , EitherF (..)
  , mkEitherF
  , hcollect

    -- ** Convenience re-exports
  , EpochInfo
  , Except
  ) where

import Cardano.Slotting.EpochInfo
import Data.Bifunctor (bimap)
import Data.Functor.Product (Product (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.SOP (I (..), K (..), type (:.:) (..))
import Data.SOP.Constraint
import Data.SOP.Dict (Dict (..))
import qualified Data.SOP.Dict as Dict
import Data.SOP.Functors
import Data.SOP.Index (Index (..), himap, injectNS)
import Data.SOP.Match (matchNS)
import Data.SOP.Strict
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , BoostedBlock
  , IsPerasCert (..)
  , IsPerasError (..)
  , IsPerasVote (..)
  , PerasEpochContext (..)
  , PerasRoundNo
  , PerasVoteCollection (..)
  , PerasVoteCollectionWithQuorum (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , castPerasParams
  , unsafeAssumeQuorum
  , unsafePerasVoteCollection
  )
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime (..))
import Ouroboros.Consensus.Committee.Crypto
  ( ElectionId
  , VoteCandidate
  )
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork
  ( CanHardFork
  , EqualHashSizeOfHead
  , HashSizeOfHead
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
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HF
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerStateSupportsPeras (..))
import Ouroboros.Consensus.Peras.Context
  ( BoundedPerasEpochContext (..)
  , PerasEpochContextResolver (..)
  )
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Consensus.Util.RedundantConstraints (keepRedundantConstraint)

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
  NS helpers for Peras
-------------------------------------------------------------------------------}

-- | Ensure that all elements of a non-empty list of 'NS' values are in the same
-- era, collecting them into a single 'NS' containing a 'NonEmpty'.
ensureSameEraNonEmpty ::
  SListI xs =>
  NonEmpty (NS f xs) ->
  Maybe (NS (NonEmpty :.: f) xs)
ensureSameEraNonEmpty (x :| rest) =
  foldl go (Just $ hmap (Comp . (:| [])) x) rest
 where
  go Nothing _ =
    Nothing
  go (Just acc) ns =
    case matchNS acc ns of
      Left _mismatch ->
        Nothing
      Right nsPair ->
        Just
          . hmap
            ( \(Pair (Comp fs) f) ->
                Comp $
                  fs <> (f :| [])
            )
          $ nsPair

-- | Ensure that all elements of a non-empty map of 'NS' values are in the same
-- era, collecting them into a single 'NS' containing a 'NEMap'.
ensureSameEraNonEmptyMap ::
  ( Ord k
  , All Top xs
  ) =>
  NEMap k (NS f xs) ->
  Maybe (NS (NEMap k :.: f) xs)
ensureSameEraNonEmptyMap neMap =
  case ensureSameEraNonEmpty keyValPairs of
    Nothing ->
      Nothing
    Just ns ->
      Just
        . hmap
          ( \(Comp neKeyValPairs) ->
              Comp
                . NEMap.fromList
                . fmap (\(Pair (K k) v) -> (k, v))
                $ neKeyValPairs
          )
        $ ns
 where
  keyValPairs =
    fmap (\(k, v) -> hmap (\v' -> Pair (K k) v') v)
      . NEMap.toList
      $ neMap

-- | Ensure that two 'NS' values are in the same era, pairing them together.
-- Returns 'Left ParamsEraMismatch' if they are from different eras.
ensureSameEraPair ::
  ( NS f xs
  , NS g xs
  ) ->
  Maybe (NS (Product f g) xs)
ensureSameEraPair (l, r) =
  case matchNS l r of
    Left _mismatch ->
      Nothing
    Right nsQueryResAndLedgerView ->
      Just nsQueryResAndLedgerView

-- | Align an 'NP' and an 'NS' of the same length, pairing them together.
alignNPWithNS ::
  All Top xs =>
  NP f xs ->
  NS g xs ->
  NS (Product f g) xs
alignNPWithNS =
  hzipWith Pair

-- | A wrapper for 'Either' that is a functor in its last argument.
newtype EitherF f g x = EitherF {unEitherF :: Either (f x) (g x)}

-- | Construct an 'EitherF' from two functions and an 'Either' value.
mkEitherF :: (a -> f x) -> (b -> g x) -> Either a b -> EitherF f g x
mkEitherF f g = EitherF . bimap f g

-- | Collect an 'NS' of 'EitherF' values into an 'Either' of 'NS' values.
hcollect ::
  All Top xs =>
  NS (EitherF f g) xs ->
  Either (NS f xs) (NS g xs)
hcollect ns =
  hcollapse (himap f ns)
 where
  f idx (EitherF (Left fx)) = K $ Left $ injectNS idx fx
  f idx (EitherF (Right gx)) = K $ Right $ injectNS idx gx

{-------------------------------------------------------------------------------
  HFC injection/projection helpers for Peras
-------------------------------------------------------------------------------}

-- | Downcast a 'Point' of the hard fork block to a 'Point' of a single era
-- by decoding the raw hash via 'fromShortRawHash'. Used when delegating
-- operations that take a 'Point' argument to a single-era implementation.
downcastHardForkPoint ::
  forall blk xs.
  ( SingleEraBlock blk
  , HashSize (HardForkBlock xs) ~ HashSize blk
  ) =>
  Point (HardForkBlock xs) ->
  Point blk
downcastHardForkPoint = \case
  GenesisPoint ->
    GenesisPoint
  BlockPoint s (OneEraHash h) ->
    -- The constraints ensure that the hash sizes match, so we can safely use
    -- the unsafe variant of 'fromShortRawHash' here.
    BlockPoint s (unsafeFromShortRawHash (Proxy @blk) h)
 where
  _ = keepRedundantConstraint (Proxy @(HashSize (HardForkBlock xs) ~ HashSize blk))

-- | Upcast a 'Point' from a single era into a 'Point' of the hard fork block
-- by encoding the raw hash via 'toShortRawHash'. Used by accessor instances
-- to return 'Point (HardForkBlock xs)' from single-era point values.
upcastToHardForkPoint ::
  forall blk xs.
  SingleEraBlock blk =>
  Point blk ->
  Point (HardForkBlock xs)
upcastToHardForkPoint = \case
  GenesisPoint ->
    GenesisPoint
  BlockPoint s h ->
    BlockPoint s (OneEraHash (toShortRawHash (Proxy @blk) h))

-- | Project a 'PerasVoteCollectionWithQuorum' of the hard fork block into a
-- 'NS' of 'PerasVoteCollectionWithQuorum' of the single era blocks.
projectHFCPerasVoteCollectionWithQuorum ::
  All SingleEraBlock xs =>
  PerasVoteCollectionWithQuorum (HardForkBlock xs) ->
  Maybe (NS PerasVoteCollectionWithQuorum xs)
projectHFCPerasVoteCollectionWithQuorum hfcCollection =
  let votes = pvcVotes $ forgetQuorum hfcCollection
      neMapNs = projectHFCWatValidatedPerasVote <$> votes
   in case ensureSameEraNonEmptyMap neMapNs of
        Nothing -> Nothing
        Just nsNeMap ->
          Just $
            hcmap
              proxySingle
              ( \(Comp compedValMap) ->
                  unsafeAssumeQuorum $
                    unsafePerasVoteCollection
                      ((\(Comp v) -> v) <$> compedValMap)
              )
              nsNeMap

-- NOTE: this assumes PerasParams are the same for all eras.
--
-- In the future, @pecParams@ will be an @NP@ of @PerasParams@.
projectHFCPerasContext ::
  All Top xs =>
  PerasEpochContext (HardForkBlock xs) ->
  NS PerasEpochContext xs
projectHFCPerasContext PerasEpochContext{pecCommittee, pecParams} =
  hmap
    ( \(WrapPerasVotingCommittee committee) ->
        PerasEpochContext
          { pecCommittee = committee
          , pecParams = castPerasParams pecParams
          }
    )
    . getOneEraPerasVotingCommittee
    $ pecCommittee

injectHFCPerasEpochContext ::
  All Top xs =>
  NS PerasEpochContext xs ->
  PerasEpochContext (HardForkBlock xs)
injectHFCPerasEpochContext nsContext =
  PerasEpochContext
    { pecCommittee =
        OneEraPerasVotingCommittee
          . hmap (WrapPerasVotingCommittee . pecCommittee)
          $ nsContext
    , pecParams =
        hcollapse
          . hmap (K . castPerasParams . pecParams)
          $ nsContext
    }

-- | Project a 'BoundedPerasEpochContext' of the hard fork block into a 'NS' of
-- 'BoundedPerasEpochContext' of the single era blocks.
projectHFCBoundedPerasEpochContext ::
  All Top xs =>
  BoundedPerasEpochContext (HardForkBlock xs) ->
  NS BoundedPerasEpochContext xs
projectHFCBoundedPerasEpochContext
  BoundedPerasEpochContext
    { startPerasRoundNo
    , endPerasRoundNo
    , epochContext
    } =
    hmap
      ( \(WrapPerasVotingCommittee committee) ->
          BoundedPerasEpochContext
            { startPerasRoundNo =
                startPerasRoundNo
            , endPerasRoundNo =
                endPerasRoundNo
            , epochContext =
                PerasEpochContext
                  { pecCommittee = committee
                  , pecParams = castPerasParams (pecParams epochContext)
                  }
            }
      )
      . getOneEraPerasVotingCommittee
      $ pecCommittee epochContext

-- | Inject a 'NS' of 'BoundedPerasEpochContext' of the single era blocks into a
-- 'BoundedPerasEpochContext' of the hard fork block.
injectHFCBoundedPerasEpochContext ::
  All Top xs =>
  NS BoundedPerasEpochContext xs ->
  BoundedPerasEpochContext (HardForkBlock xs)
injectHFCBoundedPerasEpochContext nsBoundedContext =
  BoundedPerasEpochContext
    { startPerasRoundNo =
        hcollapse
          . hmap (K . startPerasRoundNo)
          $ nsBoundedContext
    , endPerasRoundNo =
        hcollapse
          . hmap (K . endPerasRoundNo)
          $ nsBoundedContext
    , epochContext =
        injectHFCPerasEpochContext $
          hmap epochContext nsBoundedContext
    }

-- | Try to cast a 'PerasEpochContextResolver' of a 'HardForkBlock' into a
-- 'PerasEpochContextResolver' of the single era block represented by the given
-- index.
--
-- A 'PerasEpochContextResolver' is made of two maybe-like values, one for the
-- context of the current epoch, and one for the context of the past epoch. The
-- current epoch has to be in the current era, i.e. the era represented by the
-- given index; so the current context cast into the requested single era must
-- succeed for the whole operation to be succesful.
--
-- However, the past epoch may be in the past era under normal conditions. So
-- the cast of the previous epoch context into the era block represented by the
-- given index should be allowed to fail gracefully. So when it doesn't match
-- the requested index, the previous context is simply discarded and replaced
-- with a 'HF.NoPerasEnabled'.
--
-- Note that when the current context is 'HF.NoPerasEnabled', and the previous
-- one is 'HF.PerasEnabled cPrev' with 'cPrev' incompatible with the requested
-- era; the output of the cast is a:
-- 'PerasEpochContextResolver HF.NoPerasEnabled HF.NoPerasEnabled',
-- which is no functionally different from a:
-- 'PerasEpochContextResolverError', since it won't be able to resolve anything.
castHFCPerasEpochContextResolverAtIndex ::
  All Top xs =>
  Index xs blk ->
  PerasEpochContextResolver (HardForkBlock xs) ->
  PerasEpochContextResolver blk
castHFCPerasEpochContextResolverAtIndex idx = \case
  PerasEpochContextResolverError err ->
    PerasEpochContextResolverError err
  PerasEpochContextResolver peCurrentBoundedContext pePrevBoundedContext ->
    case (peCurrentBoundedContext, pePrevBoundedContext) of
      (HF.PerasEnabled currentBoundedContext, HF.PerasEnabled prevBoundedContext) ->
        case ensureSameEraPair
          ( getIndex idx
          , projectHFCBoundedPerasEpochContext currentBoundedContext
          ) of
          Nothing ->
            eraMismatchErrorResolver
          Just nsIdxCurrPair ->
            hcollapse $
              hmap
                ( \(Pair Refl currentBoundedContext') ->
                    K
                      $ PerasEpochContextResolver
                        (HF.PerasEnabled currentBoundedContext')
                      $ case ensureSameEraPair
                        ( getIndex idx
                        , projectHFCBoundedPerasEpochContext prevBoundedContext
                        ) of
                        Nothing ->
                          -- The current context is from the right era, but the
                          -- previous one is from a different one. Instead of
                          -- erroring out, we just discard the previous context.
                          HF.NoPerasEnabled
                        Just nsIdxPrevPair ->
                          hcollapse $
                            hmap
                              ( \(Pair Refl prevBoundedContext') ->
                                  K $ HF.PerasEnabled prevBoundedContext'
                              )
                              nsIdxPrevPair
                )
                nsIdxCurrPair
      (HF.PerasEnabled currentBoundedContext, HF.NoPerasEnabled) ->
        case ensureSameEraPair
          ( getIndex idx
          , projectHFCBoundedPerasEpochContext currentBoundedContext
          ) of
          Nothing ->
            eraMismatchErrorResolver
          Just nsIdxCurrPair ->
            hcollapse $
              hmap
                ( \(Pair Refl currentBoundedContext') ->
                    K $
                      PerasEpochContextResolver
                        (HF.PerasEnabled currentBoundedContext')
                        HF.NoPerasEnabled
                )
                nsIdxCurrPair
      (HF.NoPerasEnabled, HF.PerasEnabled prevBoundedContext) ->
        case ensureSameEraPair
          ( getIndex idx
          , projectHFCBoundedPerasEpochContext prevBoundedContext
          ) of
          Nothing ->
            -- The current context is for an epoch/era where Peras is disabled,
            -- and the previous context is from a different era than the
            -- requested one, so we just return an empty resolver.
            --
            -- TODO: Should we error out instead?
            -- It would probably give the same end result.
            PerasEpochContextResolver
              HF.NoPerasEnabled
              HF.NoPerasEnabled
          Just nsIdxPrevPair ->
            hcollapse $
              hmap
                ( \(Pair Refl prevBoundedContext') ->
                    K $
                      PerasEpochContextResolver
                        HF.NoPerasEnabled
                        (HF.PerasEnabled prevBoundedContext')
                )
                nsIdxPrevPair
      (HF.NoPerasEnabled, HF.NoPerasEnabled) ->
        PerasEpochContextResolver
          HF.NoPerasEnabled
          HF.NoPerasEnabled
 where
  eraMismatchErrorResolver =
    PerasEpochContextResolverError $
      unlines
        [ "projectHFCPerasEpochContextResolver: currentBoundedContext"
        , " is not in the same era as the supplied index"
        ]

-- | Inject a 'NS' of 'PerasEpochContextResolver' of the single era blocks into
-- a 'PerasEpochContextResolver' of the hard fork block.
injectHFCPerasEpochContextResolver ::
  All Top xs =>
  NS PerasEpochContextResolver xs ->
  PerasEpochContextResolver (HardForkBlock xs)
injectHFCPerasEpochContextResolver =
  hcollapse
    . himap
      ( \idx resolver ->
          case resolver of
            PerasEpochContextResolverError err ->
              K $ PerasEpochContextResolverError err
            PerasEpochContextResolver currBoundedContext prevBoundedContext ->
              let hfcCurrentBoundedContext =
                    injectHFCBoundedPerasEpochContext
                      . injectNS idx
                      <$> currBoundedContext
                  hfcPrevBoundedContext =
                    injectHFCBoundedPerasEpochContext
                      . injectNS idx
                      <$> prevBoundedContext
               in K $
                    PerasEpochContextResolver
                      hfcCurrentBoundedContext
                      hfcPrevBoundedContext
      )

injectHFCValidatedPerasVote ::
  All Top xs =>
  NS ValidatedPerasVote xs ->
  ValidatedPerasVote (HardForkBlock xs)
injectHFCValidatedPerasVote ns =
  ValidatedPerasVote
    { vpvVote = OneEraPerasVote (hmap (WrapPerasVote . vpvVote) ns)
    , vpvVoteWeight = hcollapse (hmap (K . vpvVoteWeight) ns)
    }

injectHFCValidatedPerasCert ::
  All Top xs =>
  NS ValidatedPerasCert xs ->
  ValidatedPerasCert (HardForkBlock xs)
injectHFCValidatedPerasCert ns =
  ValidatedPerasCert
    { vpcCert = OneEraPerasCert (hmap (WrapPerasCert . vpcCert) ns)
    , vpcCertBoost = hcollapse (hmap (K . vpcCertBoost) ns)
    }

projectHFCValidatedPerasVote ::
  All Top xs =>
  ValidatedPerasVote (HardForkBlock xs) ->
  NS ValidatedPerasVote xs
projectHFCValidatedPerasVote
  ValidatedPerasVote
    { vpvVote
    , vpvVoteWeight
    } =
    hmap
      ( \(WrapPerasVote v) ->
          ValidatedPerasVote
            { vpvVote = v
            , vpvVoteWeight
            }
      )
      . getOneEraPerasVote
      $ vpvVote

projectHFCWatValidatedPerasVote ::
  All Top xs =>
  WithArrivalTime (ValidatedPerasVote (HardForkBlock xs)) ->
  NS (WithArrivalTime :.: ValidatedPerasVote) xs
projectHFCWatValidatedPerasVote (WithArrivalTime arrivalTime validatedVote) =
  hmap (Comp . WithArrivalTime arrivalTime)
    . projectHFCValidatedPerasVote
    $ validatedVote

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

-- ** Type and class instances that are required for the 'BlockSupportsPeras'

-- instance of 'HardForkBlock'

type instance ElectionId (OneEraPerasCrypto xs) = PerasRoundNo
type instance BoostedBlock (OneEraPerasVote xs) = Point (HardForkBlock xs)
type instance BoostedBlock (OneEraPerasCert xs) = Point (HardForkBlock xs)
type instance VoteCandidate (OneEraPerasCrypto xs) = Point (HardForkBlock xs)

instance
  ( CanHardFork xs
  , All SingleEraBlock xs
  ) =>
  IsPerasVote (OneEraPerasVote xs) (HardForkBlock xs)
  where
  getPerasVoteRound =
    hcollapse
      . hcmap proxySingle (K . getPerasVoteRound . unwrapPerasVote)
      . getOneEraPerasVote
  getPerasVoteSeatIndex =
    hcollapse
      . hcmap proxySingle (K . getPerasVoteSeatIndex . unwrapPerasVote)
      . getOneEraPerasVote
  getPerasVoteBlock =
    hcollapse
      . hcmap proxySingle (K . upcastToHardForkPoint . getPerasVotePoint . unwrapPerasVote)
      . getOneEraPerasVote

instance
  ( CanHardFork xs
  , All SingleEraBlock xs
  ) =>
  IsPerasCert (OneEraPerasCert xs) (HardForkBlock xs)
  where
  getPerasCertRound =
    hcollapse
      . hcmap proxySingle (K . getPerasCertRound . unwrapPerasCert)
      . getOneEraPerasCert
  getPerasCertBlock =
    hcollapse
      . hcmap proxySingle (K . upcastToHardForkPoint . getPerasCertPoint . unwrapPerasCert)
      . getOneEraPerasCert

instance
  ( CanHardFork xs
  , All SingleEraBlock xs
  ) =>
  IsPerasError (HardForkPerasError xs) (HardForkBlock xs)
  where
  -- NOTE: in practice this is never produced at the HFC level
  injectVotingCommitteeError _ = HardForkPerasErrorCommitteeError

  -- NOTE: in practice this is never produced at the HFC level
  injectConversionError _ = HardForkPerasErrorConversionError

  -- NOTE: in practice this is never produced at the HFC level
  injectQuorumNotReachedError _ = HardForkPerasErrorQuorumNotReachedError

  -- NOTE: in practice this is never produced at the HFC level
  injectCertificateTooLargeError _ _ = HardForkPerasErrorCertificateTooLargeError

-- ** 'BlockSupportsPeras' instance for 'HardForkBlock'

class
  ( SingleEraBlock blk
  , EqualHashSizeOfHead xs blk
  ) =>
  SingleEraBlockWithHashSizeOfHead xs blk

instance
  ( SingleEraBlock blk
  , EqualHashSizeOfHead xs blk
  ) =>
  SingleEraBlockWithHashSizeOfHead xs blk

proofSingleEraBlockWithHashSizeOfHead ::
  forall xs.
  ( All SingleEraBlock xs
  , All (EqualHashSizeOfHead xs) xs
  ) =>
  Dict (All (SingleEraBlockWithHashSizeOfHead xs)) xs
proofSingleEraBlockWithHashSizeOfHead =
  Dict.mapAll
    (\Dict -> Dict)
    ( Dict.zipAll
        (Dict :: Dict (All SingleEraBlock) xs)
        (Dict :: Dict (All (EqualHashSizeOfHead xs)) xs)
    )

instance
  ( StandardHash (HardForkBlock xs)
  , HashSize (HardForkBlock xs) ~ HashSizeOfHead xs
  , CanHardFork xs
  ) =>
  BlockSupportsPeras (HardForkBlock xs)
  where
  type PerasVote (HardForkBlock xs) = OneEraPerasVote xs
  type PerasCert (HardForkBlock xs) = OneEraPerasCert xs
  type PerasError (HardForkBlock xs) = HardForkPerasError xs
  type PerasCrypto (HardForkBlock xs) = OneEraPerasCrypto xs
  type PerasVotingCommitteeScheme (HardForkBlock xs) = OneEraPerasVotingCommitteeScheme xs

  forgePerasVoteIfEligible context poolId privKey roundNo point =
    let
      nsPrivKeyContext =
        alignNPWithNS
          (getPerEraPerasPrivateKey privKey)
          (projectHFCPerasContext context)
     in
      case proofSingleEraBlockWithHashSizeOfHead @xs of
        Dict ->
          bimap
            (HardForkPerasErrorOneEraPerasError . OneEraPerasError)
            (fmap injectHFCValidatedPerasVote . hsequence')
            . hcollect
            . hcmap
              (Proxy @(SingleEraBlockWithHashSizeOfHead xs))
              ( \(Pair (WrapPerasPrivateKey privKey') context') ->
                  mkEitherF
                    WrapPerasError
                    Comp
                    $ forgePerasVoteIfEligible
                      context'
                      poolId
                      privKey'
                      roundNo
                      (downcastHardForkPoint point)
              )
            $ nsPrivKeyContext

  verifyPerasVote context vote =
    case ensureSameEraPair
      ( projectHFCPerasContext context
      , getOneEraPerasVote vote
      ) of
      Nothing ->
        Left HardForkPerasErrorEraMismatch
      Just nsContextVote ->
        bimap
          (HardForkPerasErrorOneEraPerasError . OneEraPerasError)
          injectHFCValidatedPerasVote
          . hcollect
          . hcmap
            proxySingle
            ( \(Pair context' (WrapPerasVote vote')) ->
                mkEitherF
                  WrapPerasError
                  id
                  $ verifyPerasVote context' vote'
            )
          $ nsContextVote

  forgePerasCert context collection =
    case projectHFCPerasVoteCollectionWithQuorum collection of
      Nothing ->
        Left HardForkPerasErrorEraMismatch
      Just nsCollection ->
        case ensureSameEraPair
          ( projectHFCPerasContext context
          , nsCollection
          ) of
          Nothing -> Left HardForkPerasErrorEraMismatch
          Just nsContextCollection ->
            bimap
              (HardForkPerasErrorOneEraPerasError . OneEraPerasError)
              injectHFCValidatedPerasCert
              . hcollect
              . hcmap
                proxySingle
                ( \(Pair context' collection') ->
                    mkEitherF
                      WrapPerasError
                      id
                      $ forgePerasCert context' collection'
                )
              $ nsContextCollection

  verifyPerasCert context cert =
    case ensureSameEraPair
      ( projectHFCPerasContext context
      , getOneEraPerasCert cert
      ) of
      Nothing ->
        Left HardForkPerasErrorEraMismatch
      Just nsContextCert ->
        bimap
          (HardForkPerasErrorOneEraPerasError . OneEraPerasError)
          injectHFCValidatedPerasCert
          . hcollect
          . hcmap
            proxySingle
            ( \(Pair context' (WrapPerasCert cert')) ->
                mkEitherF
                  WrapPerasError
                  id
                  $ verifyPerasCert context' cert'
            )
          $ nsContextCert

  getPerasCertInBlock (HardForkBlock (OneEraBlock nsBlock)) =
    fmap OneEraPerasCert
      . hsequence'
      . hcmap
        proxySingle
        ( \(I block) ->
            Comp
              . fmap WrapPerasCert
              . getPerasCertInBlock
              $ block
        )
      $ nsBlock

  readPerasPrivateKeyFromEnv _proxy =
    fmap PerEraPerasPrivateKey $
      hsequence' $
        hcpure proxySingle dispatchReadKey
   where
    dispatchReadKey ::
      forall blk.
      SingleEraBlock blk =>
      (Either String :.: WrapPerasPrivateKey) blk
    dispatchReadKey =
      Comp
        . fmap WrapPerasPrivateKey
        . readPerasPrivateKeyFromEnv
        $ Proxy @blk

{-------------------------------------------------------------------------------
  LedgerSupportsPeras
-------------------------------------------------------------------------------}

instance CanHardFork xs => LedgerStateSupportsPeras (LedgerState (HardForkBlock xs)) where
  getPoolDistr =
    hcollapse
      . hcmap proxySingle (K . getPoolDistr . unFlip)
      . State.tip
      . hardForkLedgerStatePerEra
