{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Witness isomorphism between @b@ and @HardForkBlock '[b]@
module Ouroboros.Consensus.HardFork.Combinator.Embed.Unary (
    Isomorphic (..)
  , inject'
  , project'
    -- * Dependent types
  , ProjHardForkQuery (..)
  , injNestedCtxt
  , injQuery
  , injQueryResult
  , projNestedCtxt
  , projQuery
  , projQuery'
  , projQueryResult
    -- * Convenience exports
  , I (..)
  , Proxy (..)
  ) where

import           Cardano.Slotting.EpochInfo
import           Data.Bifunctor (first)
import           Data.Coerce
import           Data.Kind (Constraint, Type)
import           Data.Proxy
import           Data.SOP.BasicFunctors
import           Data.SOP.Functors
import qualified Data.SOP.OptNP as OptNP
import           Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import           Data.Type.Equality
import           Data.Void
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Forging
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Projection/injection for a single block into degenerate HardForkBlock
-------------------------------------------------------------------------------}

class Isomorphic f where
  project :: NoHardForks blk => f (HardForkBlock '[blk]) -> f blk
  inject  :: NoHardForks blk => f blk -> f (HardForkBlock '[blk])

project' :: forall proxy f x y blk. (
              Isomorphic f
            , NoHardForks blk
            , Coercible x (f (HardForkBlock '[blk]))
            , Coercible y (f blk)
            )
         => proxy (f blk) -> x -> y
project' _ =
      (coerce :: f blk -> y)
    . project
    . (coerce :: x -> f (HardForkBlock '[blk]))

inject' :: forall proxy f x y blk. (
              Isomorphic f
            , NoHardForks blk
            , Coercible x (f blk)
            , Coercible y (f (HardForkBlock '[blk]))
            )
         => proxy (f blk) -> x -> y
inject' _ =
      (coerce :: f (HardForkBlock '[blk]) -> y)
    . inject
    . (coerce :: x -> f blk)

{-------------------------------------------------------------------------------
  Defaults (to ease implementation)
-------------------------------------------------------------------------------}

type IsomorphicUnary :: ((k -> Type) -> [k] -> Type) -> (k -> Type) -> k -> Type
newtype IsomorphicUnary h f a = IsomorphicUnary (f a)

instance
  ( IsSOPLike h
  , forall blk. Coercible (f (HardForkBlock '[blk])) (h f '[blk])
  ) => Isomorphic (IsomorphicUnary h f) where
  project ::
       forall blk.
       IsomorphicUnary h f (HardForkBlock '[blk])
    -> IsomorphicUnary h f blk
  project = coerce (fromSOPLike :: h f '[blk] -> f blk)

  inject ::
       forall blk.
       IsomorphicUnary h f blk
    -> IsomorphicUnary h f (HardForkBlock '[blk])
  inject = coerce (toSOPLike :: f blk -> h f '[blk])

type IsSOPLike :: ((k -> Type) -> [k] -> Type) -> Constraint
class IsSOPLike h where
  fromSOPLike :: h f '[a] -> f a
  toSOPLike :: f a -> h f '[a]

instance IsSOPLike NS where
  fromSOPLike = unZ
  toSOPLike = Z

instance IsSOPLike NP where
  fromSOPLike = hd
  toSOPLike = (:* Nil)

instance IsSOPLike HardForkState where
  fromSOPLike = State.fromTZ
  toSOPLike = HardForkState . Telescope.TZ . State.Current History.initBound

{-------------------------------------------------------------------------------
  Forwarding instances
-------------------------------------------------------------------------------}

instance Isomorphic ((->) a) where
  project f = coerce (project @I) . f
  inject  f = coerce (inject  @I) . f

{-------------------------------------------------------------------------------
  Simple instances
-------------------------------------------------------------------------------}

deriving via IsomorphicUnary NP BlockConfig   instance Isomorphic BlockConfig
deriving via IsomorphicUnary NP CodecConfig   instance Isomorphic CodecConfig
deriving via IsomorphicUnary NP StorageConfig instance Isomorphic StorageConfig

deriving via IsomorphicUnary NS GenTx                     instance Isomorphic GenTx
deriving via IsomorphicUnary NS Header                    instance Isomorphic Header
deriving via IsomorphicUnary NS I                         instance Isomorphic I
deriving via IsomorphicUnary NS WrapCannotForge           instance Isomorphic WrapCannotForge
deriving via IsomorphicUnary NS WrapForgeStateUpdateError instance Isomorphic WrapForgeStateUpdateError
deriving via IsomorphicUnary NS WrapGenTxId               instance Isomorphic WrapGenTxId
deriving via IsomorphicUnary NS WrapIsLeader              instance Isomorphic WrapIsLeader
deriving via IsomorphicUnary NS WrapTipInfo               instance Isomorphic WrapTipInfo
deriving via IsomorphicUnary NS WrapValidatedGenTx        instance Isomorphic WrapValidatedGenTx

deriving via IsomorphicUnary HardForkState (Flip LedgerState mk) instance Isomorphic (Flip LedgerState mk)
deriving via IsomorphicUnary HardForkState WrapChainDepState     instance Isomorphic WrapChainDepState

{-------------------------------------------------------------------------------
  Hash
-------------------------------------------------------------------------------}

instance Isomorphic WrapHeaderHash where
  project :: forall blk. ConvertRawHash blk
          => WrapHeaderHash (HardForkBlock '[blk]) -> WrapHeaderHash blk
  project =
        WrapHeaderHash
      . fromShortRawHash (Proxy @blk) . getOneEraHash
      . unwrapHeaderHash

  inject :: forall blk. ConvertRawHash blk
      => WrapHeaderHash blk -> WrapHeaderHash (HardForkBlock '[blk])
  inject =
        WrapHeaderHash
      . OneEraHash . toShortRawHash (Proxy @blk)
      . unwrapHeaderHash

instance Isomorphic ChainHash where
  project :: forall blk. NoHardForks blk
          => ChainHash (HardForkBlock '[blk]) -> ChainHash blk
  project GenesisHash   = GenesisHash
  project (BlockHash h) = BlockHash (project' (Proxy @(WrapHeaderHash blk)) h)

  inject :: forall blk. NoHardForks blk
         => ChainHash blk -> ChainHash (HardForkBlock '[blk])
  inject GenesisHash   = GenesisHash
  inject (BlockHash h) = BlockHash (inject' (Proxy @(WrapHeaderHash blk)) h)

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

-- | Projection/injection for 'TopLevelConfig'
--
-- NOTE: We do not define one for 'LedgerConfig' or 'ConsensusConfig', since
-- we need the 'EraParams' for their injections, which we can only derive if
-- we have the top-level config.
instance Isomorphic TopLevelConfig where
  project :: forall blk. NoHardForks blk
          => TopLevelConfig (HardForkBlock '[blk]) -> TopLevelConfig blk
  project tlc =
      mkTopLevelConfig
        (auxConsensus $ configConsensus tlc)
        (auxLedger    $ configLedger    tlc)
        (project      $ configBlock     tlc)
        (project      $ configCodec     tlc)
        (project      $ configStorage   tlc)
        emptyCheckpointsMap
    where
      ei :: EpochInfo (Except PastHorizonException)
      ei = immutableEpochInfo $ project tlc

      auxLedger :: LedgerConfig (HardForkBlock '[blk]) -> LedgerConfig blk
      auxLedger =
            completeLedgerConfig (Proxy @blk) ei
          . unwrapPartialLedgerConfig
          . hd
          . getPerEraLedgerConfig
          . hardForkLedgerConfigPerEra

      auxConsensus :: ConsensusConfig (BlockProtocol (HardForkBlock '[blk]))
                   -> ConsensusConfig (BlockProtocol blk)
      auxConsensus =
            completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
          . unwrapPartialConsensusConfig
          . hd
          . getPerEraConsensusConfig
          . hardForkConsensusConfigPerEra

  inject :: forall blk. NoHardForks blk
         => TopLevelConfig blk -> TopLevelConfig (HardForkBlock '[blk])
  inject tlc =
      mkTopLevelConfig
        (auxConsensus $ configConsensus tlc)
        (auxLedger    $ configLedger    tlc)
        (inject       $ configBlock     tlc)
        (inject       $ configCodec     tlc)
        (inject       $ configStorage   tlc)
        emptyCheckpointsMap
    where
      eraParams = immutableEraParams tlc

      auxLedger :: LedgerConfig blk -> LedgerConfig (HardForkBlock '[blk])
      auxLedger cfg = HardForkLedgerConfig {
            hardForkLedgerConfigShape  = History.singletonShape eraParams
          , hardForkLedgerConfigPerEra = PerEraLedgerConfig $
                 WrapPartialLedgerConfig (toPartialLedgerConfig (Proxy @blk) cfg )
              :* Nil
          }

      auxConsensus :: ConsensusConfig (BlockProtocol blk)
                   -> ConsensusConfig (BlockProtocol (HardForkBlock '[blk]))
      auxConsensus cfg = HardForkConsensusConfig {
            hardForkConsensusConfigK      = protocolSecurityParam cfg
          , hardForkConsensusConfigShape  = History.singletonShape eraParams
          , hardForkConsensusConfigPerEra = PerEraConsensusConfig $
                 WrapPartialConsensusConfig (toPartialConsensusConfig (Proxy @(BlockProtocol blk)) cfg)
              :* Nil
          }

{-------------------------------------------------------------------------------
  Various kinds of records
-------------------------------------------------------------------------------}

instance Isomorphic HeaderState where
  project :: forall blk. NoHardForks blk
          => HeaderState (HardForkBlock '[blk]) -> HeaderState blk
  project HeaderState{..} = HeaderState {
        headerStateTip      = project <$> headerStateTip
      , headerStateChainDep = project' (Proxy @(WrapChainDepState blk)) headerStateChainDep
      }

  inject :: forall blk. NoHardForks blk
         => HeaderState blk -> HeaderState (HardForkBlock '[blk])
  inject HeaderState{..} = HeaderState {
        headerStateTip      = inject <$> headerStateTip
      , headerStateChainDep = inject' (Proxy @(WrapChainDepState blk)) headerStateChainDep
      }

instance Isomorphic (FlipTickedLedgerState mk) where
  project =
        State.currentState
      . Telescope.fromTZ
      . getHardForkState
      . tickedHardForkLedgerStatePerEra
      . getFlipTickedLedgerState

  inject =
        FlipTickedLedgerState
      . TickedHardForkLedgerState TransitionImpossible
      . HardForkState
      . Telescope.TZ
      . State.Current History.initBound

instance Isomorphic (Flip ExtLedgerState mk) where
  project (Flip ExtLedgerState{..}) = Flip $ ExtLedgerState {
        ledgerState = unFlip $ project $ Flip ledgerState
      , headerState = project headerState
      }

  inject (Flip ExtLedgerState{..}) = Flip $ ExtLedgerState {
        ledgerState = unFlip $ inject $ Flip ledgerState
      , headerState = inject headerState
      }

instance Isomorphic AnnTip where
  project :: forall blk. NoHardForks blk => AnnTip (HardForkBlock '[blk]) -> AnnTip blk
  project (AnnTip s b nfo) = AnnTip s b (project' (Proxy @(WrapTipInfo blk)) nfo)

  inject (AnnTip s b nfo) = AnnTip s b (OneEraTipInfo (Z (WrapTipInfo nfo)))

instance Functor m => Isomorphic (InitChainDB m) where
  project :: forall blk. NoHardForks blk
          => InitChainDB m (HardForkBlock '[blk]) -> InitChainDB m blk
  project = InitChainDB.map (inject' (Proxy @(I blk))) (unFlip . project . Flip)

  inject :: forall blk. NoHardForks blk
         => InitChainDB m blk -> InitChainDB m (HardForkBlock '[blk])
  inject = InitChainDB.map (project' (Proxy @(I blk))) (unFlip . inject . Flip)

instance Isomorphic ProtocolClientInfo where
  project ProtocolClientInfo{..} = ProtocolClientInfo {
        pClientInfoCodecConfig = project pClientInfoCodecConfig
      }

  inject ProtocolClientInfo{..} = ProtocolClientInfo {
        pClientInfoCodecConfig = inject pClientInfoCodecConfig
      }

instance Isomorphic ForgeStateUpdateInfo where
  project :: forall blk. NoHardForks blk
          => ForgeStateUpdateInfo (HardForkBlock '[blk]) -> ForgeStateUpdateInfo blk
  project forgeStateUpdateInfo =
      case forgeStateUpdateInfo of
        ForgeStateUpdated forgeStateInfo ->
          ForgeStateUpdated
            (project' (Proxy @(WrapForgeStateInfo blk)) forgeStateInfo)
        ForgeStateUpdateFailed forgeStateUpdateError ->
          ForgeStateUpdateFailed
            (project' (Proxy @(WrapForgeStateUpdateError blk)) forgeStateUpdateError)
        ForgeStateUpdateSuppressed -> ForgeStateUpdateSuppressed

  inject :: forall blk. NoHardForks blk
         => ForgeStateUpdateInfo blk -> ForgeStateUpdateInfo (HardForkBlock '[blk])
  inject forgeStateUpdateInfo =
      case forgeStateUpdateInfo of
        ForgeStateUpdated forgeStateInfo ->
          ForgeStateUpdated
            (inject' (Proxy @(WrapForgeStateInfo blk)) forgeStateInfo)
        ForgeStateUpdateFailed forgeStateUpdateError ->
          ForgeStateUpdateFailed
            (inject' (Proxy @(WrapForgeStateUpdateError blk)) forgeStateUpdateError)
        ForgeStateUpdateSuppressed -> ForgeStateUpdateSuppressed

instance Functor m => Isomorphic (BlockForging m) where
  project :: forall blk. NoHardForks blk
          => BlockForging m (HardForkBlock '[blk]) -> BlockForging m blk
  project BlockForging {..} = BlockForging {
        forgeLabel       = forgeLabel
      , canBeLeader      = project' (Proxy @(WrapCanBeLeader blk)) canBeLeader
      , updateForgeState = \cfg sno tickedChainDepSt ->
                               project <$>
                                 updateForgeState
                                   (inject cfg)
                                   sno
                                   (injTickedChainDepSt
                                     (immutableEpochInfo cfg)
                                     tickedChainDepSt)
      , checkCanForge    = \cfg sno tickedChainDepSt isLeader forgeStateInfo ->
                               first (project' (Proxy @(WrapCannotForge blk))) $
                                 checkCanForge
                                   (inject cfg)
                                   sno
                                   (injTickedChainDepSt
                                     (immutableEpochInfo cfg)
                                     tickedChainDepSt)
                                   (inject' (Proxy @(WrapIsLeader blk)) isLeader)
                                   (inject' (Proxy @(WrapForgeStateInfo blk)) forgeStateInfo)

      , forgeBlock       = \cfg bno sno tickedLgrSt txs isLeader ->
                               project' (Proxy @(I blk)) <$>
                                 forgeBlock
                                   (inject cfg)
                                   bno
                                   sno
                                   (getFlipTickedLedgerState (inject (FlipTickedLedgerState tickedLgrSt)))
                                   (inject' (Proxy @(WrapValidatedGenTx blk)) <$> txs)
                                   (inject' (Proxy @(WrapIsLeader blk)) isLeader)
      }
    where
      injTickedChainDepSt ::
           EpochInfo (Except PastHorizonException)
        -> Ticked (ChainDepState (BlockProtocol blk))
        -> Ticked (ChainDepState (HardForkProtocol '[blk]))
      injTickedChainDepSt ei =
            (`TickedHardForkChainDepState` ei)
          . HardForkState
          . Telescope.TZ
          . State.Current History.initBound
          . Comp
          . WrapTickedChainDepState

  inject :: forall blk. NoHardForks blk
         => BlockForging m blk -> BlockForging m (HardForkBlock '[blk])
  inject BlockForging {..} = BlockForging {
        forgeLabel       = forgeLabel
      , canBeLeader      = inject' (Proxy @(WrapCanBeLeader blk)) canBeLeader
      , updateForgeState = \cfg sno tickedChainDepSt ->
                               inject <$>
                                 updateForgeState
                                   (project cfg)
                                   sno
                                   (projTickedChainDepSt tickedChainDepSt)
      , checkCanForge    = \cfg sno tickedChainDepSt isLeader forgeStateInfo ->
                               first (inject' (Proxy @(WrapCannotForge blk))) $
                                 checkCanForge
                                   (project cfg)
                                   sno
                                   (projTickedChainDepSt tickedChainDepSt)
                                   (project' (Proxy @(WrapIsLeader blk)) isLeader)
                                   (project' (Proxy @(WrapForgeStateInfo blk)) forgeStateInfo)

      , forgeBlock       = \cfg bno sno tickedLgrSt txs isLeader ->
                               inject' (Proxy @(I blk)) <$>
                                 forgeBlock
                                   (project cfg)
                                   bno
                                   sno
                                   (getFlipTickedLedgerState (project (FlipTickedLedgerState tickedLgrSt)))
                                   (project' (Proxy @(WrapValidatedGenTx blk)) <$> txs)
                                   (project' (Proxy @(WrapIsLeader blk)) isLeader)
      }
    where
      projTickedChainDepSt ::
           Ticked (ChainDepState (HardForkProtocol '[blk]))
        -> Ticked (ChainDepState (BlockProtocol blk))
      projTickedChainDepSt =
            unwrapTickedChainDepState
          . unComp
          . State.fromTZ
          . tickedHardForkChainDepStatePerEra

instance Isomorphic ProtocolInfo where
  project :: forall blk. NoHardForks blk
          => ProtocolInfo (HardForkBlock '[blk]) -> ProtocolInfo blk
  project ProtocolInfo {..} = ProtocolInfo {
        pInfoConfig       = project pInfoConfig
      , pInfoInitLedger   = unFlip $ project $ Flip pInfoInitLedger
      }

  inject :: forall blk. NoHardForks blk
         => ProtocolInfo blk -> ProtocolInfo (HardForkBlock '[blk])
  inject ProtocolInfo {..} = ProtocolInfo {
        pInfoConfig       = inject pInfoConfig
      , pInfoInitLedger   = unFlip $ inject $ Flip pInfoInitLedger
      }

{-------------------------------------------------------------------------------
  Types that require take advantage of the fact that we have a single era
-------------------------------------------------------------------------------}

instance Isomorphic WrapApplyTxErr where
  project = aux . unwrapApplyTxErr
    where
      aux :: ApplyTxErr (HardForkBlock '[blk]) -> WrapApplyTxErr blk
      aux (HardForkApplyTxErrFromEra  err) = unZ $ getOneEraApplyTxErr err
      aux (HardForkApplyTxErrWrongEra err) = absurd $ mismatchOneEra err

  inject = WrapApplyTxErr . aux
    where
      aux :: WrapApplyTxErr blk -> ApplyTxErr (HardForkBlock '[blk])
      aux = HardForkApplyTxErrFromEra . OneEraApplyTxErr . Z

instance Isomorphic WrapEnvelopeErr where
  project = aux . unwrapEnvelopeErr
    where
      aux :: OtherHeaderEnvelopeError (HardForkBlock '[blk])
          -> WrapEnvelopeErr blk
      aux (HardForkEnvelopeErrFromEra  err) = unZ $ getOneEraEnvelopeErr err
      aux (HardForkEnvelopeErrWrongEra err) = absurd $ mismatchOneEra err

  inject = WrapEnvelopeErr . aux
    where
      aux :: WrapEnvelopeErr b
          -> OtherHeaderEnvelopeError (HardForkBlock '[b])
      aux = HardForkEnvelopeErrFromEra . OneEraEnvelopeErr . Z

instance Isomorphic WrapCanBeLeader where
  project = OptNP.fromSingleton . getSomeErasCanBeLeader . unwrapCanBeLeader
  inject  = WrapCanBeLeader . SomeErasCanBeLeader . OptNP.singleton

instance Isomorphic WrapForgeStateInfo where
  project (WrapForgeStateInfo forgeStateInfo) =
      case forgeStateInfo of
        CurrentEraForgeStateUpdated info -> unZ $ getOneEraForgeStateInfo info
  inject  =
        WrapForgeStateInfo
      . CurrentEraForgeStateUpdated
      . OneEraForgeStateInfo
      . Z

instance Isomorphic WrapLedgerView where
  project = State.fromTZ . hardForkLedgerViewPerEra . unwrapLedgerView
  inject  = WrapLedgerView
          . HardForkLedgerView TransitionImpossible
          . HardForkState
          . Telescope.TZ
          . Current History.initBound

instance Isomorphic (SomeSecond (NestedCtxt f)) where
  project (SomeSecond ctxt) = SomeSecond $ projNestedCtxt ctxt
  inject  (SomeSecond ctxt) = SomeSecond $ injNestedCtxt  ctxt

instance Isomorphic WrapLedgerErr where
  project = WrapLedgerErr . aux . unwrapLedgerErr
    where
      aux :: HardForkLedgerError '[blk] -> LedgerErr (LedgerState blk)
      aux (HardForkLedgerErrorFromEra err) =
            unwrapLedgerErr
          . unZ
          . getOneEraLedgerError
          $ err
      aux (HardForkLedgerErrorWrongEra err) =
          absurd $ mismatchOneEra err

  inject = WrapLedgerErr . aux . unwrapLedgerErr
    where
      aux :: LedgerErr (LedgerState blk) -> HardForkLedgerError '[blk]
      aux = HardForkLedgerErrorFromEra . OneEraLedgerError . Z . WrapLedgerErr

{-------------------------------------------------------------------------------
  Serialised
-------------------------------------------------------------------------------}

instance Isomorphic SerialisedHeader where
  project =
        SerialisedHeaderFromDepPair
      . depPairFirst projNestedCtxt
      . serialisedHeaderToDepPair

  inject =
        SerialisedHeaderFromDepPair
      . depPairFirst injNestedCtxt
      . serialisedHeaderToDepPair

{-------------------------------------------------------------------------------
  Dependent types

  TODO: Class?
-------------------------------------------------------------------------------}

-- | Project 'BlockQuery'
--
-- Not an instance of 'Isomorphic' because the types change.
projQuery :: BlockQuery (HardForkBlock '[b]) fp result
          -> (forall result'.
                  (result :~: HardForkQueryResult '[b] result')
               -> BlockQuery b fp result'
               -> a)
          -> a
projQuery qry k =
    getHardForkQuery
      qry
      (\Refl -> k Refl . aux)
      (\Refl prfNonEmpty _ _ -> case prfNonEmpty of {})
      (\Refl prfNonEmpty _   -> case prfNonEmpty of {})
  where
    aux :: QueryIfCurrent '[b] fp result -> BlockQuery b fp result
    aux (QZ q) = q
    aux (QS q) = case q of {}

projQuery' :: BlockQuery (HardForkBlock '[b]) fp result
           -> ProjHardForkQuery fp b result
projQuery' qry = projQuery qry $ \Refl -> ProjHardForkQuery

type ProjHardForkQuery :: QueryFootprint -> Type -> Type -> Type
data ProjHardForkQuery fp b res where
  ProjHardForkQuery ::
       BlockQuery b fp result'
    -> ProjHardForkQuery fp b (HardForkQueryResult '[b] result')

-- | Inject 'BlockQuery'
--
-- Not an instance of 'Isomorphic' because the types change.
injQuery :: forall fp b result. BlockQuery b fp result
         -> BlockQuery (HardForkBlock '[b]) fp (HardForkQueryResult '[b] result)
injQuery = QueryIfCurrent . QZ

projQueryResult :: HardForkQueryResult '[b] result -> result
projQueryResult (Left  err)    = absurd $ mismatchOneEra err
projQueryResult (Right result) = result

injQueryResult :: result -> HardForkQueryResult '[b] result
injQueryResult = Right

projNestedCtxt :: NestedCtxt f (HardForkBlock '[blk]) a -> NestedCtxt f blk a
projNestedCtxt = NestedCtxt . aux . flipNestedCtxt
  where
    aux :: NestedCtxt_ (HardForkBlock '[blk]) f a -> NestedCtxt_ blk f a
    aux (NCZ ctxt) = ctxt

injNestedCtxt :: NestedCtxt f blk a -> NestedCtxt f (HardForkBlock '[blk]) a
injNestedCtxt = NestedCtxt . NCZ . flipNestedCtxt
