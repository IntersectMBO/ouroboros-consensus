{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'BlockSupportsPeras' instance and supporting helpers for 'HardForkBlock'.
module Ouroboros.Consensus.HardFork.Combinator.Node.Peras
  ( HardForkPerasErr (..)
  , HardForkPerasConfig (..)
  , forecastToViewAtRound
  , forcastToViewAtSlot
  , ensureSameEraNonEmpty
  , ensureSameEraPair
  , ensurePerasEnabled
  , upcastToHardForkPoint
  , downcastHardForkPoint
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (Pair))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.SOP (All, K (..), SListI, (:.:) (Comp))
import Data.SOP.Functors (Flip (..))
import Data.SOP.Index (hcizipWith, injectNS)
import Data.SOP.Match (matchNS)
import Data.SOP.Strict (HCollapse (..), HSequence (..), NS (..), hcmap, hczipWith, hmap)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..), Point (..), SlotNo)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasCert (..)
  , IsPerasVote (..)
  , IsValidatedPerasCert (..)
  , IsValidatedPerasVote (..)
  , ValidatedPerasVotesReachingQuorum (..)
  )
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (CanHardFork)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
  ( SingleEraBlock
  , proxySingle
  )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( OneEraHash (..)
  , OneEraLedgerView (..)
  , OneEraPerasCert (..)
  , OneEraPerasErr (..)
  , OneEraPerasVote (..)
  , OneEraValidatedPerasCert (..)
  , OneEraValidatedPerasVote (..)
  , PerEraLedgerConfig (..)
  , PerEraPerasConfig (..)
  )
import Ouroboros.Consensus.HardFork.Combinator.Basics
  ( HardForkBlock (..)
  , HardForkLedgerConfig (..)
  , LedgerState (..)
  , completeLedgerConfig''
  )
import Ouroboros.Consensus.HardFork.Combinator.Block ()
import Ouroboros.Consensus.HardFork.Combinator.State qualified as State
import Ouroboros.Consensus.HardFork.History qualified as History
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK, LedgerConfig)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol (..))
import Ouroboros.Consensus.Peras.Cert qualified as Base ()
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
import Ouroboros.Consensus.Peras.Vote (PerasVoteTarget (..))
import Ouroboros.Consensus.Peras.Vote qualified as Base ()
import Ouroboros.Consensus.TypeFamilyWrappers
  ( WrapLedgerConfig (..)
  , WrapLedgerView (..)
  , WrapPerasCert (..)
  , WrapPerasConfig (..)
  , WrapPerasErr (..)
  , WrapPerasVote (..)
  , WrapValidatedPerasCert (..)
  , WrapValidatedPerasVote (..)
  )

{-------------------------------------------------------------------------------
  'BlockSupportsPeras' instance for 'HardForkBlock'
-------------------------------------------------------------------------------}

-- | Errors that can occur in the HFC 'BlockSupportsPeras' instance.
--
-- * 'ParamsEraMismatch': multiple Peras objects (e.g. votes) that should
--   belong to the same era were found to span different eras.
-- * 'PerasRoundBeyondHorizon': the 'PerasRoundNo' could not be translated
--   to a 'SlotNo' because it falls beyond the safe zone of the hard fork
--   history.
-- * 'PerasNotEnabled': the 'PerasRoundNo' falls in an era where Peras is
--   not enabled.
-- * 'SomePerasErr': a single-era 'BlockSupportsPeras' method returned an
--   error, wrapped to identify which era it came from.
data HardForkPerasErr xs
  = ParamsEraMismatch
  | PerasRoundBeyondHorizon
  | PerasNotEnabled
  | SomePerasErr (OneEraPerasErr xs)
  deriving stock (Show, Generic)

deriving instance CanHardFork xs => Eq (HardForkPerasErr xs)
instance (Typeable xs, Show (HardForkPerasErr xs)) => Exception (HardForkPerasErr xs)
instance CanHardFork xs => NoThunks (HardForkPerasErr xs)

data HardForkPerasConfig xs = HardForkPerasConfig
  { hfpcLedgerConfig :: LedgerConfig (HardForkBlock xs)
  , hfpcPerEraPerasConfig :: PerEraPerasConfig xs
  }
  deriving stock (Show, Generic)

instance CanHardFork xs => NoThunks (HardForkPerasConfig xs)

-- | Delegates each method to the single-era implementation after unwrapping
-- the HFC-level types and re-wraps the results.
--
-- The hard fork combinator represents one-era values as n-ary sums ('NS') and
-- per-era configurations as n-ary products ('NP').
--
-- Here are some details on how the dispatching works:
--
-- * When we have a 'NS'-wrapped object, we use 'hcizipWith' to pair the per-era
--   config ('NP') with the value ('NS'), which selects the matching era
--   accordingly.
--
-- * When multiple 'NS' values are supplied, we must first ensure they agree
--   on a same era. For that we use 'ensureSameEraNonEmpty' or 'ensureSameEraPair'
--   to verify they all belong to the same era (turning a container of 'NS'
--   into a 'NS' of container)
--
-- * Each Peras object also has a 'PerasRoundNo', that we can resolve to a slot
--   wrapped in a era-indexed 'NS' thanks to 'Summary'/'Query' machinery _if we
--   also have access to a 'LedgerState'_ (cf. 'forecastToViewAtRound').
--   That is the case in methods for votes:
--   - in 'forgePerasVote', that is the sole way to determine the era, since we
--     receive no 'NS'-wrapped parameter. So we rely on the roundNo era
--     resolution logic to determine the era and dispatch to the correct
--     implementation.
--   - in 'validatePerasVote', we make sure that the round-indicated era agrees
--     with the era of the supplied 'NS'-wrapped vote.
--
-- NOTE: If the interface of 'BlockSupportsPeras' ever changes, here's how to
-- update this instance:
--
-- * If a monomorphic parameter is added to a class method, add it to the HFC
--   method implem, and pass it through to the per-era method when delegating.
--   Nothing else is needed
--
-- * If a parameter whose type varies by era must be added, then:
--   - create an associated type 'MyType' for it in 'BlockSupportsPeras'
--   - define @newtype WrapMyType blk = WrapMyType {unwrapMyType :: MyType blk}@
--     in TypeFamilyWrappers.hs (this turns the type family into a type constructor)
--   - define @newtype OneEraMyType xs = OneEraMyType {getOneEraMyType :: NS WrapMyType xs}@
--     in 'AcrossEras.hs
--   - set @type MyType (HardForkBlock xs) = OneEraMyType xs@ in this HFC
--     'BlockSupportsPeras' instance
--   - use 'ensureSameEraPair' to verify that the supplied parameter agrees on
--     the era with other 'NS'-wrapped parameters
--
--   Note that a similar process can be done for a 'NP' instead of 'NS'. In
--   which case, there is no need for 'ensureSameEraPair', instead one must
--   fiddle with 'hcizipWith' and similar helpers.

instance CanHardFork xs => BlockSupportsPeras (HardForkBlock xs) where
  type LedgerStateOrView (HardForkBlock xs) = LedgerState (HardForkBlock xs) EmptyMK
  type PerasConfig (HardForkBlock xs) = HardForkPerasConfig xs
  type PerasErr (HardForkBlock xs) = HardForkPerasErr xs
  type PerasCert (HardForkBlock xs) = OneEraPerasCert xs
  type ValidatedPerasCert (HardForkBlock xs) = OneEraValidatedPerasCert xs
  type PerasVote (HardForkBlock xs) = OneEraPerasVote xs
  type ValidatedPerasVote (HardForkBlock xs) = OneEraValidatedPerasVote xs

  -- Forge a vote by delegating to the correct era's 'forgePerasVote'.
  --
  -- The era is determined by 'forecastToViewAtRound' that gives a
  -- era-NS-indexed 'LedgerView' corresponding to the specified 'PerasRoundNo'.
  forgePerasVote HardForkPerasConfig{hfpcPerEraPerasConfig, hfpcLedgerConfig} ledgerState voterId roundNo point = do
    -- Ledger state is forecast into ledger view for the target 'PerasRoundNo'.
    (OneEraLedgerView nsLedgerView) <- forecastToViewAtRound hfpcLedgerConfig ledgerState roundNo
    -- Dispatch to the per-era forgePerasVote.
    hcollapse $
      hcizipWith
        proxySingle
        ( \idx wrappedCfg (WrapLedgerView ledgerView) ->
            K $
              bimap
                (SomePerasErr . OneEraPerasErr . injectNS idx . WrapPerasErr)
                (OneEraValidatedPerasVote . injectNS idx . WrapValidatedPerasVote)
                ( forgePerasVote
                    (unwrapPerasConfig wrappedCfg)
                    ledgerView
                    voterId
                    roundNo
                    (downcastHardForkPoint point)
                )
        )
        (getPerEraPerasConfig hfpcPerEraPerasConfig)
        nsLedgerView

  -- Validate a vote by delegating to the correct era's 'validatePerasVote'.
  --
  -- We use 'forecastToViewAtRound' to get a era-NS-indexed 'LedgerView'
  -- corresponding to the specified 'PerasRoundNo'. We make sure that this
  -- era indication matches the era of the supplied vote.
  validatePerasVote HardForkPerasConfig{hfpcPerEraPerasConfig, hfpcLedgerConfig} ledgerState vote@(OneEraPerasVote nsVote) = do
    let roundNo = getPerasVoteRound vote
    -- Ledger state is forecast into ledger view for the target 'PerasRoundNo'.
    (OneEraLedgerView nsLedgerView) <- forecastToViewAtRound hfpcLedgerConfig ledgerState roundNo
    nsVoteAndLedgerView <- ensureSameEraPair (nsVote, nsLedgerView)
    -- Dispatch to the per-era forgePerasVote.
    hcollapse $
      hcizipWith
        proxySingle
        ( \idx wrappedCfg (Pair wrappedVote (WrapLedgerView ledgerView)) ->
            K $
              bimap
                (SomePerasErr . OneEraPerasErr . injectNS idx . WrapPerasErr)
                (OneEraValidatedPerasVote . injectNS idx . WrapValidatedPerasVote)
                ( validatePerasVote
                    (unwrapPerasConfig wrappedCfg)
                    ledgerView
                    (unwrapPerasVote wrappedVote)
                )
        )
        (getPerEraPerasConfig hfpcPerEraPerasConfig)
        nsVoteAndLedgerView

  -- Forge a certificate from a quorum of validated votes.
  --
  -- The votes are 'NS'-wrapped values from potentially different eras. We
  -- use 'ensureSameEraNonEmpty' to verify that all votes belong to the
  -- same era (returning 'ParamsEraMismatch' otherwise) and collect them into a
  -- single 'NS' containing a 'NonEmpty' list.
  -- The we reconstruct a single-era 'ValidatedPerasVotesReachingQuorum' with
  -- the downcasted point, to be able to delegate to the matching
  -- era's 'forgePerasCert'.
  forgePerasCert ::
    ValidatedPerasVotesReachingQuorum (HardForkBlock xs) ->
    Either
      (PerasErr (HardForkBlock xs))
      (ValidatedPerasCert (HardForkBlock xs))
  forgePerasCert
    ValidatedPerasVotesReachingQuorum
      { vpvqVotes
      , vpvqTarget = PerasVoteTarget{pvtRoundNo, pvtBlock}
      , vpvqPerasConfig = HardForkPerasConfig{hfpcPerEraPerasConfig}
      } =
      case ensureSameEraNonEmpty (getOneEraValidatedPerasVote <$> vpvqVotes) of
        Left err -> Left err
        Right sameEraVotes ->
          hcollapse $
            hcizipWith
              proxySingle
              ( \idx wrappedCfg (Compose wrappedVotes) ->
                  K $
                    bimap
                      (SomePerasErr . OneEraPerasErr . injectNS idx . WrapPerasErr)
                      (OneEraValidatedPerasCert . injectNS idx . WrapValidatedPerasCert)
                      ( forgePerasCert
                          ValidatedPerasVotesReachingQuorum
                            { vpvqTarget =
                                PerasVoteTarget
                                  { pvtRoundNo
                                  , pvtBlock = downcastHardForkPoint pvtBlock
                                  }
                            , vpvqVotes = unwrapValidatedPerasVote <$> wrappedVotes
                            , vpvqPerasConfig = unwrapPerasConfig wrappedCfg
                            }
                      )
              )
              (getPerEraPerasConfig hfpcPerEraPerasConfig)
              sameEraVotes

  -- Validate a certificate by delegating to the matching era's
  -- 'validatePerasCert'.
  --
  -- The era to which we delegate is determined directly by the era of the
  -- supplied certificate.
  validatePerasCert ::
    PerasConfig (HardForkBlock xs) ->
    PerasCert (HardForkBlock xs) ->
    Either
      (PerasErr (HardForkBlock xs))
      (ValidatedPerasCert (HardForkBlock xs))
  validatePerasCert HardForkPerasConfig{hfpcPerEraPerasConfig} (OneEraPerasCert cert) =
    hcollapse $
      hcizipWith
        proxySingle
        ( \idx wrappedCfg wrappedCert ->
            K $
              bimap
                (SomePerasErr . OneEraPerasErr . injectNS idx . WrapPerasErr)
                (OneEraValidatedPerasCert . injectNS idx . WrapValidatedPerasCert)
                ( validatePerasCert
                    (unwrapPerasConfig wrappedCfg)
                    (unwrapPerasCert wrappedCert)
                )
        )
        (getPerEraPerasConfig hfpcPerEraPerasConfig)
        cert

{-------------------------------------------------------------------------------
  Accessor class instances for the HFC Peras-related types
-------------------------------------------------------------------------------}

instance CanHardFork xs => IsPerasCert (HardForkBlock xs) (OneEraPerasCert xs) where
  getPerasCertRound (OneEraPerasCert hcert) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . getPerasCertRound
            . unwrapPerasCert
        )
        hcert

  getPerasCertBoostedBlock (OneEraPerasCert hcert) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . upcastToHardForkPoint
            . getPerasCertBoostedBlock
            . unwrapPerasCert
        )
        hcert

instance CanHardFork xs => IsPerasCert (HardForkBlock xs) (OneEraValidatedPerasCert xs) where
  getPerasCertRound (OneEraValidatedPerasCert hcert) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . getPerasCertRound
            . unwrapValidatedPerasCert
        )
        hcert

  getPerasCertBoostedBlock (OneEraValidatedPerasCert hcert) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . upcastToHardForkPoint
            . getPerasCertBoostedBlock
            . unwrapValidatedPerasCert
        )
        hcert

instance CanHardFork xs => IsValidatedPerasCert (HardForkBlock xs) (OneEraValidatedPerasCert xs) where
  getPerasCertBoost (OneEraValidatedPerasCert hcert) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . getPerasCertBoost
            . unwrapValidatedPerasCert
        )
        hcert

instance CanHardFork xs => IsPerasVote (HardForkBlock xs) (OneEraPerasVote xs) where
  getPerasVoteRound (OneEraPerasVote hvote) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . getPerasVoteRound
            . unwrapPerasVote
        )
        hvote

  getPerasVoteVoterId (OneEraPerasVote hvote) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . getPerasVoteVoterId
            . unwrapPerasVote
        )
        hvote

  getPerasVoteBlock (OneEraPerasVote hvote) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . upcastToHardForkPoint
            . getPerasVoteBlock
            . unwrapPerasVote
        )
        hvote

instance CanHardFork xs => IsPerasVote (HardForkBlock xs) (OneEraValidatedPerasVote xs) where
  getPerasVoteRound (OneEraValidatedPerasVote hvote) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . getPerasVoteRound
            . unwrapValidatedPerasVote
        )
        hvote

  getPerasVoteVoterId (OneEraValidatedPerasVote hvote) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . getPerasVoteVoterId
            . unwrapValidatedPerasVote
        )
        hvote

  getPerasVoteBlock (OneEraValidatedPerasVote hvote) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . upcastToHardForkPoint
            . getPerasVoteBlock
            . unwrapValidatedPerasVote
        )
        hvote

instance CanHardFork xs => IsValidatedPerasVote (HardForkBlock xs) (OneEraValidatedPerasVote xs) where
  getPerasVoteStake (OneEraValidatedPerasVote hvote) =
    hcollapse $
      hcmap
        proxySingle
        ( K
            . getPerasVoteStake
            . unwrapValidatedPerasVote
        )
        hvote

{-------------------------------------------------------------------------------
  HFC/SOP-specific helpers
-------------------------------------------------------------------------------}

-- | Forecast the tip of LedgerState into a 'LedgerView' for the given
-- 'PerasRoundNo'.
--
-- Given a 'HardForkLedgerConfig' and the HFC ledger state telescope, this
-- function resolves the 'PerasRoundNo' to a 'SlotNo' via 'History.runQueryNS'
-- applied to 'History.perasRoundNoToSlot', then forecasts the ledger state to
-- that slot using 'forecastToViewAtSlot'.
--
-- Returns @Left err@ if the slot is outside the forecast range (e.g. beyond
-- the safe zone), or if Peras isn't enabled for the corresponding era.
-- Otherwise, returns @Right (OneEraLedgerView xs)@ positioned at the
-- corresponding era.
forecastToViewAtRound ::
  All SingleEraBlock xs =>
  HardForkLedgerConfig xs ->
  LedgerState (HardForkBlock xs) EmptyMK ->
  PerasRoundNo ->
  Either (HardForkPerasErr xs) (OneEraLedgerView xs)
forecastToViewAtRound ledgerConfig ledgerState@(HardForkLedgerState unwrappedLedgerState) roundNo = do
  -- Build a summary to be able to run queries
  let summary = State.reconstructSummaryLedger ledgerConfig unwrappedLedgerState
  -- Query the hard fork history to determine what is the start slot of this
  -- PerasRound. Thanks to 'runQueryNS', the result is wrapped in an 'NS' which
  -- \*also* gives us the era the round falls in by its position.
  nsQueryRes <-
    bimap (const PerasRoundBeyondHorizon) id $
      History.runQueryNS (History.perasRoundNoToSlot roundNo) summary
  -- Extract the slot corresponding to the Peras round from the query result.
  (slot, _roundLength) <- ensurePerasEnabled (hcollapse nsQueryRes)
  -- Ledger state is forecast into ledger view for the target slot.
  nsLedgerView <- forcastToViewAtSlot ledgerConfig slot ledgerState
  -- Ensure the query era and the forecasted view era agree.
  nsQueryResAndLedgerView <- ensureSameEraPair (nsQueryRes, getOneEraLedgerView nsLedgerView)
  pure $ OneEraLedgerView $ hmap (\(Pair _ y) -> y) nsQueryResAndLedgerView

-- | Forecast the tip of LedgerState into a 'LedgerView' for the given slot.
--
-- Given a 'HardForkLedgerConfig' and the HFC ledger state telescope, this
-- function completes the per-era partial ledger configs, extracts the tip
-- era's ledger state, calls 'ledgerViewForecastAt' to build a 'Forecast',
-- then 'forecastFor' to obtain the 'LedgerView' at the target slot.
--
-- Returns @Left err@ if the slot is outside the forecast range (e.g. beyond
-- the safe zone), or @Right (OneEraLedgerView xs)@ positioned at the tip era.
forcastToViewAtSlot ::
  All SingleEraBlock xs =>
  HardForkLedgerConfig xs ->
  SlotNo ->
  LedgerState (HardForkBlock xs) EmptyMK ->
  Either (HardForkPerasErr xs) (OneEraLedgerView xs)
forcastToViewAtSlot cfg slot (HardForkLedgerState st) =
  OneEraLedgerView
    <$> ( hsequence' $ -- Collapse the 'NS' of 'Either' into an 'Either' of 'NS'.
            hczipWith
              proxySingle
              ( \(WrapLedgerConfig lcfg) (Flip ledgerSt) ->
                  Comp $ case runExcept (forecastFor (ledgerViewForecastAt lcfg ledgerSt) slot) of
                    Left _err -> Left PerasRoundBeyondHorizon
                    Right view -> Right (WrapLedgerView view)
              )
              perEraFullLedgerConfig
              (State.tip st)
        )
 where
  epochInfo = State.epochInfoLedger cfg st
  perEraFullLedgerConfig =
    hcmap
      proxySingle
      (completeLedgerConfig'' epochInfo)
      (getPerEraLedgerConfig (hardForkLedgerConfigPerEra cfg))

-- | Ensure that all elements of a non-empty list of 'NS' values are in the
-- same era, collecting them into a single 'NS' containing a 'NonEmpty'. Returns
-- 'Left ParamsEraMismatch' if elements are from different eras, or
-- 'Right ns' if all are from the same era.
ensureSameEraNonEmpty ::
  SListI xs =>
  NonEmpty (NS f xs) ->
  Either (HardForkPerasErr xs) (NS (Compose NonEmpty f) xs)
ensureSameEraNonEmpty (x :| rest) = foldl go (Right $ hmap (Compose . (:| [])) x) rest
 where
  go (Left err) _ = Left err
  go (Right acc) ns =
    case matchNS acc ns of
      Left _mismatch -> Left ParamsEraMismatch
      Right nsQueryResAndLedgerView ->
        Right $ hmap (\(Pair (Compose fs) f) -> Compose (fs <> (f :| []))) nsQueryResAndLedgerView

-- | Ensure that two 'NS' values are in the same era, pairing them together.
-- Returns 'Left ParamsEraMismatch' if they are from different eras.
ensureSameEraPair ::
  (NS f xs, NS g xs) ->
  Either (HardForkPerasErr xs) (NS (Product f g) xs)
ensureSameEraPair (l, r) =
  case matchNS l r of
    Left _mismatch -> Left ParamsEraMismatch
    Right nsQueryResAndLedgerView -> Right nsQueryResAndLedgerView

-- | Transforms a 'History.PerasEnabled' result into an 'Either', returning
-- 'Left PerasNotEnabled' if Peras is not enabled in the era.
ensurePerasEnabled ::
  History.PerasEnabled a ->
  Either (HardForkPerasErr xs) a
ensurePerasEnabled = \case
  History.PerasEnabled a -> Right a
  History.NoPerasEnabled -> Left PerasNotEnabled

-- | Downcast a 'Point' of the hard fork block to a 'Point' of a single era
-- by decoding the raw hash via 'fromShortRawHash'. Used when delegating
-- operations that take a 'Point' argument to a single-era implementation.
downcastHardForkPoint ::
  forall blk xs.
  SingleEraBlock blk =>
  Point (HardForkBlock xs) ->
  Point blk
downcastHardForkPoint = \case
  GenesisPoint ->
    GenesisPoint
  BlockPoint s (OneEraHash h) ->
    BlockPoint s (fromShortRawHash (Proxy @blk) h)

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
