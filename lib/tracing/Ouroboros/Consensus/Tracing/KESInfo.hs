{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tracing of the KES key state of a block forging credential.
--
-- 'HasKESInfo' and 'GetKESInfo' project the per-block-type
-- 'ForgeStateUpdateError'\/'ForgeStateInfo' onto the block-type-agnostic
-- 'HotKey.KESInfo', which is what actually gets traced. They live next to the
-- 'LogFormatting' instances that consume them so that a consumer of this
-- sublibrary gets both from a single import.
module Ouroboros.Consensus.Tracing.KESInfo
  ( HasKESInfo (..)
  , GetKESInfo (..)
  , traceAsKESInfo
  ) where

import Cardano.Logging
import Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON (..), Value (..), (.=))
import Data.SOP
import qualified Data.Text as Text
import Ouroboros.Consensus.Block.Forging
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( OneEraForgeStateInfo (..)
  , OneEraForgeStateUpdateError (..)
  )
import Ouroboros.Consensus.Node.Tracers (TraceLabelCreds (..))
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
-- Brings the orphan @ForgeStateInfo@/@ForgeStateUpdateError@ type-family
-- instances for @ShelleyBlock@ into scope, so the KES instances below can
-- reduce them to @HotKey.KESInfo@/@HotKey.KESEvolutionError@.
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.TypeFamilyWrappers

--

-- * HasKESInfo

--
class HasKESInfo blk where
  getKESInfo :: Proxy blk -> ForgeStateUpdateError blk -> Maybe HotKey.KESInfo
  getKESInfo _ _ = Nothing

instance HasKESInfo (ShelleyBlock protocol era) where
  getKESInfo _ (HotKey.KESCouldNotEvolve ki _) = Just ki
  getKESInfo _ (HotKey.KESKeyAlreadyPoisoned ki _) = Just ki

instance HasKESInfo ByronBlock

instance All HasKESInfo xs => HasKESInfo (HardForkBlock xs) where
  getKESInfo _ =
    hcollapse
      . hcmap (Proxy @HasKESInfo) getOne
      . getOneEraForgeStateUpdateError
   where
    getOne ::
      forall blk.
      HasKESInfo blk =>
      WrapForgeStateUpdateError blk ->
      K (Maybe HotKey.KESInfo) blk
    getOne = K . getKESInfo (Proxy @blk) . unwrapForgeStateUpdateError

--

-- * GetKESInfo

--
class GetKESInfo blk where
  getKESInfoFromStateInfo :: Proxy blk -> ForgeStateInfo blk -> Maybe HotKey.KESInfo
  getKESInfoFromStateInfo _ _ = Nothing

instance GetKESInfo (ShelleyBlock protocol era) where
  getKESInfoFromStateInfo _ = Just

instance GetKESInfo ByronBlock

instance All GetKESInfo xs => GetKESInfo (HardForkBlock xs) where
  getKESInfoFromStateInfo _ forgeStateInfo =
    case forgeStateInfo of
      CurrentEraLacksBlockForging _ -> Nothing
      CurrentEraForgeStateUpdated currentEraForgeStateInfo ->
        hcollapse
          . hcmap (Proxy @GetKESInfo) getOne
          . getOneEraForgeStateInfo
          $ currentEraForgeStateInfo
   where
    getOne ::
      forall blk.
      GetKESInfo blk =>
      WrapForgeStateInfo blk ->
      K (Maybe HotKey.KESInfo) blk
    getOne = K . getKESInfoFromStateInfo (Proxy @blk) . unwrapForgeStateInfo

--

-- * Tracer

--

traceAsKESInfo ::
  forall m blk.
  (GetKESInfo blk, MonadIO m) =>
  Proxy blk ->
  Trace m (TraceLabelCreds HotKey.KESInfo) ->
  Trace m (TraceLabelCreds (ForgeStateInfo blk))
traceAsKESInfo pr tr = traceAsMaybeKESInfo pr (filterTraceMaybe tr)

traceAsMaybeKESInfo ::
  forall m blk.
  (GetKESInfo blk, MonadIO m) =>
  Proxy blk ->
  Trace m (Maybe (TraceLabelCreds HotKey.KESInfo)) ->
  Trace m (TraceLabelCreds (ForgeStateInfo blk))
traceAsMaybeKESInfo pr (Trace tr) =
  Trace $
    contramap
      ( \case
          (lc, Right (TraceLabelCreds c e)) ->
            case getKESInfoFromStateInfo pr e of
              Just kesi -> (lc, Right (Just (TraceLabelCreds c kesi)))
              Nothing -> (lc, Right Nothing)
          (lc, Left ctrl) -> (lc, Left ctrl)
      )
      tr

-- --------------------------------------------------------------------------------
-- -- KESInfo Tracer
-- --------------------------------------------------------------------------------

deriving newtype instance ToJSON KESPeriod

instance LogFormatting HotKey.KESInfo where
  forMachine _dtal forgeStateInfo =
    let currKesPeriod' = currKesPeriod + startKesPeriod
        maxKesEvos = endKesPeriod - startKesPeriod
        expiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (expiryKesPeriod - currKesPeriod')
     in if kesPeriodsUntilExpiry > 7
          then
            mconcat
              [ "kind" .= String "KESInfo"
              , "startPeriod" .= startKesPeriod
              , "endPeriod" .= currKesPeriod'
              , "evolution" .= endKesPeriod
              ]
          else
            mconcat
              [ "kind" .= String "ExpiryLogMessage"
              , "keyExpiresIn" .= kesPeriodsUntilExpiry
              , "startPeriod" .= startKesPeriod
              , "endPeriod" .= currKesPeriod'
              , "evolution" .= endKesPeriod
              ]
   where
    HotKey.KESInfo
      { HotKey.kesStartPeriod = KESPeriod startKesPeriod
      , HotKey.kesEvolution = currKesPeriod
      , HotKey.kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

  forHuman forgeStateInfo =
    let currKesPeriod' = currKesPeriod + startKesPeriod
        maxKesEvos = endKesPeriod - startKesPeriod
        expiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (expiryKesPeriod - currKesPeriod')
     in if kesPeriodsUntilExpiry > 7
          then
            "KES info startPeriod  "
              <> (Text.pack . show) startKesPeriod
              <> " currPeriod "
              <> (Text.pack . show) currKesPeriod'
              <> " endPeriod "
              <> (Text.pack . show) endKesPeriod
              <> ", "
              <> (Text.pack . show) kesPeriodsUntilExpiry
              <> " KES periods until expiry."
          else
            "Operational key will expire in "
              <> (Text.pack . show) kesPeriodsUntilExpiry
              <> " KES periods."
   where
    HotKey.KESInfo
      { HotKey.kesStartPeriod = KESPeriod startKesPeriod
      , HotKey.kesEvolution = currKesPeriod
      , HotKey.kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

  asMetrics forgeStateInfo =
    let currKesPeriod' = currKesPeriod + startKesPeriod
        maxKesEvos = endKesPeriod - startKesPeriod
        expiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (expiryKesPeriod - currKesPeriod')
     in [ IntM "operationalCertificateStartKESPeriod" (fromIntegral startKesPeriod)
        , IntM "operationalCertificateExpiryKESPeriod" (fromIntegral expiryKesPeriod)
        , IntM "currentKESPeriod" (fromIntegral currKesPeriod')
        , IntM "remainingKESPeriods" (fromIntegral kesPeriodsUntilExpiry)
        ]
   where
    HotKey.KESInfo
      { HotKey.kesStartPeriod = KESPeriod startKesPeriod
      , HotKey.kesEvolution = currKesPeriod
      , HotKey.kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

instance MetaTrace HotKey.KESInfo where
  namespaceFor HotKey.KESInfo{} = Namespace [] ["StateInfo"]

  severityFor (Namespace _ _) (Just forgeStateInfo) =
    Just $
      let currKesPeriod' = currKesPeriod + startKesPeriod
          maxKesEvos = endKesPeriod - startKesPeriod
          expiryKesPeriod = startKesPeriod + maxKesEvos
          kesPeriodsUntilExpiry = max 0 (expiryKesPeriod - currKesPeriod')
       in if kesPeriodsUntilExpiry > 7
            then Info
            else
              if kesPeriodsUntilExpiry <= 1
                then Alert
                else Warning
   where
    HotKey.KESInfo
      { HotKey.kesStartPeriod = KESPeriod startKesPeriod
      , HotKey.kesEvolution = currKesPeriod
      , HotKey.kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo
  severityFor (Namespace _ ["StateInfo"]) _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["StateInfo"]) =
    Just
      "kesStartPeriod \
      \\nkesEndPeriod is kesStartPeriod + tpraosMaxKESEvo\
      \\nkesEvolution is the current evolution or /relative period/."
  documentFor _ = Nothing

  metricsDocFor (Namespace _ ["StateInfo"]) =
    [ ("operationalCertificateStartKESPeriod", "")
    , ("operationalCertificateExpiryKESPeriod", "")
    , ("currentKESPeriod", "")
    , ("remainingKESPeriods", "")
    ]
  metricsDocFor _ = []

  allNamespaces = [Namespace [] ["StateInfo"]]

instance LogFormatting HotKey.KESEvolutionError where
  forMachine dtal (HotKey.KESCouldNotEvolve kesInfo targetPeriod) =
    mconcat
      [ "kind" .= String "KESCouldNotEvolve"
      , "kesInfo" .= forMachine dtal kesInfo
      , "targetPeriod" .= targetPeriod
      ]
  forMachine dtal (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) =
    mconcat
      [ "kind" .= String "KESKeyAlreadyPoisoned"
      , "kesInfo" .= forMachine dtal kesInfo
      , "targetPeriod" .= targetPeriod
      ]
