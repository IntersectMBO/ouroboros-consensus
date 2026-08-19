{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tools.DBAnalyser.Block.Cardano
  ( Args (configFile, threshold, CardanoBlockArgs)
  , CardanoBlockArgs

    -- * Interpreting the node configuration

    -- | The piece of the node configuration that db-analyser derives itself,
    -- rather than getting it ready-made from @cardano-config@ or from
    -- "Cardano.Tools.Config". Exposed so that it can be tested against a
    -- configuration file directly.
  , mkLedgerDBBackend
  ) where

import qualified Cardano.Chain.Block as Byron.Block
import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Chain.UTxO as Byron.UTxO
import qualified Cardano.Chain.Update as Byron.Update
import qualified Cardano.Configuration as Cfg
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Transition as SL
import Cardano.Ledger.Core (TxOut)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.UTxO as Shelley.UTxO
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Protocol.Crypto
import Cardano.Tools.Config
  ( mkHardForkTriggers
  , mkInitialNonce
  , mkTransitionConfig
  , reportConfigWarnings
  , resolveNodeConfiguration
  , throwConfigError
  )
import Cardano.Tools.DBAnalyser.Block.Byron ()
import Cardano.Tools.DBAnalyser.Block.Shelley ()
import Cardano.Tools.DBAnalyser.HasAnalysis
import Cardano.Tools.DBAnalyser.Types
  ( LSMOptions (..)
  , LedgerDBBackend (..)
  , defaultLSMDatabasePath
  )
import qualified Data.Compact as Compact
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict
  ( StrictMaybe (..)
  , fromSMaybe
  , strictMaybeToMaybe
  )
import Data.SOP.BasicFunctors
import Data.SOP.Functors
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron.Ledger
import Ouroboros.Consensus.Cardano
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.Cardano.Node
  ( CardanoProtocolParams (..)
  , protocolInfoCardano
  )
import Ouroboros.Consensus.Config (emptyCheckpointsMap)
import Ouroboros.Consensus.HardFork.Combinator
  ( HardForkBlock (..)
  , OneEraBlock (..)
  , OneEraHash (..)
  , getHardForkState
  , hardForkLedgerStatePerEra
  )
import Ouroboros.Consensus.HardFork.Combinator.State (currentState)
import Ouroboros.Consensus.Ledger.Abstract hiding (TxIn, TxOut)
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Shelley.HFEras ()
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( ShelleyBlock
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import System.Directory (makeAbsolute)
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (MountPoint (MountPoint))
import System.FS.IO (ioHasFS)
import System.FilePath (isAbsolute, takeDirectory)
import TextBuilder (TextBuilder)
import qualified TextBuilder as Builder

analyseBlock ::
  (forall blk. HasAnalysis blk => blk -> a) ->
  CardanoBlock StandardCrypto ->
  a
analyseBlock f =
  hcollapse
    . hcmap p (K . f . unI)
    . getOneEraBlock
    . getHardForkBlock
 where
  p :: Proxy HasAnalysis
  p = Proxy

-- | Lift a function polymorphic over all block types supporting `HasAnalysis`
-- into a corresponding function over `CardanoBlock.`
analyseWithLedgerState ::
  forall a.
  (forall blk. HasAnalysis blk => WithLedgerState blk -> a) ->
  WithLedgerState (CardanoBlock StandardCrypto) ->
  a
analyseWithLedgerState f (WithLedgerState cb sb sa) =
  hcollapse
    . hcmap p (K . f)
    . fromJust
    . hsequence'
    $ hzipWith3 zipLS (goLS sb) (goLS sa) oeb
 where
  p :: Proxy HasAnalysis
  p = Proxy

  zipLS (Comp (Just (Flip sb'))) (Comp (Just (Flip sa'))) (I blk) =
    Comp . Just $ WithLedgerState blk sb' sa'
  zipLS _ _ _ = Comp Nothing

  oeb = getOneEraBlock . getHardForkBlock $ cb

  goLS ::
    LedgerState (CardanoBlock StandardCrypto) mk ->
    NP (Maybe :.: Flip LedgerState mk) (CardanoEras StandardCrypto)
  goLS =
    hexpand (Comp Nothing)
      . hmap (Comp . Just . currentState)
      . Telescope.tip
      . getHardForkState
      . hardForkLedgerStatePerEra

instance HasProtocolInfo (CardanoBlock StandardCrypto) where
  data Args (CardanoBlock StandardCrypto) = CardanoBlockArgs
    { configFile :: FilePath
    , threshold :: Maybe PBftSignatureThreshold
    }

  mkProtocolInfoAndBackend CardanoBlockArgs{configFile, threshold} = do
    absoluteConfig <- makeAbsolute configFile
    let configDir = takeDirectory absoluteConfig

    -- The node configuration is parsed and resolved by the shared
    -- 'cardano-config' package, which also loads every era's genesis file
    -- (resolving genesis paths relative to the configuration file's directory)
    -- and hands them back already decoded.
    (nc, warns) <- resolveNodeConfiguration configFile
    reportConfigWarnings warns
    triggers <- either throwConfigError pure $ mkHardForkTriggers (Cfg.testingConfiguration nc)
    backend <- either throwConfigError pure $ mkLedgerDBBackend (Cfg.storageConfiguration nc)

    let fs = SomeHasFS (ioHasFS (MountPoint configDir))

    pInfo <-
      mkCardanoProtocolInfo
        fs
        (Cfg.byronGenesisConfig nc)
        threshold
        (mkTransitionConfig nc)
        (mkInitialNonce nc)
        triggers
    pure (pInfo, backend)

-- | The LedgerDB backend the node configuration selects, if it selects one.
mkLedgerDBBackend ::
  Cfg.StorageConfiguration Identity -> Either String (Maybe LedgerDBBackend)
mkLedgerDBBackend storeCfg =
  case Cfg.backendSelector (runIdentity (Cfg.ledgerDbConfiguration storeCfg)) of
    SNothing -> Right Nothing
    SJust Cfg.V2InMemory -> Right (Just V2InMem)
    SJust (Cfg.V2LSM dbPath exportPath) -> do
      lsmDatabasePath <-
        checkRelative "DatabasePath" (fromSMaybe defaultLSMDatabasePath dbPath)
      lsmExportPath <-
        traverse (checkRelative "ExportPath") (strictMaybeToMaybe exportPath)
      Right $
        Just $
          V2LSM
            LSMOptions
              { lsmDatabasePath
              , lsmExportPath
              , -- Not configurable via the node configuration file; the OS page
                -- cache is only bypassed on explicit request (@--lsm-no-cache@).
                lsmNoDiskCache = False
              }
 where
  -- The LSM paths name directories inside the LedgerDB filesystem, whose root is
  -- the ChainDB directory, not the directory of the configuration file; hence
  -- they are not adjusted the way the genesis paths are. An absolute path
  -- therefore cannot be honoured as written -- it would be reinterpreted as
  -- relative to the ChainDB -- so reject it rather than silently mount it
  -- somewhere the user did not ask for.
  checkRelative :: String -> FilePath -> Either String FilePath
  checkRelative field path
    | isAbsolute path =
        Left $
          "the node configuration sets LedgerDB.Backend.LSM."
            <> field
            <> " to the absolute path "
            <> show path
            <> ", but it must be relative to the ChainDB directory."
    | otherwise = Right path

instance HasAnalysis (CardanoBlock StandardCrypto) where
  countTxOutputs = analyseBlock countTxOutputs
  blockTxSizes = analyseBlock blockTxSizes
  knownEBBs _ =
    Map.mapKeys castHeaderHash . Map.map castChainHash $
      knownEBBs (Proxy @ByronBlock)

  emitTraces = analyseWithLedgerState emitTraces

  blockStats = analyseBlock blockStats

  blockApplicationMetrics =
    [
      ( "Slot Number"
      , \(WithLedgerState blk _preSt _postSt) ->
          pure $ Builder.decimal $ unSlotNo $ blockSlot blk
      )
    ,
      ( "Block Number"
      , \(WithLedgerState blk _preSt _postSt) ->
          pure $ Builder.decimal $ unBlockNo $ blockNo blk
      )
    , -- TODO the states will only contain the outputs produced by the block,
      -- not the whole UTxO set, so there is a regression here.

      ( "UTxO size (via Compact)"
      , \(WithLedgerState _blk _preSt postSt) -> do
          let compactSize utxo = do
                compactedUtxo <- Compact.compact utxo
                compactedUtxoSize <- Compact.compactSize compactedUtxo
                pure $ Builder.decimal $ compactedUtxoSize

          dispatch
            postSt
            (applyToByronUtxo compactSize)
            (applyToShelleyBasedUtxo compactSize)
      )
    ,
      ( "UTxO map size"
      , \(WithLedgerState _blk _preSt postSt) -> do
          let mapSize = pure . Builder.decimal . Map.size
          dispatch
            postSt
            (applyToByronUtxo mapSize)
            (applyToShelleyBasedUtxo mapSize)
      )
    ]

dispatch ::
  LedgerState (CardanoBlock StandardCrypto) ValuesMK ->
  (LedgerState ByronBlock ValuesMK -> IO TextBuilder) ->
  (forall proto era. LedgerState (ShelleyBlock proto era) ValuesMK -> IO TextBuilder) ->
  IO TextBuilder
dispatch cardanoSt fByron fShelley =
  hcollapse $
    hap
      ( fn k_fByron
          :* fn k_fShelley
          :* fn k_fShelley
          :* fn k_fShelley
          :* fn k_fShelley
          :* fn k_fShelley
          :* fn k_fShelley
          :* fn k_fShelley
          :* Nil
      )
      (hardForkLedgerStatePerEra cardanoSt)
 where
  k_fByron = K . fByron . unFlip

  k_fShelley ::
    forall proto era.
    Flip LedgerState ValuesMK (ShelleyBlock proto era) ->
    K (IO TextBuilder) (ShelleyBlock proto era)
  k_fShelley = K . fShelley . unFlip

applyToByronUtxo ::
  (Map Byron.UTxO.CompactTxIn Byron.UTxO.CompactTxOut -> IO TextBuilder) ->
  LedgerState ByronBlock ValuesMK ->
  IO TextBuilder
applyToByronUtxo f st =
  f $ getByronUtxo st

getByronUtxo ::
  LedgerState ByronBlock ValuesMK ->
  Map Byron.UTxO.CompactTxIn Byron.UTxO.CompactTxOut
getByronUtxo =
  Byron.UTxO.unUTxO
    . Byron.Block.cvsUtxo
    . Byron.Ledger.byronLedgerState

applyToShelleyBasedUtxo ::
  (Map TxIn (TxOut era) -> IO TextBuilder) ->
  LedgerState (ShelleyBlock proto era) ValuesMK ->
  IO TextBuilder
applyToShelleyBasedUtxo f st = do
  f $ getShelleyBasedUtxo st

getShelleyBasedUtxo ::
  LedgerState (ShelleyBlock proto era) ValuesMK ->
  Map TxIn (TxOut era)
getShelleyBasedUtxo =
  (\(Shelley.UTxO.UTxO xs) -> xs)
    . Shelley.LedgerState.utxosUtxo
    . Shelley.LedgerState.lsUTxOState
    . Shelley.LedgerState.esLState
    . Shelley.LedgerState.nesEs
    . Shelley.Ledger.shelleyLedgerState

type CardanoBlockArgs = Args (CardanoBlock StandardCrypto)

mkCardanoProtocolInfo ::
  SomeHasFS IO ->
  Byron.Genesis.Config ->
  Maybe PBftSignatureThreshold ->
  SL.TransitionConfig L.LatestKnownEra ->
  Nonce ->
  CardanoHardForkTriggers ->
  IO (ProtocolInfo (CardanoBlock StandardCrypto))
mkCardanoProtocolInfo fs genesisByron signatureThreshold transitionConfig initialNonce triggers =
  fst
    <$> protocolInfoCardano @_ @IO
      fs
      ( CardanoProtocolParams
          ProtocolParamsByron
            { byronGenesis = genesisByron
            , byronPbftSignatureThreshold = signatureThreshold
            , byronProtocolVersion = Byron.Update.ProtocolVersion 1 2 0
            , byronSoftwareVersion =
                Byron.Update.SoftwareVersion (Byron.Update.ApplicationName "db-analyser") 2
            , byronLeaderCredentials = Nothing
            }
          ProtocolParamsShelleyBased
            { shelleyBasedInitialNonce = initialNonce
            , shelleyBasedLeaderCredentials = []
            }
          triggers
          transitionConfig
          emptyCheckpointsMap
          (ProtVer (L.eraProtVerHigh @L.LatestKnownEra) 0)
      )

castHeaderHash ::
  HeaderHash ByronBlock ->
  HeaderHash (CardanoBlock StandardCrypto)
castHeaderHash = OneEraHash . toShortRawHash (Proxy @ByronBlock)

castChainHash ::
  ChainHash ByronBlock ->
  ChainHash (CardanoBlock StandardCrypto)
castChainHash GenesisHash = GenesisHash
castChainHash (BlockHash h) = BlockHash $ castHeaderHash h
