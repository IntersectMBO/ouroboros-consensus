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
  ) where

import qualified Cardano.Chain.Block as Byron.Block
import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Chain.UTxO as Byron.UTxO
import qualified Cardano.Chain.Update as Byron.Update
import qualified Cardano.Configuration as Cfg
import qualified Cardano.Crypto.Hash.Class as CryptoClass
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Transition as SL
import Cardano.Ledger.BaseTypes (boundRational, unsafeNonZero)
import Cardano.Ledger.Core (TxOut)
import Cardano.Ledger.Dijkstra.PParams
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.UTxO as Shelley.UTxO
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Protocol.Crypto
import Cardano.Tools.DBAnalyser.Block.Byron ()
import Cardano.Tools.DBAnalyser.Block.Shelley ()
import Cardano.Tools.DBAnalyser.HasAnalysis
import qualified Data.Compact as Compact
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Maybe.Strict (StrictMaybe (..), strictMaybe, strictMaybeToMaybe)
import Data.SOP.BasicFunctors
import Data.SOP.Functors
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Data.Word (Word64)
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
import System.FilePath (takeDirectory)
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

  mkProtocolInfo CardanoBlockArgs{configFile, threshold} = do
    absoluteConfig <- makeAbsolute configFile
    let configDir = takeDirectory absoluteConfig

    -- The node configuration is parsed and resolved by the shared
    -- 'cardano-config' package, which also loads every era's genesis file
    -- (resolving genesis paths relative to the configuration file's directory)
    -- and hands them back already decoded.
    nc <- resolveNodeConfiguration configFile
    let protoCfg = Cfg.protocolConfiguration nc

        genesisByron = Cfg.byronGenesisConfig nc
        genesisShelley = Cfg.shelleyGenesisConfig nc
        genesisAlonzo = Cfg.alonzoGenesisConfig nc
        genesisConway = Cfg.conwayGenesisConfig nc
        genesisDijkstra =
          strictMaybe emptyDijkstraGenesis id (Cfg.experimentalGenesisConfig nc)

        transCfg =
          SL.mkLatestTransitionConfig genesisShelley genesisAlonzo genesisConway genesisDijkstra

        -- The initial nonce is the Blake2b-256 hash of the Shelley genesis
        -- file, which 'cardano-config' records alongside the file path.
        initialNonce = Nonce $ CryptoClass.castHash $ Cfg.hash (Cfg.shelleyGenesis protoCfg)

        fs = SomeHasFS (ioHasFS (MountPoint configDir))

    mkCardanoProtocolInfo
      fs
      genesisByron
      threshold
      transCfg
      initialNonce
      (mkHardForkTriggers (Cfg.testingConfiguration nc))

  mkLSMConfig CardanoBlockArgs{configFile} = do
    -- The export path is interpreted relative to the LedgerDB filesystem root,
    -- not the config file, so we do not adjust file paths here.
    nc <- resolveNodeConfiguration configFile
    let storeCfg = Cfg.storageConfiguration nc
        ldbCfg = runIdentity (Cfg.ledgerDbConfiguration storeCfg)
        exportPath = case Cfg.backendSelector ldbCfg of
          SJust (Cfg.V2LSM _ e) -> strictMaybeToMaybe e
          _ -> Nothing
    pure
      LSMConfig
        { lsmConfigExportPath = exportPath
        }

-- | Parse and resolve the node configuration with the shared 'cardano-config'
-- package. db-analyser drives the configuration from the config file alone, with
-- no command-line overrides, so 'Cfg.resolveConfigurationFromFile' resolves it
-- against cardano-config's all-defaults CLI arguments and the file layer wins.
resolveNodeConfiguration :: FilePath -> IO Cfg.NodeConfiguration
resolveNodeConfiguration configFile =
  Cfg.resolveConfigurationFromFile configFile >>= \case
    Left err -> error ("db-analyser: invalid node configuration: " <> show err)
    Right (nc, _warns) -> pure nc

-- | An empty Dijkstra genesis to be provided when none is specified in the config.
emptyDijkstraGenesis :: SL.DijkstraGenesis
emptyDijkstraGenesis =
  let upgradePParamsDef =
        UpgradeDijkstraPParams
          { udppMaxRefScriptSizePerBlock = 1048576
          , udppMaxRefScriptSizePerTx = 204800
          , udppRefScriptCostStride = unsafeNonZero 25600
          , udppRefScriptCostMultiplier = fromMaybe (error "impossible") $ boundRational 1.2
          }
   in SL.DijkstraGenesis{SL.dgUpgradePParams = upgradePParamsDef}

-- | Build the 'CardanoHardForkTriggers' from the @Testing@ section of the
-- configuration: each era hard-forks at its configured epoch, or at the
-- default protocol version when no epoch is given.
--
-- If an era is configured to hard-fork at a specific epoch, then so must all
-- earlier eras; otherwise the configuration is rejected.
mkHardForkTriggers :: Cfg.TestingConfiguration Identity -> CardanoHardForkTriggers
mkHardForkTriggers testCfg
  | any (\(earlier, later) -> isNothing earlier && isJust later) (zip epochs (drop 1 epochs)) =
      error
        "if the Cardano config file sets a Test*HardForkAtEpoch, it must also set it for all previous eras."
  | otherwise =
      CardanoHardForkTriggers'
        { triggerHardForkShelley = toTrigger (epochOf Cfg.testShelleyHardForkAtEpoch)
        , triggerHardForkAllegra = toTrigger (epochOf Cfg.testAllegraHardForkAtEpoch)
        , triggerHardForkMary = toTrigger (epochOf Cfg.testMaryHardForkAtEpoch)
        , triggerHardForkAlonzo = toTrigger (epochOf Cfg.testAlonzoHardForkAtEpoch)
        , triggerHardForkBabbage = toTrigger (epochOf Cfg.testBabbageHardForkAtEpoch)
        , triggerHardForkConway = toTrigger (epochOf Cfg.testConwayHardForkAtEpoch)
        , triggerHardForkDijkstra = toTrigger (epochOf Cfg.testDijkstraHardForkAtEpoch)
        }
 where
  -- cardano-config records the configured epochs as 'StrictMaybe'; db-analyser
  -- works with plain 'Maybe' here.
  epochOf ::
    (Cfg.TestingConfiguration Identity -> StrictMaybe Word64) -> Maybe Word64
  epochOf f = strictMaybeToMaybe (f testCfg)

  -- In Shelley-era order; mirrors the field order of 'CardanoHardForkTriggers''.
  epochs =
    [ epochOf Cfg.testShelleyHardForkAtEpoch
    , epochOf Cfg.testAllegraHardForkAtEpoch
    , epochOf Cfg.testMaryHardForkAtEpoch
    , epochOf Cfg.testAlonzoHardForkAtEpoch
    , epochOf Cfg.testBabbageHardForkAtEpoch
    , epochOf Cfg.testConwayHardForkAtEpoch
    , epochOf Cfg.testDijkstraHardForkAtEpoch
    ]

  toTrigger :: Maybe Word64 -> CardanoHardForkTrigger blk
  toTrigger =
    maybe
      CardanoTriggerHardForkAtDefaultVersion
      (CardanoTriggerHardForkAtEpoch . EpochNo)

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
