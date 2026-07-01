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
import Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Hash.Class as CryptoClass
import Cardano.Crypto.Raw (Raw)
import qualified Cardano.Configuration.File as Cfg
import qualified Cardano.Configuration.File.Protocol as Cfg
import qualified Cardano.Configuration.File.Storage as Cfg
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Transition as SL
import Cardano.Ledger.BaseTypes (boundRational, unsafeNonZero)
import Cardano.Ledger.Core (TxOut)
import Cardano.Ledger.Dijkstra.PParams
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.UTxO as Shelley.UTxO
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Protocol.Crypto
import qualified Cardano.Tools.DBAnalyser.Block.Byron as BlockByron
import Cardano.Tools.DBAnalyser.Block.Shelley ()
import Cardano.Tools.DBAnalyser.HasAnalysis
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Compact as Compact
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
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
import System.FilePath (takeDirectory, (</>))
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
        relativeToConfig :: FilePath -> FilePath
        relativeToConfig = (configDir </>)

    -- The node configuration is parsed by the shared 'cardano-config' package
    -- rather than by a parser local to db-analyser. Genesis file paths in the
    -- configuration are relative to the configuration file's directory.
    ncff <- Cfg.parseConfigurationFiles configFile
    let protoCfg = runIdentity (Cfg.protocolConfiguration ncff)
        testCfg = runIdentity (Cfg.testingConfiguration ncff)
        byronCfg = Cfg.byronGenesis protoCfg

        byronGenesisPath = relativeToConfig $ Cfg.hashed (Cfg.byronGenesisFile byronCfg)
        shelleyGenesisPath = relativeToConfig $ Cfg.hashed (Cfg.shelleyGenesis protoCfg)
        alonzoGenesisPath = relativeToConfig $ Cfg.hashed (Cfg.alonzoGenesis protoCfg)
        conwayGenesisPath = relativeToConfig $ Cfg.hashed (Cfg.conwayGenesis protoCfg)
        dijkstraGenesisPath = relativeToConfig . Cfg.hashed <$> Cfg.experimentalGenesis testCfg

        -- 'cardano-config' merges in its bundled defaults (which set
        -- @RequiresNetworkMagic@ to @RequiresNoMagic@) before handing us the
        -- configuration, so this field is normally populated; we treat an
        -- absent value as that same default.
        requiresNetworkMagic = case Cfg.byronReqNetworkMagic byronCfg of
          Just Cfg.RequiresNoMagic -> RequiresNoMagic
          Just Cfg.RequiresMagic -> RequiresMagic
          Nothing -> RequiresNoMagic

        -- 'cardano-config' stores hashes as cardano-crypto-class
        -- 'Hash Blake2b_256', whereas Byron expects a 'Crypto.Hash Raw'. Both
        -- are the same 32 Blake2b-256 digest bytes, so we transfer them via the
        -- raw bytes.
        byronGenesisHash :: Maybe (Crypto.Hash Raw)
        byronGenesisHash =
          (Crypto.abstractHashFromBytes . CryptoClass.hashToBytes)
            =<< Cfg.hash (Cfg.byronGenesisFile byronCfg)

    genesisByron <-
      BlockByron.openGenesisByron byronGenesisPath byronGenesisHash requiresNetworkMagic
    genesisShelley <-
      either (error . show) return
        =<< Aeson.eitherDecodeFileStrict' shelleyGenesisPath
    genesisAlonzo <-
      either (error . show) return
        =<< Aeson.eitherDecodeFileStrict' alonzoGenesisPath
    genesisConway <-
      either (error . show) return
        =<< Aeson.eitherDecodeFileStrict' conwayGenesisPath
    genesisDijkstra <- case dijkstraGenesisPath of
      Nothing -> pure emptyDijkstraGenesis
      Just fp ->
        either (error . show) return
          =<< Aeson.eitherDecodeFileStrict' fp

    let transCfg =
          SL.mkLatestTransitionConfig genesisShelley genesisAlonzo genesisConway genesisDijkstra

    initialNonce <- case Nonce . CryptoClass.castHash <$> Cfg.hash (Cfg.shelleyGenesis protoCfg) of
      Just h -> pure h
      Nothing -> do
        content <- BS.readFile shelleyGenesisPath
        pure $
          Nonce $
            CryptoClass.castHash $
              CryptoClass.hashWith id $
                content

    let fs = SomeHasFS (ioHasFS (MountPoint configDir))

    mkCardanoProtocolInfo
      fs
      genesisByron
      threshold
      transCfg
      initialNonce
      (mkHardForkTriggers testCfg)

  mkLSMConfig CardanoBlockArgs{configFile} = do
    -- The export path is interpreted relative to the LedgerDB filesystem root,
    -- not the config file, so we do not adjust file paths here.
    ncff <- Cfg.parseConfigurationFiles configFile
    let storeCfg = runIdentity (Cfg.storageConfiguration ncff)
        exportPath = case Cfg.ledgerDbConfiguration storeCfg of
          Just ldbCfg
            | Just (Cfg.V2LSM _ e) <- Cfg.backendSelector ldbCfg -> e
          _ -> Nothing
    pure
      LSMConfig
        { lsmConfigExportPath = exportPath
        }

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
mkHardForkTriggers :: Cfg.TestingConfiguration Maybe -> CardanoHardForkTriggers
mkHardForkTriggers testCfg
  | any (\(earlier, later) -> isNothing earlier && isJust later) (zip epochs (drop 1 epochs)) =
      error
        "if the Cardano config file sets a Test*HardForkAtEpoch, it must also set it for all previous eras."
  | otherwise =
      CardanoHardForkTriggers'
        { triggerHardForkShelley = toTrigger (Cfg.testShelleyHardForkAtEpoch testCfg)
        , triggerHardForkAllegra = toTrigger (Cfg.testAllegraHardForkAtEpoch testCfg)
        , triggerHardForkMary = toTrigger (Cfg.testMaryHardForkAtEpoch testCfg)
        , triggerHardForkAlonzo = toTrigger (Cfg.testAlonzoHardForkAtEpoch testCfg)
        , triggerHardForkBabbage = toTrigger (Cfg.testBabbageHardForkAtEpoch testCfg)
        , triggerHardForkConway = toTrigger (Cfg.testConwayHardForkAtEpoch testCfg)
        , triggerHardForkDijkstra = toTrigger (Cfg.testDijkstraHardForkAtEpoch testCfg)
        }
 where
  -- In Shelley-era order; mirrors the field order of 'CardanoHardForkTriggers''.
  epochs =
    [ Cfg.testShelleyHardForkAtEpoch testCfg
    , Cfg.testAllegraHardForkAtEpoch testCfg
    , Cfg.testMaryHardForkAtEpoch testCfg
    , Cfg.testAlonzoHardForkAtEpoch testCfg
    , Cfg.testBabbageHardForkAtEpoch testCfg
    , Cfg.testConwayHardForkAtEpoch testCfg
    , Cfg.testDijkstraHardForkAtEpoch testCfg
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
