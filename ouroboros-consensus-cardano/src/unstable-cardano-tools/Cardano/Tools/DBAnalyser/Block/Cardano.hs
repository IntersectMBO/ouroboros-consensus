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
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Transition as SL
import Cardano.Ledger.BaseTypes (boundRational, unsafeNonZero)
import Cardano.Ledger.Core (TxOut)
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Node.Types (AdjustFilePaths (..))
import Cardano.Protocol.Crypto
import qualified Cardano.Tools.DBAnalyser.Block.Byron as BlockByron
import Cardano.Tools.DBAnalyser.Block.Shelley ()
import Cardano.Tools.DBAnalyser.HasAnalysis
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.Compact as Compact
import Data.Functor.Product (Product (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.SOP.BasicFunctors
import qualified Data.SOP.Match as Match
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Data.String (IsString (..))
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
import Ouroboros.Consensus.HardFork.Combinator.State (Current, currentState)
import Ouroboros.Consensus.Ledger.Abstract hiding (TxIn, TxOut)
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( IsShelleyBlock
  , ShelleyBlock
  , ShelleyBlockLedgerEra
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.TypeFamilyWrappers (WrapValues (..))
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
analyseWithLedgerState f (WithLedgerState cb sb vsb sa vsa) =
  hcollapse
    . hcmap p (K . f)
    . fromJust
    . hsequence'
    $ hzipWith3
      zipLS
      (hzipWith combineMaybe (goLS sb) (goVals vsb))
      (hzipWith combineMaybe (goLS sa) (goVals vsa))
      oeb
 where
  p :: Proxy HasAnalysis
  p = Proxy

  combineMaybe ::
    (Maybe :.: LedgerState) blk ->
    (Maybe :.: WrapValues) blk ->
    (Maybe :.: Product LedgerState WrapValues) blk
  combineMaybe (Comp ms) (Comp mv) = Comp (Pair <$> ms <*> mv)

  zipLS ::
    (Maybe :.: Product LedgerState WrapValues) blk ->
    (Maybe :.: Product LedgerState WrapValues) blk ->
    I blk ->
    (Maybe :.: WithLedgerState) blk
  zipLS
    (Comp (Just (Pair sb' (WrapValues vsb'))))
    (Comp (Just (Pair sa' (WrapValues vsa'))))
    (I blk) =
      Comp . Just $ WithLedgerState blk sb' vsb' sa' vsa'
  zipLS _ _ _ = Comp Nothing

  oeb = getOneEraBlock . getHardForkBlock $ cb

  goLS ::
    LedgerState (CardanoBlock StandardCrypto) ->
    NP (Maybe :.: LedgerState) (CardanoEras StandardCrypto)
  goLS =
    hexpand (Comp Nothing)
      . hmap (Comp . Just . currentState)
      . Telescope.tip
      . getHardForkState
      . hardForkLedgerStatePerEra

  goVals ::
    Values (CardanoBlock StandardCrypto) ->
    NP (Maybe :.: WrapValues) (CardanoEras StandardCrypto)
  goVals =
    hexpand (Comp Nothing)
      . hmap (Comp . Just)

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

    cc :: CardanoConfig <-
      either (error . show) (return . adjustFilePaths relativeToConfig)
        =<< Aeson.eitherDecodeFileStrict' configFile

    genesisByron <-
      BlockByron.openGenesisByron (byronGenesisPath cc) (byronGenesisHash cc) (requiresNetworkMagic cc)
    genesisShelley <-
      either (error . show) return
        =<< Aeson.eitherDecodeFileStrict' (shelleyGenesisPath cc)
    genesisAlonzo <-
      either (error . show) return
        =<< Aeson.eitherDecodeFileStrict' (alonzoGenesisPath cc)
    genesisConway <-
      either (error . show) return
        =<< Aeson.eitherDecodeFileStrict' (conwayGenesisPath cc)
    genesisDijkstra <- case dijkstraGenesisPath cc of
      Nothing -> pure emptyDijkstraGenesis
      Just fp ->
        either (error . show) return
          =<< Aeson.eitherDecodeFileStrict' fp

    let transCfg =
          SL.mkLatestTransitionConfig genesisShelley genesisAlonzo genesisConway genesisDijkstra

    initialNonce <- case shelleyGenesisHash cc of
      Just h -> pure h
      Nothing -> do
        content <- BS.readFile (shelleyGenesisPath cc)
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
      (cfgHardForkTriggers cc)

  mkLSMConfig CardanoBlockArgs{configFile} = do
    -- The export path is interpreted relative to the LedgerDB filesystem root,
    -- not the config file, so we read the config without adjusting file paths.
    cc :: CardanoConfig <-
      either (error . show) return
        =<< Aeson.eitherDecodeFileStrict' configFile
    pure
      LSMConfig
        { lsmConfigExportPath = lsmLedgerDBExportPath cc
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

data CardanoConfig = CardanoConfig
  { requiresNetworkMagic :: RequiresNetworkMagic
  -- ^ @RequiresNetworkMagic@ field
  , byronGenesisPath :: FilePath
  -- ^ @ByronGenesisFile@ field
  , byronGenesisHash :: Maybe (Crypto.Hash Raw)
  -- ^ @ByronGenesisHash@ field
  , shelleyGenesisPath :: FilePath
  -- ^ @ShelleyGenesisFile@ field
  -- | @ShelleyGenesisHash@ field
  , shelleyGenesisHash :: Maybe Nonce
  , alonzoGenesisPath :: FilePath
  -- ^ @AlonzoGenesisFile@ field
  , conwayGenesisPath :: FilePath
  -- ^ @ConwayGenesisFile@ field
  , dijkstraGenesisPath :: Maybe FilePath
  -- ^ @DijkstraGenesisFile@ field
  , cfgHardForkTriggers :: CardanoHardForkTriggers
  -- ^ @Test*HardForkAtEpoch@ for each Shelley era
  , lsmLedgerDBExportPath :: Maybe FilePath
  -- ^ @LedgerDB.LSMExportPath@ field: the directory (relative to the LSM-trees
  -- LedgerDB filesystem root) into which the LSM backend exports snapshots as it
  -- takes them. Only meaningful for the LSM backend.
  }

instance AdjustFilePaths CardanoConfig where
  adjustFilePaths f cc =
    cc
      { byronGenesisPath = f $ byronGenesisPath cc
      , shelleyGenesisPath = f $ shelleyGenesisPath cc
      , alonzoGenesisPath = f $ alonzoGenesisPath cc
      , conwayGenesisPath = f $ conwayGenesisPath cc
      , dijkstraGenesisPath = f <$> dijkstraGenesisPath cc
      -- Byron, Shelley, Alonzo, and Conway are the only eras that have genesis
      -- data. The actual genesis block is a Byron block, therefore we needed a
      -- genesis file. To transition to Shelley, we needed to add some additional
      -- genesis data (eg some initial values of new protocol parametrers like
      -- @d@). Similarly in Alonzo (eg Plutus interpreter parameters/limits) and
      -- in Conway too (ie keys of the new genesis delegates).
      --
      -- In contrast, the Allegra, Mary, and Babbage eras did not introduce any new
      -- genesis data.
      }

instance Aeson.FromJSON CardanoConfig where
  parseJSON = Aeson.withObject "CardanoConfigFile" $ \v -> do
    requiresNetworkMagic <- v Aeson..: "RequiresNetworkMagic"

    byronGenesisPath <- v Aeson..: "ByronGenesisFile"
    byronGenesisHash <- v Aeson..:? "ByronGenesisHash"

    shelleyGenesisPath <- v Aeson..: "ShelleyGenesisFile"
    shelleyGenesisHash <-
      v Aeson..:? "ShelleyGenesisHash" >>= \case
        Nothing -> pure Nothing
        Just hex -> case CryptoClass.hashFromTextAsHex hex of
          Nothing -> fail "could not parse ShelleyGenesisHash as a hex string"
          Just h -> pure $ Just $ Nonce h

    alonzoGenesisPath <- v Aeson..: "AlonzoGenesisFile"

    conwayGenesisPath <- v Aeson..: "ConwayGenesisFile"

    dijkstraGenesisPath <- v Aeson..:? "DijkstraGenesisFile"

    -- The LSM settings live in the @LedgerDB@ object (the rest of which is
    -- parsed by the node, not here). @LSMExportPath@ is a directory path.
    lsmLedgerDBExportPath <-
      v Aeson..:? "LedgerDB" >>= \case
        Nothing -> pure Nothing
        Just ledgerDB -> ledgerDB Aeson..:? "LSMExportPath"

    triggers <- do
      let parseTrigger ::
            forall blk era.
            (IsShelleyBlock blk, ShelleyBlockLedgerEra blk ~ era) =>
            (Aeson.Parser :.: CardanoHardForkTrigger) blk
          parseTrigger =
            Comp $
              (fmap CardanoTriggerHardForkAtEpoch <$> (v Aeson..:? nm))
                Aeson..!= CardanoTriggerHardForkAtDefaultVersion
           where
            nm = fromString $ "Test" <> L.eraName @era <> "HardForkAtEpoch"

      triggers <- hsequence' $ hcpure (Proxy @IsShelleyBlock) parseTrigger

      let isBad :: NP CardanoHardForkTrigger xs -> Bool
          isBad = \case
            CardanoTriggerHardForkAtDefaultVersion
              :* CardanoTriggerHardForkAtEpoch{}
              :* _ -> True
            _ :* np -> isBad np
            Nil -> False
      fmap (\() -> triggers) $
        when (isBad triggers) $
          fail $
            "if the Cardano config file sets a Test*HardForkEpoch,"
              <> " it must also set it for all previous eras."

    pure $
      CardanoConfig
        { requiresNetworkMagic = requiresNetworkMagic
        , byronGenesisPath = byronGenesisPath
        , byronGenesisHash = byronGenesisHash
        , shelleyGenesisPath = shelleyGenesisPath
        , shelleyGenesisHash = shelleyGenesisHash
        , alonzoGenesisPath = alonzoGenesisPath
        , conwayGenesisPath = conwayGenesisPath
        , dijkstraGenesisPath = dijkstraGenesisPath
        , cfgHardForkTriggers = CardanoHardForkTriggers triggers
        , lsmLedgerDBExportPath = lsmLedgerDBExportPath
        }

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
      , \(WithLedgerState blk _preSt _preVals _postSt _postVals) ->
          pure $ Builder.decimal $ unSlotNo $ blockSlot blk
      )
    ,
      ( "Block Number"
      , \(WithLedgerState blk _preSt _preVals _postSt _postVals) ->
          pure $ Builder.decimal $ unBlockNo $ blockNo blk
      )
    , -- TODO the values only contain the outputs produced by the block, not the
      -- whole UTxO set, so there is a regression here.

      ( "UTxO size (via Compact)"
      , \(WithLedgerState _blk _preSt _preVals postSt postVals) -> do
          let compactSize utxo = do
                compactedUtxo <- Compact.compact utxo
                compactedUtxoSize <- Compact.compactSize compactedUtxo
                pure $ Builder.decimal $ compactedUtxoSize

          dispatch
            postSt
            postVals
            (applyToByronUtxo compactSize)
            (applyToShelleyBasedUtxo compactSize)
      )
    ,
      ( "UTxO map size"
      , \(WithLedgerState _blk _preSt _preVals postSt postVals) -> do
          let mapSize = pure . Builder.decimal . Map.size
          dispatch
            postSt
            postVals
            (applyToByronUtxo mapSize)
            (applyToShelleyBasedUtxo mapSize)
      )
    ]

-- | Dispatch a per-era UTxO computation to the current era. The Byron UTxO
-- still lives in the ledger /state/ (Byron has no on-disk HD tables), so the
-- Byron arm reads the state; the Shelley-based UTxO lives in the @'Values'@
-- (the @mk@-free design moved it out of the state), so those arms read the
-- supplied values for that era.
dispatch ::
  LedgerState (CardanoBlock StandardCrypto) ->
  Values (CardanoBlock StandardCrypto) ->
  (LedgerState ByronBlock -> IO TextBuilder) ->
  (forall era. Map TxIn (TxOut era) -> IO TextBuilder) ->
  IO TextBuilder
dispatch cardanoSt cardanoVals fByron fShelley =
  case Match.matchNS
    (Telescope.tip (getHardForkState (hardForkLedgerStatePerEra cardanoSt)))
    cardanoVals of
    Left _ -> error "dispatch: current era of state and values disagree"
    Right matched ->
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
          matched
 where
  k_fByron ::
    Product (Current LedgerState) WrapValues ByronBlock ->
    K (IO TextBuilder) ByronBlock
  k_fByron (Pair st _vals) = K . fByron $ currentState st

  k_fShelley ::
    forall proto era.
    Product (Current LedgerState) WrapValues (ShelleyBlock proto era) ->
    K (IO TextBuilder) (ShelleyBlock proto era)
  k_fShelley (Pair _st (WrapValues vals)) = K $ fShelley vals

applyToByronUtxo ::
  (Map Byron.UTxO.CompactTxIn Byron.UTxO.CompactTxOut -> IO TextBuilder) ->
  LedgerState ByronBlock ->
  IO TextBuilder
applyToByronUtxo f st =
  f $ getByronUtxo st

getByronUtxo ::
  LedgerState ByronBlock ->
  Map Byron.UTxO.CompactTxIn Byron.UTxO.CompactTxOut
getByronUtxo =
  Byron.UTxO.unUTxO
    . Byron.Block.cvsUtxo
    . Byron.Ledger.byronLedgerState

-- | The Shelley-based UTxO now /is/ the era's @'Values'@ (a @Map TxIn TxOut@).
applyToShelleyBasedUtxo ::
  (Map TxIn (TxOut era) -> IO TextBuilder) ->
  Map TxIn (TxOut era) ->
  IO TextBuilder
applyToShelleyBasedUtxo f vals = f vals

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
