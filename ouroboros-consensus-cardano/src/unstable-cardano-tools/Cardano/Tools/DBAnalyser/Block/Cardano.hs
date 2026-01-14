{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.UTxO as Shelley.UTxO
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
import Data.Functor.Const
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.SOP.BasicFunctors
import Data.SOP.Functors
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import Data.String (IsString (..))
import Lens.Micro
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
  ( IsShelleyBlock
  , ShelleyBlock
  , ShelleyBlockLedgerEra
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import System.Directory (makeAbsolute)
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
    relativeToConfig :: (FilePath -> FilePath) <-
      (</>) . takeDirectory <$> makeAbsolute configFile

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

    return $
      mkCardanoProtocolInfo
        genesisByron
        threshold
        transCfg
        initialNonce
        (cfgHardForkTriggers cc)

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
        }

data SomeTx where
  ATx :: HasAnalysis blk => TxOf blk -> SomeTx

data SomeWit where
  AWit :: HasAnalysis blk => WitsOf blk -> SomeWit

data SomeScriptType where
  AScriptType :: HasAnalysis blk => ScriptType blk -> SomeScriptType

instance HasAnalysis (CardanoBlock StandardCrypto) where
  protVer = analyseBlock protVer
  
  type TxOf (CardanoBlock StandardCrypto) = SomeTx

  txs inner =
    -- A little bit of argument shuffling to satisfy `analyseBlock`'s
    -- requirements.
    -- The Const bit is quite unfortunate.
    analyseBlock $ \ @blk -> Const . getConst . (txs @blk . to (ATx @blk)) inner

  type WitsOf (CardanoBlock StandardCrypto) = SomeWit

  wits inner (ATx @blk tx) = foldMapOf (wits @blk) (Const . getConst . inner . AWit @blk) tx
  addrWits inner (AWit @blk wit) = AWit @blk <$> addrWits @blk inner wit

  type ScriptType (CardanoBlock StandardCrypto) = SomeScriptType

  scriptWits inner (AWit @blk wit) = foldMapOf (scriptWits @blk) (Const . getConst . inner . Map.map (AScriptType @blk)) wit

  scriptSize (AScriptType @blk scr) = scriptSize @blk scr

  eraName = analyseBlock eraName

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
  Byron.Genesis.Config ->
  Maybe PBftSignatureThreshold ->
  SL.TransitionConfig L.LatestKnownEra ->
  Nonce ->
  CardanoHardForkTriggers ->
  ProtocolInfo (CardanoBlock StandardCrypto)
mkCardanoProtocolInfo genesisByron signatureThreshold transitionConfig initialNonce triggers =
  fst $
    protocolInfoCardano @_ @IO
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
 where

castHeaderHash ::
  HeaderHash ByronBlock ->
  HeaderHash (CardanoBlock StandardCrypto)
castHeaderHash = OneEraHash . toShortRawHash (Proxy @ByronBlock)

castChainHash ::
  ChainHash ByronBlock ->
  ChainHash (CardanoBlock StandardCrypto)
castChainHash GenesisHash = GenesisHash
castChainHash (BlockHash h) = BlockHash $ castHeaderHash h
