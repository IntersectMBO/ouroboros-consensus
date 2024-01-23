{-# LANGUAGE ApplicativeDo           #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tools.DBAnalyser.Block.Cardano (
    Args (configFile, threshold, CardanoBlockArgs)
  , CardanoBlockArgs
  ) where

import qualified Cardano.Chain.Genesis as Byron.Genesis
import qualified Cardano.Chain.Update as Byron.Update
import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Hash.Class as CryptoClass
import           Cardano.Crypto.Raw (Raw)
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Transition as L
import           Cardano.Ledger.Binary.Version (getVersion)
import           Cardano.Ledger.Crypto
import           Cardano.Node.Types (AdjustFilePaths (..))
import qualified Cardano.Tools.DBAnalyser.Block.Byron as BlockByron
import           Cardano.Tools.DBAnalyser.Block.Shelley ()
import           Cardano.Tools.DBAnalyser.HasAnalysis
import           Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.SOP.BasicFunctors
import           Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import           Data.String (IsString (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Cardano.Block (CardanoEras)
import           Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..),
                     protocolInfoCardano')
import           Ouroboros.Consensus.Cardano.Node.ProtocolVersions
                     (CardanoProtocolVersions (..),
                     cardanoMainnetProtocolVersions)
import           Ouroboros.Consensus.HardFork.Combinator (HardForkBlock (..),
                     OneEraBlock (..), OneEraHash (..), getHardForkState,
                     hardForkLedgerStatePerEra)
import           Ouroboros.Consensus.HardFork.Combinator.State (currentState)
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip)
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           System.Directory (makeAbsolute)
import           System.FilePath (takeDirectory, (</>))

analyseBlock ::
     (forall blk. HasAnalysis blk => blk -> a)
  -> CardanoBlock StandardCrypto -> a
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

    zipLS (Comp (Just sb')) (Comp (Just sa')) (I blk) =
      Comp . Just $ WithLedgerState blk sb' sa'
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

instance HasProtocolInfo (CardanoBlock StandardCrypto) where
  data Args (CardanoBlock StandardCrypto) = CardanoBlockArgs {
          configFile           :: FilePath
        , threshold            :: Maybe PBftSignatureThreshold
        }

  mkProtocolInfo CardanoBlockArgs{configFile, threshold} = do
    relativeToConfig :: (FilePath -> FilePath) <-
        (</>) . takeDirectory <$> makeAbsolute configFile

    cc :: CardanoConfig <-
      either (error . show) (return . adjustFilePaths relativeToConfig) =<<
        Aeson.eitherDecodeFileStrict' configFile

    genesisByron   <-
      BlockByron.openGenesisByron (byronGenesisPath cc) (byronGenesisHash cc) (requiresNetworkMagic cc)
    genesisShelley <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' (shelleyGenesisPath cc)
    genesisAlonzo  <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' (alonzoGenesisPath cc)
    genesisConway  <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' (conwayGenesisPath cc)

    let transCfg =
          L.mkLatestTransitionConfig genesisShelley genesisAlonzo genesisConway

    initialNonce <- case shelleyGenesisHash cc of
      Just h  -> pure h
      Nothing -> do
        content <- BS.readFile (shelleyGenesisPath cc)
        pure
          $ Nonce
          $ CryptoClass.castHash
          $ CryptoClass.hashWith id
          $ content

    return
      $ mkCardanoProtocolInfo
          genesisByron
          threshold
          transCfg
          initialNonce
          (cfgHardForkTriggers cc)
          (cardanoMainnetProtocolVersions False)

data CardanoConfig = CardanoConfig {
    -- | @RequiresNetworkMagic@ field
    requiresNetworkMagic :: RequiresNetworkMagic

     -- | @ByronGenesisFile@ field
  , byronGenesisPath     :: FilePath
    -- | @ByronGenesisHash@ field
  , byronGenesisHash     :: Maybe (Crypto.Hash Raw)

    -- | @ShelleyGenesisFile@ field
    -- | @ShelleyGenesisHash@ field
  , shelleyGenesisPath   :: FilePath
  , shelleyGenesisHash   :: Maybe Nonce

    -- | @AlonzoGenesisFile@ field
  , alonzoGenesisPath    :: FilePath

    -- | @ConwayGenesisFile@ field
  , conwayGenesisPath    :: FilePath

    -- | @Test*HardForkAtEpoch@ for each Shelley era
  , cfgHardForkTriggers  :: CardanoHardForkTriggers
  }

instance AdjustFilePaths CardanoConfig where
    adjustFilePaths f cc =
        cc {
            byronGenesisPath    = f $ byronGenesisPath cc
          , shelleyGenesisPath  = f $ shelleyGenesisPath cc
          , alonzoGenesisPath   = f $ alonzoGenesisPath cc
          , conwayGenesisPath   = f $ conwayGenesisPath cc
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

type family ShelleyBlockEra blk where
  ShelleyBlockEra (ShelleyBlock proto era) = era

class L.Era (ShelleyBlockEra blk) => IsShelleyBlock blk
instance L.Era era => IsShelleyBlock (ShelleyBlock proto era)

instance Aeson.FromJSON CardanoConfig where
  parseJSON = Aeson.withObject "CardanoConfigFile" $ \v -> do

    requiresNetworkMagic <- v Aeson..: "RequiresNetworkMagic"

    byronGenesisPath <- v Aeson..:  "ByronGenesisFile"
    byronGenesisHash <- v Aeson..:? "ByronGenesisHash"

    shelleyGenesisPath <- v Aeson..: "ShelleyGenesisFile"
    shelleyGenesisHash <- v Aeson..:? "ShelleyGenesisHash" >>= \case
      Nothing -> pure Nothing
      Just hex -> case CryptoClass.hashFromTextAsHex hex of
        Nothing -> fail "could not parse ShelleyGenesisHash as a hex string"
        Just h  -> pure $ Just $ Nonce h

    alonzoGenesisPath <- v Aeson..: "AlonzoGenesisFile"

    conwayGenesisPath <- v Aeson..: "ConwayGenesisFile"

    triggers <- do
      let parseTrigger ::
               forall blk era. (IsShelleyBlock blk, ShelleyBlockEra blk ~ era)
            => (Aeson.Parser :.: K TriggerHardFork) blk
          parseTrigger = Comp $ fmap K $
                        (fmap TriggerHardForkAtEpoch <$> (v Aeson..:? nm))
              Aeson..!= TriggerHardForkAtVersion (getVersion (L.eraProtVerLow @era))
            where
              nm = fromString $ "Test" <> L.eraName @era <> "HardForkAtEpoch"

      triggers <- hsequence' $ hcpure (Proxy @IsShelleyBlock) parseTrigger

      let isBad :: NP (K TriggerHardFork) xs -> Bool
          isBad = \case
            K TriggerHardForkAtVersion{} :* K TriggerHardForkAtEpoch{} :* _ -> True
            K{} :* np -> isBad np
            Nil       -> False
      fmap (\() -> triggers) $ when (isBad triggers) $ fail $
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
        , cfgHardForkTriggers = CardanoHardForkTriggers triggers
        }

instance (HasAnnTip (CardanoBlock StandardCrypto), GetPrevHash (CardanoBlock StandardCrypto)) => HasAnalysis (CardanoBlock StandardCrypto) where
  countTxOutputs = analyseBlock countTxOutputs
  blockTxSizes   = analyseBlock blockTxSizes
  knownEBBs _    =
      Map.mapKeys castHeaderHash . Map.map castChainHash $
        knownEBBs (Proxy @ByronBlock)

  emitTraces = analyseWithLedgerState emitTraces

  blockStats = analyseBlock blockStats

type CardanoBlockArgs = Args (CardanoBlock StandardCrypto)

mkCardanoProtocolInfo ::
     Byron.Genesis.Config
  -> Maybe PBftSignatureThreshold
  -> L.TransitionConfig (L.LatestKnownEra StandardCrypto)
  -> Nonce
  -> CardanoHardForkTriggers
  -> CardanoProtocolVersions StandardCrypto
  -> ProtocolInfo (CardanoBlock StandardCrypto)
mkCardanoProtocolInfo genesisByron signatureThreshold transitionConfig initialNonce triggers protocolVersions =
    fst $ protocolInfoCardano' @_ @IO
      protocolVersions
      (CardanoPartialProtocolParams
        PartialProtocolParamsByron {
            pbyronGenesis                = genesisByron
          , pbyronPbftSignatureThreshold = signatureThreshold
          , pbyronSoftwareVersion        = Byron.Update.SoftwareVersion (Byron.Update.ApplicationName "db-analyser") 2
          , pbyronLeaderCredentials      = Nothing
          , pbyronMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        ProtocolParamsShelleyBased {
            shelleyBasedInitialNonce      = initialNonce
          , shelleyBasedLeaderCredentials = []
          }
        PartialProtocolParamsShelley {
            pshelleyMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        PartialProtocolParamsAllegra {
            pallegraMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        PartialProtocolParamsMary {
            pmaryMaxTxCapacityOverrides    = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        PartialProtocolParamsAlonzo {
            palonzoMaxTxCapacityOverrides  = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        PartialProtocolParamsBabbage {
            pbabbageMaxTxCapacityOverrides  = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        PartialProtocolParamsConway {
            pconwayMaxTxCapacityOverrides   = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        triggers
        transitionConfig
      )
  where

castHeaderHash ::
     HeaderHash ByronBlock
  -> HeaderHash (CardanoBlock StandardCrypto)
castHeaderHash = OneEraHash . toShortRawHash (Proxy @ByronBlock)

castChainHash ::
     ChainHash ByronBlock
  -> ChainHash (CardanoBlock StandardCrypto)
castChainHash GenesisHash   = GenesisHash
castChainHash (BlockHash h) = BlockHash $ castHeaderHash h
