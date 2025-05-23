{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tools.DBAnalyser.Block.Byron
  ( Args (..)
  , ByronBlockArgs
  , openGenesisByron
  ) where

import qualified Cardano.Chain.Block as Chain
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.UTxO as Chain
import qualified Cardano.Chain.Update as Update
import Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto as Crypto
import Cardano.Crypto.Raw (Raw)
import Cardano.Ledger.Binary (unAnnotated)
import Cardano.Tools.DBAnalyser.HasAnalysis
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import Ouroboros.Consensus.Byron.Node
  ( PBftSignatureThreshold (..)
  , ProtocolParamsByron (..)
  , protocolInfoByron
  )
import Ouroboros.Consensus.Node.ProtocolInfo
import TextBuilder (decimal)

instance HasAnalysis ByronBlock where
  countTxOutputs = aBlockOrBoundary (const 0) countTxOutputsByron
  blockTxSizes = aBlockOrBoundary (const []) blockTxSizesByron
  knownEBBs = const Byron.knownEBBs
  emitTraces _ = []
  blockStats blk =
    [ decimal $ length $ blockTxSizes blk
    , decimal $ sum $ blockTxSizes blk
    ]

  -- For the time being we do not support any block application
  -- metrics for the Byron era only.
  blockApplicationMetrics = []

instance HasProtocolInfo ByronBlock where
  data Args ByronBlock
    = ByronBlockArgs
    { configFile :: FilePath
    , requiresNetworkMagic :: RequiresNetworkMagic
    , genesisHash :: Maybe (Crypto.Hash Raw)
    , threshold :: Maybe PBftSignatureThreshold
    }
  mkProtocolInfo args = do
    config <- openGenesisByron (configFile args) (genesisHash args) (requiresNetworkMagic args)
    return $ mkByronProtocolInfo config (threshold args)

type ByronBlockArgs = Args ByronBlock

-- | Equivalent of 'either' for 'ABlockOrBoundary'.
aBlockOrBoundary ::
  (Chain.ABoundaryBlock ByteString -> a) ->
  (Chain.ABlock ByteString -> a) ->
  ByronBlock ->
  a
aBlockOrBoundary fromBoundary fromRegular blk = case blk of
  Byron.ByronBlock (Chain.ABOBBoundary boundaryBlock) _ _ ->
    fromBoundary boundaryBlock
  Byron.ByronBlock (Chain.ABOBBlock regularBlk) _ _ ->
    fromRegular regularBlk

countTxOutputsByron :: Chain.ABlock ByteString -> Int
countTxOutputsByron Chain.ABlock{Chain.blockBody} = countTxPayload bodyTxPayload
 where
  Chain.ABody{Chain.bodyTxPayload} = blockBody

  countTxPayload :: Chain.ATxPayload a -> Int
  countTxPayload =
    sum
      . map (countTx . unAnnotated . Chain.aTaTx)
      . Chain.aUnTxPayload

  countTx :: Chain.Tx -> Int
  countTx = length . Chain.txOutputs

blockTxSizesByron :: Chain.ABlock ByteString -> [SizeInBytes]
blockTxSizesByron block =
  map (fromIntegral . BL.length . BL.fromStrict . Chain.aTaAnnotation) blockTxAuxs
 where
  Chain.ABlock{Chain.blockBody} = block
  Chain.ABody{Chain.bodyTxPayload} = blockBody
  Chain.ATxPayload{Chain.aUnTxPayload = blockTxAuxs} = bodyTxPayload

openGenesisByron ::
  FilePath ->
  Maybe (Crypto.Hash Raw) ->
  RequiresNetworkMagic ->
  IO Genesis.Config
openGenesisByron configFile mHash requiresNetworkMagic = do
  genesisHash <- case mHash of
    Nothing ->
      either (error . show) return
        =<< runExceptT
          (Genesis.unGenesisHash . snd <$> Genesis.readGenesisData configFile)
    Just hash -> return hash
  genesisConfig <-
    either (error . show) return
      =<< runExceptT
        ( Genesis.mkConfigFromFile
            requiresNetworkMagic
            configFile
            genesisHash
        )
  return genesisConfig

mkByronProtocolInfo ::
  Genesis.Config ->
  Maybe PBftSignatureThreshold ->
  ProtocolInfo ByronBlock
mkByronProtocolInfo genesisConfig signatureThreshold =
  protocolInfoByron $
    ProtocolParamsByron
      { byronGenesis = genesisConfig
      , byronPbftSignatureThreshold = signatureThreshold
      , byronProtocolVersion = Update.ProtocolVersion 1 0 0
      , byronSoftwareVersion = Update.SoftwareVersion (Update.ApplicationName "db-analyser") 2
      , byronLeaderCredentials = Nothing
      }
