{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The Byron-era 'HasAnalysis' instance. db-analyser's analyses are per-era,
-- so the Cardano instance dispatches to this one. There is no Byron-only
-- db-analyser, hence no 'HasProtocolInfo' instance.
module Cardano.Tools.DBAnalyser.Block.Byron () where

import qualified Cardano.Chain.Block as Chain
import qualified Cardano.Chain.UTxO as Chain
import Cardano.Ledger.Binary (unAnnotated)
import Cardano.Tools.DBAnalyser.HasAnalysis
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
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
