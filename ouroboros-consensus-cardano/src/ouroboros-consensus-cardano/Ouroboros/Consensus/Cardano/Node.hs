{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableSuperClasses  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Disable completeness checks on GHC versions pre-9.6, where this can be
-- exceptionally slow:
#if __GLASGOW_HASKELL__ <= 906
{-# OPTIONS_GHC -Wno-incomplete-patterns
                -Wno-incomplete-uni-patterns
                -Wno-incomplete-record-updates
                -Wno-overlapping-patterns #-}
#endif

-- TODO: this is required for ghc-8.10.7, because using NamedFieldPuns and
-- PatternSynonyms with record syntax results in warnings related to shadowing.
-- This can be removed once we drop ghc-8.10.7.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ouroboros.Consensus.Cardano.Node (
    CardanoHardForkConstraints
  , CardanoHardForkTriggers (.., CardanoHardForkTriggers', triggerHardForkShelley, triggerHardForkAllegra, triggerHardForkMary, triggerHardForkAlonzo, triggerHardForkBabbage, triggerHardForkConway)
  , CardanoProtocolParams
  , MaxMajorProtVer (..)
  , ProtocolParams (.., CardanoProtocolParams, paramsByron, paramsShelleyBased, paramsShelley, paramsAllegra, paramsMary, paramsAlonzo, paramsBabbage, paramsConway, hardForkTriggers, ledgerTransitionConfig, checkpoints)
  , TriggerHardFork (..)
  , protocolClientInfoCardano
  , protocolInfoCardano
    -- * SupportedNetworkProtocolVersion
  , pattern CardanoNodeToClientVersion1
  , pattern CardanoNodeToClientVersion10
  , pattern CardanoNodeToClientVersion11
  , pattern CardanoNodeToClientVersion12
  , pattern CardanoNodeToClientVersion2
  , pattern CardanoNodeToClientVersion3
  , pattern CardanoNodeToClientVersion4
  , pattern CardanoNodeToClientVersion5
  , pattern CardanoNodeToClientVersion6
  , pattern CardanoNodeToClientVersion7
  , pattern CardanoNodeToClientVersion8
  , pattern CardanoNodeToClientVersion9
  , pattern CardanoNodeToNodeVersion1
  , pattern CardanoNodeToNodeVersion2
  ) where

import           Cardano.Binary (DecoderError (..), enforceSize)
import           Cardano.Chain.Slotting (EpochSlots)
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Transition as L
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Prelude (cborError)
import qualified Cardano.Protocol.TPraos.OCert as Absolute (KESPeriod (..),
                     ocertKESPeriod)
import           Cardano.Slotting.Time (SystemStart (SystemStart))
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Exception (assert)
import qualified Data.ByteString.Short as Short
import           Data.Functor.These (These1 (..))
import qualified Data.Map.Strict as Map
import           Data.SOP.BasicFunctors
import           Data.SOP.Counting
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index (Index (..))
import           Data.SOP.NonEmpty
import           Data.SOP.OptNP (NonEmptyOptNP, OptNP (OptSkip))
import qualified Data.SOP.OptNP as OptNP
import           Data.SOP.Strict
import           Data.Word (Word16, Word64)
import           Lens.Micro ((^.))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Conversions as Byron
import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Byron.Node
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.Ledger ()
import           Ouroboros.Consensus.Cardano.QueryHF ()
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
                     (PerEraProtocolParams (..))
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Praos (Praos, PraosParams (..))
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (praosCanBeLeaderOpCert)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos, TPraosParams (..))
import qualified Ouroboros.Consensus.Protocol.TPraos as Shelley
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Ledger.Block (IsShelleyBlock,
                     ShelleyBlockLedgerEra)
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Node.Common (ShelleyEraWithCrypto,
                     shelleyBlockIssuerVKey)
import qualified Ouroboros.Consensus.Shelley.Node.Praos as Praos
import qualified Ouroboros.Consensus.Shelley.Node.TPraos as TPraos
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.IOLike
{-------------------------------------------------------------------------------
  SerialiseHFC
-------------------------------------------------------------------------------}

instance SerialiseConstraintsHFC ByronBlock

-- | Important: we need to maintain binary compatibility with Byron blocks, as
-- they are already stored on disk.
--
-- We also want to be able to efficiently detect (without having to peek far
-- ahead) whether we're dealing with a Byron or Shelley block, so that we can
-- invoke the right decoder. We plan to have a few more hard forks after
-- Shelley (Goguen, Basho, Voltaire), so we want a future-proof envelope for
-- distinguishing the different block types, i.e., a byte indicating the era.
--
-- Byron does not provide such an envelope. However, a Byron block is a CBOR
-- 2-tuple with the first element being a tag ('Word': 0 = EBB; 1 = regular
-- block) and the second being the payload. We can easily extend this encoding
-- format with support for Shelley, Goguen, etc.
--
-- We encode a 'CardanoBlock' as the same CBOR 2-tuple as a Byron block, but
-- we use the tags after 1 for the hard forks after Byron:
--
-- 0. Byron EBB
-- 1. Byron regular block
-- 2. Shelley block
-- 3. Allegra block
-- 4. Mary block
-- 5. Goguen block
-- 6. etc.
--
-- For more details, see:
-- <https://github.com/IntersectMBO/ouroboros-network/pull/1175#issuecomment-558147194>
instance CardanoHardForkConstraints c => SerialiseHFC (CardanoEras c) where
  encodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley ccfgAllegra ccfgMary ccfgAlonzo ccfgBabbage ccfgConway) = \case
      -- We are backwards compatible with Byron and thus use the exact same
      -- encoding.
      BlockByron   blockByron   ->                encodeDisk ccfgByron blockByron
      -- For Shelley and later eras, we need to prepend the hard fork envelope.
      BlockShelley blockShelley -> prependTag 2 $ encodeDisk ccfgShelley blockShelley
      BlockAllegra blockAllegra -> prependTag 3 $ encodeDisk ccfgAllegra blockAllegra
      BlockMary    blockMary    -> prependTag 4 $ encodeDisk ccfgMary    blockMary
      BlockAlonzo  blockAlonzo  -> prependTag 5 $ encodeDisk ccfgAlonzo  blockAlonzo
      BlockBabbage blockBabbage -> prependTag 6 $ encodeDisk ccfgBabbage blockBabbage
      BlockConway  blockConway  -> prependTag 7 $ encodeDisk ccfgConway  blockConway
  decodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley ccfgAllegra ccfgMary ccfgAlonzo ccfgBabbage ccfgConway) = do
      enforceSize "CardanoBlock" 2
      CBOR.decodeWord >>= \case
        0 -> fmap BlockByron   <$> Byron.decodeByronBoundaryBlock epochSlots
        1 -> fmap BlockByron   <$> Byron.decodeByronRegularBlock  epochSlots
        -- We don't have to drop the first two bytes from the 'ByteString'
        -- passed to the decoder as slicing already takes care of this.
        2 -> fmap BlockShelley <$> decodeDisk ccfgShelley
        3 -> fmap BlockAllegra <$> decodeDisk ccfgAllegra
        4 -> fmap BlockMary    <$> decodeDisk ccfgMary
        5 -> fmap BlockAlonzo  <$> decodeDisk ccfgAlonzo
        6 -> fmap BlockBabbage <$> decodeDisk ccfgBabbage
        7 -> fmap BlockConway  <$> decodeDisk ccfgConway
        t -> cborError $ DecoderErrorUnknownTag "CardanoBlock" (fromIntegral t)
    where
      epochSlots = Byron.getByronEpochSlots ccfgByron

  reconstructHfcPrefixLen _ = PrefixLen 2

  reconstructHfcNestedCtxt _ prefix blockSize =
      case Short.index prefix 1 of
        0 -> SomeSecond $ NestedCtxt (NCZ (Byron.CtxtByronBoundary blockSize))
        1 -> SomeSecond $ NestedCtxt (NCZ (Byron.CtxtByronRegular  blockSize))
        2 -> SomeSecond $ NestedCtxt (NCS (NCZ Shelley.CtxtShelley))
        3 -> SomeSecond $ NestedCtxt (NCS (NCS (NCZ Shelley.CtxtShelley)))
        4 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCZ Shelley.CtxtShelley))))
        5 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCS (NCZ Shelley.CtxtShelley)))))
        6 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCS (NCS (NCZ Shelley.CtxtShelley))))))
        7 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCS (NCS (NCS (NCZ Shelley.CtxtShelley)))))))
        _ -> error $ "CardanoBlock: invalid prefix " <> show prefix

  getHfcBinaryBlockInfo = \case
      BlockByron   blockByron   ->
        getBinaryBlockInfo blockByron
      -- For Shelley and the later eras, we need to account for the two extra
      -- bytes of the envelope.
      BlockShelley blockShelley ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockShelley
      BlockAllegra blockAllegra ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockAllegra
      BlockMary blockMary ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockMary
      BlockAlonzo blockAlonzo ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockAlonzo
      BlockBabbage blockBabbage ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockBabbage
      BlockConway blockConway ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockConway
    where
      shiftHeaderOffset :: Word16 -> BinaryBlockInfo -> BinaryBlockInfo
      shiftHeaderOffset shift binfo = binfo {
            headerOffset = headerOffset binfo + shift
          }

  estimateHfcBlockSize = \case
      HeaderByron   headerByron   -> estimateBlockSize headerByron
      -- For Shelley and later eras, we add two extra bytes, see the
      -- 'SerialiseHFC' instance.
      HeaderShelley headerShelley -> estimateBlockSize headerShelley + 2
      HeaderAllegra headerAllegra -> estimateBlockSize headerAllegra + 2
      HeaderMary    headerMary    -> estimateBlockSize headerMary    + 2
      HeaderAlonzo  headerAlonzo  -> estimateBlockSize headerAlonzo  + 2
      HeaderBabbage headerBabbage -> estimateBlockSize headerBabbage + 2
      HeaderConway  headerConway  -> estimateBlockSize headerConway  + 2

-- | Prepend the given tag by creating a CBOR 2-tuple with the tag as the
-- first element and the given 'Encoding' as the second.
prependTag :: Word -> Encoding -> Encoding
prependTag tag payload = mconcat [
      CBOR.encodeListLen 2
    , CBOR.encodeWord tag
    , payload
    ]

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- Note: we don't support all combinations, so we don't declare them as
-- COMPLETE

-- | We support only Byron V1 with the hard fork disabled, as no other
-- versions have been released before the hard fork
pattern CardanoNodeToNodeVersion1 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion1 =
    HardForkNodeToNodeDisabled ByronNodeToNodeVersion1

-- | The hard fork enabled using the latest version of Byron and Shelley for
-- each Byron and Shelley era.
pattern CardanoNodeToNodeVersion2 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion2 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  WrapNodeToNodeVersion ByronNodeToNodeVersion2
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* Nil
      )

-- | We support the sole Byron version with the hard fork disabled.
pattern CardanoNodeToClientVersion1 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion1 =
    HardForkNodeToClientDisabled ByronNodeToClientVersion1

-- | The hard fork enabled and the Shelley era enabled.
pattern CardanoNodeToClientVersion2 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion2 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion1
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled and the Shelley era enabled, but using
-- 'ShelleyNodeToClientVersion2' and 'HardForkSpecificNodeToClientVersion2'.
pattern CardanoNodeToClientVersion3 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion3 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley and Allegra eras enabled.
--
-- We don't bother with 'ShelleyNodeToClientVersion1' and
-- 'HardForkSpecificNodeToClientVersion1'.
pattern CardanoNodeToClientVersion4 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion4 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, and Mary eras enabled.
--
-- We don't bother with 'ShelleyNodeToClientVersion1'.
pattern CardanoNodeToClientVersion5 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion5 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, and Mary eras enabled, but
-- using 'ShelleyNodeToClientVersion3' for the Shelley-based eras , which
-- enables new queries.
pattern CardanoNodeToClientVersion6 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion6 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary and Alonzo eras enabled
pattern CardanoNodeToClientVersion7 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion7 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion4
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion4
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion4
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion4
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary and Alonzo eras enabled
-- Using 'ShelleyNodeToClientVersion5' for the Shelley-based eras , which
-- enables new queries.
pattern CardanoNodeToClientVersion8 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion8 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- eras enabled Using 'ShelleyNodeToClientVersion5' for the Shelley-based eras,
-- which enables new queries.
pattern CardanoNodeToClientVersion9 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion9 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- eras enabled Using 'ShelleyNodeToClientVersion6' for the Shelley-based eras,
-- which enables new queries.
pattern CardanoNodeToClientVersion10 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion10 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- eras enabled, using 'ShelleyNodeToClientVersion7' for the Shelley-based eras,
-- which enables new queries.
pattern CardanoNodeToClientVersion11 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion11 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion7
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion7
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion7
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion7
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion7
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- and Conway eras enabled, using 'ShelleyNodeToClientVersion7' for the
-- Shelley-based eras.
pattern CardanoNodeToClientVersion12 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion12 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion3
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion8
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion8
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion8
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion8
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion8
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion8
      :* Nil
      )

instance CardanoHardForkConstraints c
      => SupportedNetworkProtocolVersion (CardanoBlock c) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (NodeToNodeV_7, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_8, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_9, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_10, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_11, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_12, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_13, CardanoNodeToNodeVersion2)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (NodeToClientV_9 , CardanoNodeToClientVersion7)
      , (NodeToClientV_10, CardanoNodeToClientVersion7)
      , (NodeToClientV_11, CardanoNodeToClientVersion8)
      , (NodeToClientV_12, CardanoNodeToClientVersion8)
      , (NodeToClientV_13, CardanoNodeToClientVersion9)
      , (NodeToClientV_14, CardanoNodeToClientVersion10)
      , (NodeToClientV_15, CardanoNodeToClientVersion11)
      , (NodeToClientV_16, CardanoNodeToClientVersion12)
      ]

  latestReleasedNodeVersion _prx = (Just NodeToNodeV_13, Just NodeToClientV_15)

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

newtype CardanoHardForkTriggers = CardanoHardForkTriggers {
    getCardanoHardForkTriggers :: NP (K TriggerHardFork) (CardanoShelleyEras StandardCrypto)
  }

pattern CardanoHardForkTriggers' ::
     TriggerHardFork
  -> TriggerHardFork
  -> TriggerHardFork
  -> TriggerHardFork
  -> TriggerHardFork
  -> TriggerHardFork
  -> CardanoHardForkTriggers
pattern CardanoHardForkTriggers' {
        triggerHardForkShelley
      , triggerHardForkAllegra
      , triggerHardForkMary
      , triggerHardForkAlonzo
      , triggerHardForkBabbage
      , triggerHardForkConway
      } =
    CardanoHardForkTriggers
      (  K triggerHardForkShelley
      :* K triggerHardForkAllegra
      :* K triggerHardForkMary
      :* K triggerHardForkAlonzo
      :* K triggerHardForkBabbage
      :* K triggerHardForkConway
      :* Nil
      )
{-# COMPLETE CardanoHardForkTriggers' #-}

-- | Parameters needed to run Cardano.
data instance ProtocolParams (CardanoBlock c) = ProtocolParamsCardano {
    cardanoProtocolParamsPerEra   :: PerEraProtocolParams (CardanoEras c)
  , shelleyBasedProtocolParams    :: ProtocolParamsShelleyBased c
  , cardanoHardForkTriggers       :: CardanoHardForkTriggers
  , cardanoLedgerTransitionConfig :: L.TransitionConfig (L.LatestKnownEra c)
  , cardanoCheckpoints            :: CheckpointsMap (CardanoBlock c)
  }

type CardanoProtocolParams c = ProtocolParams (CardanoBlock c)

pattern CardanoProtocolParams ::
     ProtocolParams ByronBlock
  -> ProtocolParamsShelleyBased c
  -> ProtocolParams (ShelleyBlock (TPraos c) (ShelleyEra c))
  -> ProtocolParams (ShelleyBlock (TPraos c) (AllegraEra c))
  -> ProtocolParams (ShelleyBlock (TPraos c) (MaryEra    c))
  -> ProtocolParams (ShelleyBlock (TPraos c) (AlonzoEra  c))
  -> ProtocolParams (ShelleyBlock (Praos  c) (BabbageEra c))
  -> ProtocolParams (ShelleyBlock (Praos  c) (ConwayEra  c))
  -> CardanoHardForkTriggers
  -> L.TransitionConfig (L.LatestKnownEra c)
  -> CheckpointsMap (CardanoBlock c)
  -> CardanoProtocolParams c
pattern CardanoProtocolParams {
        paramsByron
      , paramsShelleyBased
      , paramsShelley
      , paramsAllegra
      , paramsMary
      , paramsAlonzo
      , paramsBabbage
      , paramsConway
      , hardForkTriggers
      , ledgerTransitionConfig
      , checkpoints
      } =
    ProtocolParamsCardano {
        cardanoProtocolParamsPerEra = PerEraProtocolParams
          (  paramsByron
          :* paramsShelley
          :* paramsAllegra
          :* paramsMary
          :* paramsAlonzo
          :* paramsBabbage
          :* paramsConway
          :* Nil
          )
      , shelleyBasedProtocolParams = paramsShelleyBased
      , cardanoHardForkTriggers = hardForkTriggers
      , cardanoLedgerTransitionConfig = ledgerTransitionConfig
      , cardanoCheckpoints = checkpoints
      }

{-# COMPLETE CardanoProtocolParams #-}

-- | Create a 'ProtocolInfo' for 'CardanoBlock'
--
-- NOTE: For testing and benchmarking purposes, the 'ShelleyGenesis' can contain
-- initial staking and funds. These are registered in the initial ledger state
-- /only if/ the given 'CardanoHardForkTriggers' tell us to skip the Byron era
-- and hard fork directly to Shelley or a later era by using
-- @TestXHardForkAtEpoch 0@. When @'SL.gNetworkId' == 'SL.Mainnet'@, the initial
-- staking and funds /must/ be empty.
--
-- PRECONDITION: only a single set of Shelley credentials is allowed when used
-- for mainnet (check against @'SL.gNetworkId' == 'SL.Mainnet'@).
protocolInfoCardano ::
     forall c m. (IOLike m, CardanoHardForkConstraints c)
  => CardanoProtocolParams c
  -> ( ProtocolInfo      (CardanoBlock c)
     , m [BlockForging m (CardanoBlock c)]
     )
protocolInfoCardano paramsCardano
  | SL.Mainnet <- SL.sgNetworkId genesisShelley
  , length credssShelleyBased > 1
  = error "Multiple Shelley-based credentials not allowed for mainnet"
  | otherwise
  = assertWithMsg (validateGenesis genesisShelley)
    ( ProtocolInfo {
        pInfoConfig       = cfg
      , pInfoInitLedger   = initExtLedgerStateCardano
      }
    , blockForging
    )
  where
    CardanoProtocolParams {
        paramsByron
      , paramsShelleyBased
      , paramsShelley
      , paramsAllegra
      , paramsMary
      , paramsAlonzo
      , paramsBabbage
      , paramsConway
      , hardForkTriggers = CardanoHardForkTriggers' {
          triggerHardForkShelley
        , triggerHardForkAllegra
        , triggerHardForkMary
        , triggerHardForkAlonzo
        , triggerHardForkBabbage
        , triggerHardForkConway
        }
      , ledgerTransitionConfig
      , checkpoints
      } = paramsCardano

    genesisShelley = ledgerTransitionConfig ^. L.tcShelleyGenesisL

    ProtocolParamsByron {
          byronGenesis                = genesisByron
        , byronLeaderCredentials      = mCredsByron
        , byronMaxTxCapacityOverrides = maxTxCapacityOverridesByron
        } = paramsByron
    ProtocolParamsShelleyBased {
          shelleyBasedInitialNonce      = initialNonceShelley
        , shelleyBasedLeaderCredentials = credssShelleyBased
        } = paramsShelleyBased
    ProtocolParamsShelley {
          shelleyProtVer                = protVerShelley
        , shelleyMaxTxCapacityOverrides = maxTxCapacityOverridesShelley
        } = paramsShelley
    ProtocolParamsAllegra {
          allegraProtVer                = protVerAllegra
        , allegraMaxTxCapacityOverrides = maxTxCapacityOverridesAllegra
        } = paramsAllegra
    ProtocolParamsMary {
          maryProtVer                = protVerMary
        , maryMaxTxCapacityOverrides = maxTxCapacityOverridesMary
        } = paramsMary
    ProtocolParamsAlonzo {
          alonzoProtVer                = protVerAlonzo
        , alonzoMaxTxCapacityOverrides = maxTxCapacityOverridesAlonzo
        } = paramsAlonzo
    ProtocolParamsBabbage {
          babbageProtVer                = protVerBabbage
        , babbageMaxTxCapacityOverrides = maxTxCapacityOverridesBabbage
        } = paramsBabbage
    ProtocolParamsConway {
          conwayProtVer                = protVerConway
        , conwayMaxTxCapacityOverrides = maxTxCapacityOverridesConway
        } = paramsConway

    transitionConfigShelley = transitionConfigAllegra ^. L.tcPreviousEraConfigL
    transitionConfigAllegra = transitionConfigMary    ^. L.tcPreviousEraConfigL
    transitionConfigMary    = transitionConfigAlonzo  ^. L.tcPreviousEraConfigL
    transitionConfigAlonzo  = transitionConfigBabbage ^. L.tcPreviousEraConfigL
    transitionConfigBabbage = transitionConfigConway  ^. L.tcPreviousEraConfigL
    transitionConfigConway  = ledgerTransitionConfig

    -- The major protocol version of the last era is the maximum major protocol
    -- version we support.
    --
    -- TODO: use index of CardanoProtocolParams NP
    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer =
          MaxMajorProtVer
        $ pvMajor
        $ nonEmptyLast
        $ exactlyWeakenNonEmpty
        $ protVers
      where
        protVers :: Exactly (CardanoShelleyEras StandardCrypto) ProtVer
        protVers = Exactly $
          -- ensure that these have the same order as 'CardanoShelleyEras'!
          K protVerShelley :*
          K protVerAllegra :*
          K protVerMary :*
          K protVerAlonzo :*
          K protVerBabbage :*
          K protVerConway :*
          Nil

    -- Byron

    ProtocolInfo {
        pInfoConfig = topLevelConfigByron@TopLevelConfig {
            topLevelConfigProtocol = consensusConfigByron
          , topLevelConfigLedger   = ledgerConfigByron
          , topLevelConfigBlock    = blockConfigByron
          }
      , pInfoInitLedger = initExtLedgerStateByron
      } = protocolInfoByron paramsByron

    partialConsensusConfigByron :: PartialConsensusConfig (BlockProtocol ByronBlock)
    partialConsensusConfigByron = consensusConfigByron

    partialLedgerConfigByron :: PartialLedgerConfig ByronBlock
    partialLedgerConfigByron = ByronPartialLedgerConfig {
          byronLedgerConfig    = ledgerConfigByron
        , byronTriggerHardFork = triggerHardForkShelley
        }

    kByron :: SecurityParam
    kByron = Byron.genesisSecurityParam genesisByron

    -- Shelley

    tpraosParams :: TPraosParams
    tpraosParams =
        Shelley.mkTPraosParams
          maxMajorProtVer
          initialNonceShelley
          genesisShelley

    TPraosParams { tpraosSlotsPerKESPeriod, tpraosMaxKESEvo } = tpraosParams

    praosParams :: PraosParams
    praosParams = PraosParams
      { praosSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesisShelley,
        praosLeaderF = SL.mkActiveSlotCoeff $ SL.sgActiveSlotsCoeff genesisShelley,
        praosSecurityParam = SecurityParam $ SL.sgSecurityParam genesisShelley,
        praosMaxKESEvo = SL.sgMaxKESEvolutions genesisShelley,
        praosQuorum = SL.sgUpdateQuorum genesisShelley,
        praosMaxMajorPV = maxMajorProtVer,
        praosMaxLovelaceSupply = SL.sgMaxLovelaceSupply genesisShelley,
        praosNetworkId = SL.sgNetworkId genesisShelley,
        praosSystemStart = SystemStart $ SL.sgSystemStart genesisShelley,
        praosRandomnessStabilisationWindow =
          -- This value is used for all Praos eras /except/ Babbage, see
          -- 'partialConsensusConfigBabbage'.
          SL.computeRandomnessStabilisationWindow
            (SL.sgSecurityParam genesisShelley)
            (SL.mkActiveSlotCoeff $ SL.sgActiveSlotsCoeff genesisShelley)
      }

    PraosParams { praosSlotsPerKESPeriod, praosMaxKESEvo } = praosParams

    blockConfigShelley :: BlockConfig (ShelleyBlock (TPraos c) (ShelleyEra c))
    blockConfigShelley =
        Shelley.mkShelleyBlockConfig
          protVerShelley
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigShelley ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) (ShelleyEra c)))
    partialConsensusConfigShelley = tpraosParams

    partialLedgerConfigShelley :: PartialLedgerConfig (ShelleyBlock (TPraos c) (ShelleyEra c))
    partialLedgerConfigShelley =
        mkPartialLedgerConfigShelley
          transitionConfigShelley
          maxMajorProtVer
          triggerHardForkAllegra

    kShelley :: SecurityParam
    kShelley = SecurityParam $ sgSecurityParam genesisShelley

    -- Allegra

    blockConfigAllegra :: BlockConfig (ShelleyBlock (TPraos c) (AllegraEra c))
    blockConfigAllegra =
        Shelley.mkShelleyBlockConfig
          protVerAllegra
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigAllegra ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) (AllegraEra c)))
    partialConsensusConfigAllegra = tpraosParams

    partialLedgerConfigAllegra :: PartialLedgerConfig (ShelleyBlock (TPraos c) (AllegraEra c))
    partialLedgerConfigAllegra =
        mkPartialLedgerConfigShelley
          transitionConfigAllegra
          maxMajorProtVer
          triggerHardForkMary

    -- Mary

    blockConfigMary :: BlockConfig (ShelleyBlock (TPraos c) (MaryEra c))
    blockConfigMary =
        Shelley.mkShelleyBlockConfig
          protVerMary
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigMary ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) (MaryEra c)))
    partialConsensusConfigMary = tpraosParams

    partialLedgerConfigMary :: PartialLedgerConfig (ShelleyBlock (TPraos c) (MaryEra c))
    partialLedgerConfigMary =
        mkPartialLedgerConfigShelley
          transitionConfigMary
          maxMajorProtVer
          triggerHardForkAlonzo

    -- Alonzo

    blockConfigAlonzo :: BlockConfig (ShelleyBlock (TPraos c) (AlonzoEra c))
    blockConfigAlonzo =
        Shelley.mkShelleyBlockConfig
          protVerAlonzo
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigAlonzo ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) (AlonzoEra c)))
    partialConsensusConfigAlonzo = tpraosParams

    partialLedgerConfigAlonzo :: PartialLedgerConfig (ShelleyBlock (TPraos c) (AlonzoEra c))
    partialLedgerConfigAlonzo =
        mkPartialLedgerConfigShelley
          transitionConfigAlonzo
          maxMajorProtVer
          triggerHardForkBabbage

    -- Babbage

    blockConfigBabbage :: BlockConfig (ShelleyBlock (Praos c) (BabbageEra c))
    blockConfigBabbage =
        Shelley.mkShelleyBlockConfig
          protVerBabbage
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigBabbage ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (Praos c) (BabbageEra c)))
    partialConsensusConfigBabbage = praosParams {
          -- For Praos in Babbage (just as in all TPraos eras) we use the
          -- smaller (3k/f vs 4k/f slots) stability window here for
          -- backwards-compatibility. See erratum 17.3 in the Shelley ledger
          -- specs for context.
          praosRandomnessStabilisationWindow =
            SL.computeStabilityWindow
              (SL.sgSecurityParam genesisShelley)
              (SL.mkActiveSlotCoeff $ SL.sgActiveSlotsCoeff genesisShelley)
        }


    partialLedgerConfigBabbage :: PartialLedgerConfig (ShelleyBlock (Praos c) (BabbageEra c))
    partialLedgerConfigBabbage =
        mkPartialLedgerConfigShelley
          transitionConfigBabbage
          maxMajorProtVer
          triggerHardForkConway

    -- Conway

    blockConfigConway :: BlockConfig (ShelleyBlock (Praos c) (ConwayEra c))
    blockConfigConway =
        Shelley.mkShelleyBlockConfig
          protVerConway
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigConway ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (Praos c) (ConwayEra c)))
    partialConsensusConfigConway = praosParams

    partialLedgerConfigConway :: PartialLedgerConfig (ShelleyBlock (Praos c) (ConwayEra c))
    partialLedgerConfigConway =
        mkPartialLedgerConfigShelley
          transitionConfigConway
          maxMajorProtVer
          TriggerHardForkNotDuringThisExecution

    -- Cardano

    k :: SecurityParam
    k = assert (kByron == kShelley) kByron

    shape :: History.Shape (CardanoEras c)
    shape = History.Shape $ Exactly $
           K (Byron.byronEraParams     genesisByron)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* Nil

    cfg :: TopLevelConfig (CardanoBlock c)
    cfg = TopLevelConfig {
        topLevelConfigProtocol = HardForkConsensusConfig {
            hardForkConsensusConfigK      = k
          , hardForkConsensusConfigShape  = shape
          , hardForkConsensusConfigPerEra = PerEraConsensusConfig
              (  WrapPartialConsensusConfig partialConsensusConfigByron
              :* WrapPartialConsensusConfig partialConsensusConfigShelley
              :* WrapPartialConsensusConfig partialConsensusConfigAllegra
              :* WrapPartialConsensusConfig partialConsensusConfigMary
              :* WrapPartialConsensusConfig partialConsensusConfigAlonzo
              :* WrapPartialConsensusConfig partialConsensusConfigBabbage
              :* WrapPartialConsensusConfig partialConsensusConfigConway
              :* Nil
              )
          }
      , topLevelConfigLedger = HardForkLedgerConfig {
            hardForkLedgerConfigShape  = shape
          , hardForkLedgerConfigPerEra = PerEraLedgerConfig
              (  WrapPartialLedgerConfig partialLedgerConfigByron
              :* WrapPartialLedgerConfig partialLedgerConfigShelley
              :* WrapPartialLedgerConfig partialLedgerConfigAllegra
              :* WrapPartialLedgerConfig partialLedgerConfigMary
              :* WrapPartialLedgerConfig partialLedgerConfigAlonzo
              :* WrapPartialLedgerConfig partialLedgerConfigBabbage
              :* WrapPartialLedgerConfig partialLedgerConfigConway
              :* Nil
              )
          }
      , topLevelConfigBlock =
          CardanoBlockConfig
            blockConfigByron
            blockConfigShelley
            blockConfigAllegra
            blockConfigMary
            blockConfigAlonzo
            blockConfigBabbage
            blockConfigConway
      , topLevelConfigCodec =
          CardanoCodecConfig
            (configCodec topLevelConfigByron)
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
      , topLevelConfigStorage =
          CardanoStorageConfig
            (configStorage topLevelConfigByron)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
      , topLevelConfigCheckpoints = checkpoints
      }

    -- When the initial ledger state is not in the Byron era, register the
    -- initial staking and initial funds (if provided in the genesis config) in
    -- the ledger state.
    --
    -- NOTE: we hope this code might change in the future when the
    -- Ledger layer provides an era-generic registration function,
    -- which will abstract away the data to be registered for a given
    -- era.
    initExtLedgerStateCardano :: ExtLedgerState (CardanoBlock c) ValuesMK
    initExtLedgerStateCardano = ExtLedgerState {
          headerState = initHeaderState
        , ledgerState = overShelleyBasedLedgerState initLedgerState
        }
      where
        overShelleyBasedLedgerState (HardForkLedgerState st) =
          HardForkLedgerState $ hap (fn id :* registerAny) st

        initHeaderState :: HeaderState (CardanoBlock c)
        initLedgerState :: LedgerState (CardanoBlock c) ValuesMK
        ExtLedgerState initLedgerState initHeaderState =
            injectInitialExtLedgerState cfg
          $ initExtLedgerStateByron

        registerAny :: NP (Flip LedgerState ValuesMK -.-> Flip LedgerState ValuesMK) (CardanoShelleyEras c)
        registerAny =
            hcmap (Proxy @IsShelleyBlock) injectIntoTestState $
                WrapTransitionConfig transitionConfigShelley
             :* WrapTransitionConfig transitionConfigAllegra
             :* WrapTransitionConfig transitionConfigMary
             :* WrapTransitionConfig transitionConfigAlonzo
             :* WrapTransitionConfig transitionConfigBabbage
             :* WrapTransitionConfig transitionConfigConway
             :* Nil

        injectIntoTestState ::
             ShelleyBasedEra era
          => WrapTransitionConfig (ShelleyBlock proto era)
          -> (Flip LedgerState ValuesMK -.-> Flip LedgerState ValuesMK) (ShelleyBlock proto era)
        injectIntoTestState (WrapTransitionConfig cfg) = fn $ \(Flip st) ->
          Flip $ unstowLedgerTables $ forgetLedgerTables $ st {
            Shelley.shelleyLedgerState = L.injectIntoTestState cfg
              (Shelley.shelleyLedgerState $ stowLedgerTables st)
          }

    -- | For each element in the list, a block forging thread will be started.
    --
    -- When no credentials are passed, there will be no threads.
    --
    -- Typically, there will only be a single set of credentials for Shelley.
    --
    -- In case there are multiple credentials for Shelley, which is only done
    -- for testing/benchmarking purposes, we'll have a separate thread for each
    -- of them.
    --
    -- If Byron credentials are passed, we merge them with the Shelley
    -- credentials if possible, so that we only have a single thread running in
    -- the case we have Byron credentials and a single set of Shelley
    -- credentials. If there are multiple Shelley credentials, we merge the
    -- Byron credentials with the first Shelley one but still have separate
    -- threads for the remaining Shelley ones.
    blockForging :: m [BlockForging m (CardanoBlock c)]
    blockForging = do
        shelleyBased <- traverse blockForgingShelleyBased credssShelleyBased
        let blockForgings :: [NonEmptyOptNP (BlockForging m) (CardanoEras c)]
            blockForgings = case (mBlockForgingByron, shelleyBased) of
              (Nothing,    shelleys)         -> shelleys
              (Just byron, [])               -> [byron]
              (Just byron, shelley:shelleys) ->
                  OptNP.zipWith merge byron shelley : shelleys
                where
                  -- When merging Byron with Shelley-based eras, we should never
                  -- merge two from the same era.
                  merge (These1 _ _) = error "forgings of the same era"
                  merge (This1 x)    = x
                  merge (That1 y)    = y

        return $ hardForkBlockForging "Cardano" <$> blockForgings

    mBlockForgingByron :: Maybe (NonEmptyOptNP (BlockForging m) (CardanoEras c))
    mBlockForgingByron = do
        creds <- mCredsByron
        return $ byronBlockForging maxTxCapacityOverridesByron creds `OptNP.at` IZ

    blockForgingShelleyBased ::
         ShelleyLeaderCredentials c
      -> m (NonEmptyOptNP (BlockForging m) (CardanoEras c))
    blockForgingShelleyBased credentials = do
        let ShelleyLeaderCredentials
              { shelleyLeaderCredentialsInitSignKey = initSignKey
              , shelleyLeaderCredentialsCanBeLeader = canBeLeader
              } = credentials

        hotKey <- do
          let maxKESEvo :: Word64
              maxKESEvo = assert (tpraosMaxKESEvo == praosMaxKESEvo) praosMaxKESEvo

              startPeriod :: Absolute.KESPeriod
              startPeriod = Absolute.ocertKESPeriod $ praosCanBeLeaderOpCert canBeLeader

          HotKey.mkHotKey @m @c initSignKey startPeriod maxKESEvo

        let slotToPeriod :: SlotNo -> Absolute.KESPeriod
            slotToPeriod (SlotNo slot) = assert (tpraosSlotsPerKESPeriod == praosSlotsPerKESPeriod) $
              Absolute.KESPeriod $ fromIntegral $ slot `div` praosSlotsPerKESPeriod

        let tpraos :: forall era.
                 ShelleyEraWithCrypto c (TPraos c) era
              => Mempool.TxOverrides (ShelleyBlock (TPraos c) era)
              -> BlockForging m      (ShelleyBlock (TPraos c) era)
            tpraos maxTxCapacityOverrides =
              TPraos.shelleySharedBlockForging hotKey slotToPeriod credentials maxTxCapacityOverrides

        let praos :: forall era.
                 ShelleyEraWithCrypto c (Praos c) era
              => Mempool.TxOverrides (ShelleyBlock (Praos c) era)
              -> BlockForging m      (ShelleyBlock (Praos c) era)
            praos maxTxCapacityOverrides =
              Praos.praosSharedBlockForging hotKey slotToPeriod credentials maxTxCapacityOverrides

        pure
          $ OptSkip    -- Byron
          $ OptNP.fromNonEmptyNP $
            tpraos maxTxCapacityOverridesShelley :*
            tpraos maxTxCapacityOverridesAllegra :*
            tpraos maxTxCapacityOverridesMary    :*
            tpraos maxTxCapacityOverridesAlonzo  :*
            praos  maxTxCapacityOverridesBabbage :*
            praos  maxTxCapacityOverridesConway  :*
            Nil

protocolClientInfoCardano ::
     forall c.
     -- Byron
     EpochSlots
  -> ProtocolClientInfo (CardanoBlock c)
protocolClientInfoCardano epochSlots = ProtocolClientInfo {
      pClientInfoCodecConfig =
        CardanoCodecConfig
          (pClientInfoCodecConfig (protocolClientInfoByron epochSlots))
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
    }

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

mkPartialLedgerConfigShelley ::
     L.EraTransition era
  => L.TransitionConfig era
  -> MaxMajorProtVer
  -> TriggerHardFork
  -> PartialLedgerConfig (ShelleyBlock proto era)
mkPartialLedgerConfigShelley transitionConfig maxMajorProtVer shelleyTriggerHardFork =
    ShelleyPartialLedgerConfig {
          shelleyLedgerConfig =
            Shelley.mkShelleyLedgerConfig
              (transitionConfig ^. L.tcShelleyGenesisL)
              (transitionConfig ^. L.tcTranslationContextL)
              -- 'completeLedgerConfig' will replace the 'History.dummyEpochInfo'
              -- in the partial ledger config with the correct one.
              History.dummyEpochInfo
              maxMajorProtVer
        , shelleyTriggerHardFork = shelleyTriggerHardFork
        }

-- | We need this wrapper to partially apply a 'TransitionConfig' in an NP.
newtype WrapTransitionConfig blk =
    WrapTransitionConfig (L.TransitionConfig (ShelleyBlockLedgerEra blk))
