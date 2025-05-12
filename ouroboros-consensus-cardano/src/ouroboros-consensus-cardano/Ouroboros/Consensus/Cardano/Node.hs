{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.Node (
    CardanoHardForkConstraints
  , CardanoHardForkTrigger (..)
  , CardanoHardForkTriggers (.., CardanoHardForkTriggers', triggerHardForkShelley, triggerHardForkAllegra, triggerHardForkMary, triggerHardForkAlonzo, triggerHardForkBabbage, triggerHardForkConway)
  , CardanoProtocolParams (..)
  , MaxMajorProtVer (..)
  , TriggerHardFork (..)
  , protocolClientInfoCardano
  , protocolInfoCardano
    -- * SupportedNetworkProtocolVersion
  , pattern CardanoNodeToClientVersion12
  , pattern CardanoNodeToClientVersion13
  , pattern CardanoNodeToClientVersion14
  , pattern CardanoNodeToClientVersion15
  , pattern CardanoNodeToClientVersion16
  , pattern CardanoNodeToClientVersion17
  , pattern CardanoNodeToNodeVersion1
  , pattern CardanoNodeToNodeVersion2
  , pattern CardanoNodeToNodeVersion3
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
import           Data.SOP.Index
import           Data.SOP.OptNP (NonEmptyOptNP, OptNP (OptSkip))
import qualified Data.SOP.OptNP as OptNP
import           Data.SOP.Strict
import           Data.Word (Word16, Word64)
import           Lens.Micro ((^.))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.ByronHFC
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Conversions as Byron
import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Byron.Node
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.QueryHF ()
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
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
  HasBlessedGenTxIdEra instances
-------------------------------------------------------------------------------}

instance HasBlessedGenTxIdEra (CardanoShelleyEras c) where
  blessedGenTxIdEra = Z mempty

instance HasBlessedGenTxIdEra (CardanoEras c) where
  blessedGenTxIdEra = S blessedGenTxIdEra

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

pattern CardanoNodeToNodeVersion3 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion3 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion2
      (  WrapNodeToNodeVersion ByronNodeToNodeVersion2
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- and Conway eras enabled, using 'ShelleyNodeToClientVersion8' for the
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

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- and Conway eras enabled, using 'ShelleyNodeToClientVersion9' for the
-- Shelley-based eras.
pattern CardanoNodeToClientVersion13 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion13 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion3
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion9
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion9
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion9
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion9
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion9
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion9
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- and Conway eras enabled, using 'ShelleyNodeToClientVersion10' for the
-- Shelley-based eras.
pattern CardanoNodeToClientVersion14 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion14 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion3
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion10
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion10
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion10
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion10
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion10
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion10
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- and Conway eras enabled, using 'ShelleyNodeToClientVersion11' for the
-- Shelley-based eras.
pattern CardanoNodeToClientVersion15 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion15 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion3
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion11
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion11
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion11
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion11
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion11
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion11
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- and Conway eras enabled, using 'ShelleyNodeToClientVersion12' for the
-- Shelley-based eras.
pattern CardanoNodeToClientVersion16 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion16 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion3
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* Nil
      )

-- | The hard fork enabled, using 'HardForkSpecificNodeToClientVersion4', and the Shelley, Allegra, Mary, Alonzo and Babbage
-- and Conway eras enabled, using 'ShelleyNodeToClientVersion12' for the
-- Shelley-based eras.
pattern CardanoNodeToClientVersion17 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion17 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion4
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion12
      :* Nil
      )

instance CardanoHardForkConstraints c
      => SupportedNetworkProtocolVersion (CardanoBlock c) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (NodeToNodeV_13, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_14, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_15, CardanoNodeToNodeVersion3)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (NodeToClientV_16, CardanoNodeToClientVersion12)
      , (NodeToClientV_17, CardanoNodeToClientVersion13)
      , (NodeToClientV_18, CardanoNodeToClientVersion14)
      , (NodeToClientV_19, CardanoNodeToClientVersion15)
      , (NodeToClientV_20, CardanoNodeToClientVersion16)
      , (NodeToClientV_21, CardanoNodeToClientVersion17)
      ]

  latestReleasedNodeVersion _prx = (Just NodeToNodeV_15, Just NodeToClientV_20)

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | When to trigger a hard fork to a Cardano era.
data CardanoHardForkTrigger blk =
    -- | Trigger the hard fork when the ledger protocol version is updated to
    -- the default for that era (@'L.eraProtVerLow' \@('ShelleyBlockLedgerEra'
    -- blk)@). Also see 'TriggerHardForkAtVersion'.
    CardanoTriggerHardForkAtDefaultVersion
  |
    -- | Trigger the hard fork at the given epoch. For testing only. Also see
    -- 'TriggerHardForkAtEpoch'.
    CardanoTriggerHardForkAtEpoch EpochNo
  deriving stock (Show)

toTriggerHardFork ::
     forall blk. L.Era (ShelleyBlockLedgerEra blk)
  => CardanoHardForkTrigger blk
  -> TriggerHardFork
toTriggerHardFork = \case
    CardanoTriggerHardForkAtDefaultVersion ->
      TriggerHardForkAtVersion $
        SL.getVersion (L.eraProtVerLow @(ShelleyBlockLedgerEra blk))
    CardanoTriggerHardForkAtEpoch epochNo ->
      TriggerHardForkAtEpoch epochNo

newtype CardanoHardForkTriggers = CardanoHardForkTriggers {
    getCardanoHardForkTriggers ::
         NP CardanoHardForkTrigger (CardanoShelleyEras StandardCrypto)
  }

pattern CardanoHardForkTriggers' ::
     (c ~ StandardCrypto)
  => CardanoHardForkTrigger (ShelleyBlock (TPraos c) ShelleyEra)
  -> CardanoHardForkTrigger (ShelleyBlock (TPraos c) AllegraEra)
  -> CardanoHardForkTrigger (ShelleyBlock (TPraos c) MaryEra)
  -> CardanoHardForkTrigger (ShelleyBlock (TPraos c) AlonzoEra)
  -> CardanoHardForkTrigger (ShelleyBlock (Praos  c) BabbageEra)
  -> CardanoHardForkTrigger (ShelleyBlock (Praos  c) ConwayEra)
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
      (  triggerHardForkShelley
      :* triggerHardForkAllegra
      :* triggerHardForkMary
      :* triggerHardForkAlonzo
      :* triggerHardForkBabbage
      :* triggerHardForkConway
      :* Nil
      )
{-# COMPLETE CardanoHardForkTriggers' #-}

-- | Parameters needed to run Cardano.
--
-- __On the relation between 'cardanoHardForkTriggers' and 'cardanoProtocolVersion'__:
--
-- The 'cardanoHardForkTriggers' can mention __ledger__ protocol
-- version versions at which the hard fork will occur. In principle
-- there is no relation between the versions mentioned in
-- 'cardanoProtocolVerson' (if any) and 'cardanoHardForkTriggers',
-- however their relationship might indicate experimental eras or
-- intra-era hard forks. For instance if the last era in the
-- 'CardanoHardForkTriggers' is set to @9 0@, ie:
--
-- > ... :* TriggerHardForkAtVersion (ProtVer (SL.natVersion @9) 0)
--
-- Setting 'cardanoProtocolVersion' to @ProtVer (SL.natVersion @8) 0@
-- will mark that last era as experimental because the obsolete node
-- checks determine that the highest version we support is @8 0@.
--
-- If, on the other hand, we would set 'cardanoProtocolVersion' to
-- @ProtVer (SL.natVersion @10) 0@, this indicates that the node is
-- ready to perform an intra-era hardfork (from version @9@ to version
-- @10@).
--
data CardanoProtocolParams c = CardanoProtocolParams {
    byronProtocolParams           :: ProtocolParamsByron
  , shelleyBasedProtocolParams    :: ProtocolParamsShelleyBased c
  , cardanoHardForkTriggers       :: CardanoHardForkTriggers
  , cardanoLedgerTransitionConfig :: L.TransitionConfig L.LatestKnownEra
  , cardanoCheckpoints            :: CheckpointsMap (CardanoBlock c)
    -- | The greatest protocol version that this node's software and config
    -- files declare to handle correctly.
    --
    -- This parameter has two consequences. First, the blocks minted
    -- will include the protocol version in their header, but
    -- essentially only for public signaling (eg measuring the
    -- percentage of adoption of software updates).
    --
    -- Second, and more importantly, it's passed to the protocol logic. In
    -- particular, the node's envelope check will begin rejecting all blocks
    -- (actually, their headers) if the chain moves to a greater protocol
    -- version. This should never happen in a node that is using up-to-date
    -- software and config files. Note that the missing software update is
    -- not necessarily a 'HardForkBlock' era transition: it might be an
    -- /intra-era hard fork/ (ie conditionals in the ledger rules).
    --
  , cardanoProtocolVersion        :: ProtVer
  }

-- | Create a 'ProtocolInfo' for 'CardanoBlock'
--
-- NOTE: For testing and benchmarking purposes, the genesis config can contain
-- certain data to be registered in the initial ledger state, like initial
-- staking and funds. These are registered /only if/ the given
-- 'CardanoHardForkTriggers' tell us to skip the Byron era and hard fork
-- directly to Shelley or a later era by using @TestXHardForkAtEpoch 0@. When
-- @'SL.gNetworkId' == 'SL.Mainnet'@, no such data must be present.
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
        byronProtocolParams
      , shelleyBasedProtocolParams
      , cardanoHardForkTriggers = CardanoHardForkTriggers' {
          triggerHardForkShelley
        , triggerHardForkAllegra
        , triggerHardForkMary
        , triggerHardForkAlonzo
        , triggerHardForkBabbage
        , triggerHardForkConway
        }
      , cardanoLedgerTransitionConfig
      , cardanoCheckpoints
      , cardanoProtocolVersion
      } = paramsCardano

    genesisShelley = cardanoLedgerTransitionConfig ^. L.tcShelleyGenesisL

    ProtocolParamsByron {
          byronGenesis           = genesisByron
        , byronLeaderCredentials = mCredsByron
        } = byronProtocolParams
    ProtocolParamsShelleyBased {
          shelleyBasedInitialNonce      = initialNonceShelley
        , shelleyBasedLeaderCredentials = credssShelleyBased
        } = shelleyBasedProtocolParams

    transitionConfigShelley = transitionConfigAllegra ^. L.tcPreviousEraConfigL
    transitionConfigAllegra = transitionConfigMary    ^. L.tcPreviousEraConfigL
    transitionConfigMary    = transitionConfigAlonzo  ^. L.tcPreviousEraConfigL
    transitionConfigAlonzo  = transitionConfigBabbage ^. L.tcPreviousEraConfigL
    transitionConfigBabbage = transitionConfigConway  ^. L.tcPreviousEraConfigL
    transitionConfigConway  = cardanoLedgerTransitionConfig

    -- The major protocol version of the last era is the maximum major protocol
    -- version we support.
    --
    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer $ pvMajor cardanoProtocolVersion

    -- Byron

    ProtocolInfo {
        pInfoConfig = topLevelConfigByron@TopLevelConfig {
            topLevelConfigProtocol = consensusConfigByron
          , topLevelConfigLedger   = ledgerConfigByron
          , topLevelConfigBlock    = blockConfigByron
          }
      , pInfoInitLedger = initExtLedgerStateByron
      } = protocolInfoByron byronProtocolParams

    partialConsensusConfigByron :: PartialConsensusConfig (BlockProtocol ByronBlock)
    partialConsensusConfigByron = consensusConfigByron

    partialLedgerConfigByron :: PartialLedgerConfig ByronBlock
    partialLedgerConfigByron = ByronPartialLedgerConfig {
          byronLedgerConfig    = ledgerConfigByron
        , byronTriggerHardFork = toTriggerHardFork triggerHardForkShelley
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
        praosMaxMajorPV = maxMajorProtVer,
        praosRandomnessStabilisationWindow =
          -- This value is used for all Praos eras /except/ Babbage, see
          -- 'partialConsensusConfigBabbage'.
          SL.computeRandomnessStabilisationWindow
            (SL.unNonZero $ SL.sgSecurityParam genesisShelley)
            (SL.mkActiveSlotCoeff $ SL.sgActiveSlotsCoeff genesisShelley)
      }

    PraosParams { praosSlotsPerKESPeriod, praosMaxKESEvo } = praosParams

    blockConfigShelley :: BlockConfig (ShelleyBlock (TPraos c) ShelleyEra)
    blockConfigShelley =
        Shelley.mkShelleyBlockConfig
          cardanoProtocolVersion
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigShelley ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) ShelleyEra))
    partialConsensusConfigShelley = tpraosParams

    partialLedgerConfigShelley :: PartialLedgerConfig (ShelleyBlock (TPraos c) ShelleyEra)
    partialLedgerConfigShelley =
        mkPartialLedgerConfigShelley
          transitionConfigShelley
          (toTriggerHardFork triggerHardForkAllegra)

    kShelley :: SecurityParam
    kShelley = SecurityParam $ sgSecurityParam genesisShelley

    -- Allegra

    blockConfigAllegra :: BlockConfig (ShelleyBlock (TPraos c) AllegraEra)
    blockConfigAllegra =
        Shelley.mkShelleyBlockConfig
          cardanoProtocolVersion
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigAllegra ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) AllegraEra))
    partialConsensusConfigAllegra = tpraosParams

    partialLedgerConfigAllegra :: PartialLedgerConfig (ShelleyBlock (TPraos c) AllegraEra)
    partialLedgerConfigAllegra =
        mkPartialLedgerConfigShelley
          transitionConfigAllegra
          (toTriggerHardFork triggerHardForkMary)

    -- Mary

    blockConfigMary :: BlockConfig (ShelleyBlock (TPraos c) MaryEra)
    blockConfigMary =
        Shelley.mkShelleyBlockConfig
          cardanoProtocolVersion
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigMary ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) MaryEra))
    partialConsensusConfigMary = tpraosParams

    partialLedgerConfigMary :: PartialLedgerConfig (ShelleyBlock (TPraos c) MaryEra)
    partialLedgerConfigMary =
        mkPartialLedgerConfigShelley
          transitionConfigMary
          (toTriggerHardFork triggerHardForkAlonzo)

    -- Alonzo

    blockConfigAlonzo :: BlockConfig (ShelleyBlock (TPraos c) AlonzoEra)
    blockConfigAlonzo =
        Shelley.mkShelleyBlockConfig
          cardanoProtocolVersion
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigAlonzo ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) AlonzoEra))
    partialConsensusConfigAlonzo = tpraosParams

    partialLedgerConfigAlonzo :: PartialLedgerConfig (ShelleyBlock (TPraos c) AlonzoEra)
    partialLedgerConfigAlonzo =
        mkPartialLedgerConfigShelley
          transitionConfigAlonzo
          (toTriggerHardFork triggerHardForkBabbage)

    -- Babbage

    blockConfigBabbage :: BlockConfig (ShelleyBlock (Praos c) BabbageEra)
    blockConfigBabbage =
        Shelley.mkShelleyBlockConfig
          cardanoProtocolVersion
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigBabbage ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (Praos c) BabbageEra))
    partialConsensusConfigBabbage = praosParams {
          -- For Praos in Babbage (just as in all TPraos eras) we use the
          -- smaller (3k/f vs 4k/f slots) stability window here for
          -- backwards-compatibility. See erratum 17.3 in the Shelley ledger
          -- specs for context.
          praosRandomnessStabilisationWindow =
            SL.computeStabilityWindow
              (SL.unNonZero $ SL.sgSecurityParam genesisShelley)
              (SL.mkActiveSlotCoeff $ SL.sgActiveSlotsCoeff genesisShelley)
        }


    partialLedgerConfigBabbage :: PartialLedgerConfig (ShelleyBlock (Praos c) BabbageEra)
    partialLedgerConfigBabbage =
        mkPartialLedgerConfigShelley
          transitionConfigBabbage
          (toTriggerHardFork triggerHardForkConway)

    -- Conway

    blockConfigConway :: BlockConfig (ShelleyBlock (Praos c) ConwayEra)
    blockConfigConway =
        Shelley.mkShelleyBlockConfig
          cardanoProtocolVersion
          genesisShelley
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigConway ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (Praos c) ConwayEra))
    partialConsensusConfigConway = praosParams

    partialLedgerConfigConway :: PartialLedgerConfig (ShelleyBlock (Praos c) ConwayEra)
    partialLedgerConfigConway =
        mkPartialLedgerConfigShelley
          transitionConfigConway
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
      , topLevelConfigCheckpoints = cardanoCheckpoints
      }

    -- When the initial ledger state is not in the Byron era, register various
    -- data from the genesis config (if provided) in the ledger state. For
    -- example, this includes initial staking and initial funds (useful for
    -- testing/benchmarking).
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
        injectIntoTestState (WrapTransitionConfig tcfg) = fn $ \(Flip st) ->
          -- We need to unstow the injected values
          Flip $ unstowLedgerTables $ forgetLedgerTables $ st {
            Shelley.shelleyLedgerState = L.injectIntoTestState tcfg
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
        return $ byronBlockForging creds `OptNP.at` IZ

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
                 ShelleyEraWithCrypto c       (TPraos c) era
              => BlockForging m (ShelleyBlock (TPraos c) era)
            tpraos =
              TPraos.shelleySharedBlockForging hotKey slotToPeriod credentials

        let praos :: forall era.
                 ShelleyEraWithCrypto c       (Praos c) era
              => BlockForging m (ShelleyBlock (Praos c) era)
            praos =
              Praos.praosSharedBlockForging hotKey slotToPeriod credentials

        pure
          $ OptSkip    -- Byron
          $ OptNP.fromNonEmptyNP $
            tpraos :*
            tpraos :*
            tpraos :*
            tpraos :*
            praos  :*
            praos  :*
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
  -> TriggerHardFork
  -> PartialLedgerConfig (ShelleyBlock proto era)
mkPartialLedgerConfigShelley transitionConfig shelleyTriggerHardFork =
    ShelleyPartialLedgerConfig {
          shelleyLedgerConfig =
            Shelley.mkShelleyLedgerConfig
              (transitionConfig ^. L.tcShelleyGenesisL)
              (transitionConfig ^. L.tcTranslationContextL)
              -- 'completeLedgerConfig' will replace the 'History.dummyEpochInfo'
              -- in the partial ledger config with the correct one.
              History.dummyEpochInfo
        , shelleyTriggerHardFork = shelleyTriggerHardFork
        }

-- | We need this wrapper to partially apply a 'TransitionConfig' in an NP.
newtype WrapTransitionConfig blk =
    WrapTransitionConfig (L.TransitionConfig (ShelleyBlockLedgerEra blk))
