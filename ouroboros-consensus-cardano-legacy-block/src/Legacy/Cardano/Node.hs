{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Legacy.Cardano.Node (
    protocolClientInfoLegacyCardano
  , protocolInfoLegacyCardano
  ) where

import           Cardano.Chain.Slotting (EpochSlots)
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.Coerce
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.SOP.Classes
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Strict hiding (Shape (..))
import           Legacy.Byron.Node ()
import           Legacy.Cardano.Block
import           Legacy.Cardano.CanHardFork
import           Legacy.Cardano.Ledger ()
import           Legacy.Shelley.Node ()
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Trans
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Legacy.Block
import           Ouroboros.Consensus.Legacy.Util
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion,
                     SupportedNetworkProtocolVersion (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..),
                     ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..))
import           Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol (..))
import           Ouroboros.Consensus.Shelley.Node.Praos (ProtocolParamsBabbage,
                     ProtocolParamsConway)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance EncodeDisk blk (HardForkChainDepState xs)
      => EncodeDisk (LegacyBlock blk) (HardForkChainDepState xs) where
  encodeDisk ::
       CodecConfig (LegacyBlock blk)
    -> HardForkChainDepState xs
    -> Encoding
  encodeDisk ccfg = coerce $
      encodeDisk @blk @(HardForkChainDepState xs) (coerce ccfg)

instance DecodeDisk blk (HardForkChainDepState xs)
      => DecodeDisk (LegacyBlock blk) (HardForkChainDepState xs) where
  decodeDisk ::
       CodecConfig (LegacyBlock blk)
    -> forall s. Decoder s (HardForkChainDepState xs)
  decodeDisk ccfg = coerce $
      decodeDisk @blk @(HardForkChainDepState xs) (coerce ccfg)

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClient blk (HardForkApplyTxErr xs)
      => SerialiseNodeToClient (LegacyBlock blk) (HardForkApplyTxErr xs) where
  encodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> HardForkApplyTxErr xs -> Encoding
  encodeNodeToClient ccfg version = coerce $
      encodeNodeToClient
        @blk
        @(HardForkApplyTxErr xs)
        (coerce ccfg)
        version

  decodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> forall s. Decoder s (HardForkApplyTxErr xs)
  decodeNodeToClient ccfg version = coerce $
      decodeNodeToClient
        @blk
        @(HardForkApplyTxErr xs)
        (coerce ccfg)
        version

{-------------------------------------------------------------------------------
  SerialiseHFC
-------------------------------------------------------------------------------}

instance LegacyCardanoHardForkConstraints c
      => SerialiseHFC (LegacyCardanoEras c) where

  encodeDiskHfcBlock ::
       CodecConfig (HardForkBlock (LegacyCardanoEras c))
    -> HardForkBlock (LegacyCardanoEras c)
    -> Encoding
  encodeDiskHfcBlock ccfg b =
      encodeDiskHfcBlock
        @(CardanoEras c)
        (hcoerce_CodecConfig ccfg)
        (hcoerce_HardForkBlock b)

  decodeDiskHfcBlock ::
       CodecConfig (HardForkBlock (LegacyCardanoEras c))
    -> forall s. Decoder s (Lazy.ByteString -> HardForkBlock (LegacyCardanoEras c))
  decodeDiskHfcBlock ccfg =
          (hcoerce_HardForkBlock .)
      <$> decodeDiskHfcBlock
            @(CardanoEras c)
            (hcoerce_CodecConfig ccfg)

  reconstructHfcPrefixLen ::
       proxy (Header (HardForkBlock (LegacyCardanoEras c)))
    -> PrefixLen
  reconstructHfcPrefixLen _ =
      reconstructHfcPrefixLen
        (Proxy @(Header (HardForkBlock (CardanoEras c))))

  reconstructHfcNestedCtxt ::
       proxy (Header (HardForkBlock (LegacyCardanoEras c)))
    -> ShortByteString
    -> SizeInBytes
    -> SomeSecond (NestedCtxt Header) (HardForkBlock (LegacyCardanoEras c))
  reconstructHfcNestedCtxt _ sbs sib =
        conv
      $ reconstructHfcNestedCtxt
          (Proxy @(Header (HardForkBlock (CardanoEras c))))
          sbs
          sib
    where
      conv ::
           SomeSecond (NestedCtxt Header) (HardForkBlock (CardanoEras c))
        -> SomeSecond (NestedCtxt Header) (HardForkBlock (LegacyCardanoEras c))
      conv (SomeSecond ctxt) = SomeSecond $
          castNestedCtxt hcoerce_NestedCtxt_ ctxt

  getHfcBinaryBlockInfo ::
       HardForkBlock (LegacyCardanoEras c)
    -> BinaryBlockInfo
  getHfcBinaryBlockInfo = getHfcBinaryBlockInfo @(CardanoEras c) . hcoerce_HardForkBlock

  estimateHfcBlockSize ::
       Header (HardForkBlock (LegacyCardanoEras c))
    -> SizeInBytes
  estimateHfcBlockSize = estimateHfcBlockSize @(CardanoEras c) . hcoerce_Header

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion
-------------------------------------------------------------------------------}

instance LegacyCardanoHardForkConstraints c
      => SupportedNetworkProtocolVersion (HardForkBlock (LegacyCardanoEras c)) where
  supportedNodeToNodeVersions _ = Map.map conv
      $ supportedNodeToNodeVersions (Proxy @(CardanoBlock c))
    where
      conv ::
           HardForkNodeToNodeVersion (CardanoEras c)
        -> HardForkNodeToNodeVersion (LegacyCardanoEras c)
      conv (HardForkNodeToNodeDisabled x) = HardForkNodeToNodeDisabled x
      conv (HardForkNodeToNodeEnabled x y) =
          HardForkNodeToNodeEnabled x (htrans (Proxy @ToLegacyBlock) conv' y)

      conv' :: EraNodeToNodeVersion blk -> EraNodeToNodeVersion (LegacyBlock blk)
      conv' (EraNodeToNodeEnabled x) = EraNodeToNodeEnabled x
      conv' EraNodeToNodeDisabled    = EraNodeToNodeDisabled

  supportedNodeToClientVersions _ = Map.map conv
      $ supportedNodeToClientVersions (Proxy @(CardanoBlock c))
    where
      conv ::
           HardForkNodeToClientVersion (CardanoEras c)
        -> HardForkNodeToClientVersion (LegacyCardanoEras c)
      conv (HardForkNodeToClientDisabled x) = HardForkNodeToClientDisabled x
      conv (HardForkNodeToClientEnabled x y) =
          HardForkNodeToClientEnabled x (htrans (Proxy @ToLegacyBlock) conv' y)

      conv' :: EraNodeToClientVersion blk -> EraNodeToClientVersion (LegacyBlock blk)
      conv' (EraNodeToClientEnabled x) = EraNodeToClientEnabled x
      conv' EraNodeToClientDisabled    = EraNodeToClientDisabled

  latestReleasedNodeVersion _ =
      latestReleasedNodeVersion (Proxy @(CardanoBlock c))

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Only used in 'protocolInfoLegacyCardano'.
class (ToLegacyBlock x y, CanStowLedgerTables (LedgerState x)) => C x y
instance CanStowLedgerTables (LedgerState x) => C x (LegacyBlock x)

protocolInfoLegacyCardano ::
     forall c m. (IOLike m, LegacyCardanoHardForkConstraints c)
  => ProtocolParamsByron
  -> ProtocolParamsShelleyBased (ShelleyEra c)
  -> ProtocolParamsShelley c
  -> ProtocolParamsAllegra c
  -> ProtocolParamsMary    c
  -> ProtocolParamsAlonzo  c
  -> ProtocolParamsBabbage c
  -> ProtocolParamsConway  c
  -> ProtocolTransitionParamsShelleyBased (ShelleyEra c)
  -> ProtocolTransitionParamsShelleyBased (AllegraEra c)
  -> ProtocolTransitionParamsShelleyBased (MaryEra c)
  -> ProtocolTransitionParamsShelleyBased (AlonzoEra c)
  -> ProtocolTransitionParamsShelleyBased (BabbageEra c)
  -> ProtocolTransitionParamsShelleyBased (ConwayEra c)
  -> ( ProtocolInfo      (LegacyCardanoBlock c)
     , m [BlockForging m (LegacyCardanoBlock c)]
     )
protocolInfoLegacyCardano p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 =
    ( ProtocolInfo {
          pInfoConfig = castTopLevelConfig (pInfoConfig pinfo')
        , pInfoInitLedger =
            castExtLedgerState (coerce . stowLedgerTables) (pInfoInitLedger pinfo')
        }
    , fmap (fmap convBlockForging) forging
    )
  where
    pinfo :: ProtocolInfo (CardanoBlock c)
    forging :: m [BlockForging m (CardanoBlock c)]
    (pinfo, forging) =
        protocolInfoCardano @c @m p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14

    pinfo' :: ProtocolInfo (HardForkBlock (LegacyCardanoEras c))
    pinfo' =
        htrans_ProtocolInfo
          (Proxy @C)
          coerce
          coerce
          coerce
          coerce
          coerce
          coerce
          (Flip . coerce . stowLedgerTables . unFlip)
          coerce
          coerce
          pinfo

    convBlockForging ::
         BlockForging m (CardanoBlock c)
      -> BlockForging m (LegacyCardanoBlock c)
    convBlockForging bf = BlockForging {
          forgeLabel
        , canBeLeader = hcoerce_CanBeLeader canBeLeader
        , updateForgeState = updateForgeState'
        , checkCanForge = checkCanForge'
        , forgeBlock = forgeBlock'
        }
      where
        BlockForging {
            forgeLabel
          , canBeLeader
          , updateForgeState
          , checkCanForge
          , forgeBlock
          } = bf

        updateForgeState' ::
             TopLevelConfig (LegacyCardanoBlock c)
          -> SlotNo
          -> Ticked (ChainDepState (BlockProtocol (LegacyCardanoBlock c)))
          -> m (ForgeStateUpdateInfo (LegacyCardanoBlock c))
        updateForgeState' tlcfg sl tcdst = castForgeStateUpdateInfo
                                        . hcoerce_ForgeStateUpdateInfo <$>
            updateForgeState
              (hcoerce_TopLevelConfig $ castTopLevelConfig tlcfg)
              sl
              (hcoerce_TickedChainDepState tcdst)

        checkCanForge' ::
             TopLevelConfig (LegacyCardanoBlock c)
          -> SlotNo
          -> Ticked (ChainDepState (BlockProtocol (LegacyCardanoBlock c)))
          -> IsLeader (BlockProtocol (LegacyCardanoBlock c))
          -> ForgeStateInfo (LegacyCardanoBlock c)
          -> Either (CannotForge (LegacyCardanoBlock c)) ()
        checkCanForge' tlcfg sl tcdst il fsi = first hcoerce_CannotForge $
            checkCanForge
              (hcoerce_TopLevelConfig $ castTopLevelConfig tlcfg)
              sl
              (hcoerce_TickedChainDepState tcdst)
              (hcoerce_IsLeader il)
              (hcoerce_ForgeStateInfo fsi)


        forgeBlock' ::
             TopLevelConfig (LegacyCardanoBlock c)
          -> BlockNo
          -> SlotNo
          -> TickedLedgerState (LegacyCardanoBlock c) EmptyMK
          -> [Validated (GenTx (LegacyCardanoBlock c))]
          -> IsLeader (BlockProtocol (LegacyCardanoBlock c))
          -> m (LegacyCardanoBlock c)
        forgeBlock' tlcfg bl sl tlst vgts il = LegacyBlock
                                             . hcoerce_HardForkBlock <$>
            forgeBlock
              (hcoerce_TopLevelConfig $ castTopLevelConfig tlcfg)
              bl
              sl
              ( hcoerce_TickedLedgerState
              . getTickedLegacyLedgerState
              $ tlst
              )
              (fmap (hcoerce_ValidatedGenTx . getLegacyValidatedGenTx) vgts)
              (hcoerce_IsLeader il)

protocolClientInfoLegacyCardano ::
     forall c.
     EpochSlots
  -> ProtocolClientInfo (LegacyBlock (HardForkBlock (LegacyCardanoEras c)))
protocolClientInfoLegacyCardano epochslots = ProtocolClientInfo {
      pClientInfoCodecConfig = LegacyCodecConfig $ pClientInfoCodecConfig pcinfo'
    }
  where
    pcinfo :: ProtocolClientInfo (CardanoBlock c)
    pcinfo = protocolClientInfoCardano epochslots

    pcinfo' :: ProtocolClientInfo (HardForkBlock (LegacyCardanoEras c))
    pcinfo' = hcoerce_ProtocolClientInfo pcinfo
