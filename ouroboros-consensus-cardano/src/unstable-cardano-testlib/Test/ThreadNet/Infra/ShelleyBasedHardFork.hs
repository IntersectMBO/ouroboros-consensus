{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test infrastructure to test hard-forking from one Shelley-based era to
-- another, e.g., Shelley to Allegra.
module Test.ThreadNet.Infra.ShelleyBasedHardFork
  ( -- * Blocks
    ShelleyBasedHardForkBlock
  , ShelleyBasedHardForkEras

    -- * Transactions
  , pattern GenTxShelley1
  , pattern GenTxShelley2

    -- * Node
  , ShelleyBasedHardForkConstraints
  , protocolInfoShelleyBasedHardFork
  ) where

import qualified Cardano.Ledger.Api.Transition as L
import Cardano.Ledger.Binary.Decoding
  ( decShareCBOR
  , decodeMap
  , decodeMemPack
  , internsFromMap
  )
import Cardano.Ledger.Binary.Encoding
  ( encodeMap
  , encodeMemPack
  , toPlainEncoding
  )
import qualified Cardano.Ledger.Conway.State as SL
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Control.Monad.Except (runExcept)
import qualified Control.Tracer as Tracer
import Data.Coerce
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Functors (Flip (..))
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Index (Index (..), hcimap)
import Data.SOP.Strict
import qualified Data.SOP.Tails as Tails
import qualified Data.SOP.Telescope as Telescope
import Data.Void (Void)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Forging (MkBlockForging)
import Ouroboros.Consensus.Cardano.CanHardFork
  ( crossEraForecastAcrossShelley
  , translateChainDepStateAcrossShelley
  )
import Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..))
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Embed.Binary
import Ouroboros.Consensus.HardFork.Combinator.Serialisation
import Ouroboros.Consensus.HardFork.Combinator.State.Types as HFC
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Node
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Protocol.Praos.AgentClient
  ( KESAgentClientTrace
  , KESAgentContext
  )
import Ouroboros.Consensus.Protocol.TPraos
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Node
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (eitherToMaybe)
import Ouroboros.Consensus.Util.IndexedMemPack
import Test.ThreadNet.TxGen
import Test.ThreadNet.TxGen.Shelley ()

{-------------------------------------------------------------------------------
  Block type
-------------------------------------------------------------------------------}

-- | Two eras, both Shelley-based.
type ShelleyBasedHardForkEras proto1 era1 proto2 era2 =
  '[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2]

type ShelleyBasedHardForkBlock proto1 era1 proto2 era2 =
  HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)

{-------------------------------------------------------------------------------
  Pattern synonyms, for encapsulation and legibility
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkGenTx proto1 era1 proto2 era2 =
  GenTx (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)

pattern GenTxShelley1 ::
  GenTx (ShelleyBlock proto1 era1) ->
  ShelleyBasedHardForkGenTx proto1 era1 proto2 era2
pattern GenTxShelley1 tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley2 ::
  GenTx (ShelleyBlock proto2 era2) ->
  ShelleyBasedHardForkGenTx proto1 era1 proto2 era2
pattern GenTxShelley2 tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxShelley1, GenTxShelley2 #-}

pattern ShelleyBasedHardForkNodeToNodeVersionMax ::
  BlockNodeToNodeVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
pattern ShelleyBasedHardForkNodeToNodeVersionMax =
  HardForkNodeToNodeEnabled
    HardForkSpecificNodeToNodeVersionMax
    ( WrapNodeToNodeVersion ShelleyNodeToNodeVersionMax
        :* WrapNodeToNodeVersion ShelleyNodeToNodeVersionMax
        :* Nil
      )

pattern HardForkSpecificNodeToNodeVersionMax :: HardForkSpecificNodeToNodeVersion
pattern HardForkSpecificNodeToNodeVersionMax <- ((== maxBound) -> True)
  where
    HardForkSpecificNodeToNodeVersionMax = maxBound

pattern ShelleyNodeToNodeVersionMax :: ShelleyNodeToNodeVersion
pattern ShelleyNodeToNodeVersionMax <- ((== maxBound) -> True)
  where
    ShelleyNodeToNodeVersionMax = maxBound

pattern ShelleyBasedHardForkNodeToClientVersionMax ::
  BlockNodeToClientVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
pattern ShelleyBasedHardForkNodeToClientVersionMax =
  HardForkNodeToClientEnabled
    HardForkSpecificNodeToClientVersionMax
    ( EraNodeToClientEnabled ShelleyNodeToClientVersionMax
        :* EraNodeToClientEnabled ShelleyNodeToClientVersionMax
        :* Nil
      )

pattern HardForkSpecificNodeToClientVersionMax :: HardForkSpecificNodeToClientVersion
pattern HardForkSpecificNodeToClientVersionMax <- ((== maxBound) -> True)
  where
    HardForkSpecificNodeToClientVersionMax = maxBound

pattern ShelleyNodeToClientVersionMax :: ShelleyNodeToClientVersion
pattern ShelleyNodeToClientVersionMax <- ((== maxBound) -> True)
  where
    ShelleyNodeToClientVersionMax = maxBound

{-------------------------------------------------------------------------------
  Consensus instances
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =
  ( ShelleyCompatible proto1 era1
  , ShelleyCompatible proto2 era2
  , LedgerSupportsProtocol (ShelleyBlock proto1 era1)
  , LedgerSupportsProtocol (ShelleyBlock proto2 era2)
  , TxLimits (ShelleyBlock proto1 era1)
  , TxLimits (ShelleyBlock proto2 era2)
  , TranslateTxMeasure (TxMeasure (ShelleyBlock proto1 era1)) (TxMeasure (ShelleyBlock proto2 era2))
  , SL.PreviousEra era2 ~ era1
  , SL.TranslateEra era2 SL.NewEpochState
  , SL.TranslateEra era2 SL.Tx
  , SL.TranslationError era2 SL.NewEpochState ~ Void
  , -- At the moment, fix the protocols together
    ProtoCrypto proto1 ~ ProtoCrypto proto2
  , PraosCrypto (ProtoCrypto proto1)
  , proto1 ~ TPraos (ProtoCrypto proto1)
  , proto1 ~ proto2
  , MemPack (TxOut (LedgerState (ShelleyBlock proto1 era1)))
  , MemPack (TxOut (LedgerState (ShelleyBlock proto2 era2)))
  )

class TranslateTxMeasure a b where
  translateTxMeasure :: a -> b

instance TranslateTxMeasure (IgnoringOverflow ByteSize32) (IgnoringOverflow ByteSize32) where
  translateTxMeasure = id

instance TranslateTxMeasure (IgnoringOverflow ByteSize32) AlonzoMeasure where
  translateTxMeasure x = AlonzoMeasure x mempty

instance TranslateTxMeasure (IgnoringOverflow ByteSize32) ConwayMeasure where
  translateTxMeasure =
    translateTxMeasure . (\x -> x :: AlonzoMeasure) . translateTxMeasure

instance TranslateTxMeasure AlonzoMeasure AlonzoMeasure where
  translateTxMeasure = id

instance TranslateTxMeasure AlonzoMeasure ConwayMeasure where
  translateTxMeasure x = ConwayMeasure x mempty

instance TranslateTxMeasure ConwayMeasure ConwayMeasure where
  translateTxMeasure = id

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  SerialiseHFC (ShelleyBasedHardForkEras proto1 era1 proto2 era2)

-- use defaults

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  CanHardFork (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  where
  type
    HardForkTxMeasure (ShelleyBasedHardForkEras proto1 era1 proto2 era2) =
      TxMeasure (ShelleyBlock proto2 era2)

  hardForkEraTranslation =
    EraTranslation
      { translateLedgerState = PCons translateLedgerState PNil
      , translateLedgerTables = PCons translateLedgerTables PNil
      , translateChainDepState = PCons translateChainDepStateAcrossShelley PNil
      , crossEraForecast = PCons crossEraForecastAcrossShelley PNil
      }
   where
    translateLedgerState ::
      InPairs.RequiringBoth
        WrapLedgerConfig
        TranslateLedgerState
        (ShelleyBlock proto1 era1)
        (ShelleyBlock proto2 era2)
    translateLedgerState =
      InPairs.RequireBoth $
        \_cfg1 cfg2 ->
          HFC.TranslateLedgerState
            { translateLedgerStateWith = \_epochNo ->
                noNewTickingDiffs
                  . unFlip
                  . unComp
                  . SL.translateEra'
                    (shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2))
                  . Comp
                  . Flip
            }

    translateLedgerTables ::
      TranslateLedgerTables
        (ShelleyBlock proto1 era1)
        (ShelleyBlock proto2 era2)
    translateLedgerTables =
      HFC.TranslateLedgerTables
        { translateTxInWith = coerce
        , translateTxOutWith = SL.upgradeTxOut
        }

  hardForkChainSel = Tails.mk2 SameTiebreakerAcrossEras

  hardForkInjectTxs =
    InPairs.mk2 $
      InPairs.RequireBoth $ \_cfg1 cfg2 ->
        let ctxt = shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2)
         in Pair2
              (InjectTx (translateTx ctxt))
              (InjectValidatedTx (translateValidatedTx ctxt))
   where
    translateTx ::
      SL.TranslationContext era2 ->
      GenTx (ShelleyBlock proto era1) ->
      Maybe (GenTx (ShelleyBlock proto era2))
    translateTx transCtxt =
      fmap unComp
        . eitherToMaybe
        . runExcept
        . SL.translateEra transCtxt
        . Comp

    translateValidatedTx ::
      SL.TranslationContext era2 ->
      WrapValidatedGenTx (ShelleyBlock proto era1) ->
      Maybe (WrapValidatedGenTx (ShelleyBlock proto era2))
    translateValidatedTx transCtxt =
      fmap unComp
        . eitherToMaybe
        . runExcept
        . SL.translateEra transCtxt
        . Comp

  hardForkInjTxMeasure = \case
    (Z (WrapTxMeasure x)) -> translateTxMeasure x
    S (Z (WrapTxMeasure x)) -> x

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  SupportedNetworkProtocolVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
  where
  supportedNodeToNodeVersions _ =
    Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToNodeVersionMax)
      ]

  supportedNodeToClientVersions _ =
    Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToClientVersionMax)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  Query HF
-------------------------------------------------------------------------------}

answerShelleyBasedQueryHF ::
  ( xs ~ '[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2]
  , ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
  ) =>
  ( forall blk.
    IsShelleyBlock blk =>
    Index xs blk ->
    ExtLedgerCfg blk ->
    BlockQuery blk footprint result ->
    ReadOnlyForker' m (HardForkBlock xs) ->
    m result
  ) ->
  Index xs x ->
  ExtLedgerCfg x ->
  BlockQuery x footprint result ->
  ReadOnlyForker' m (HardForkBlock xs) ->
  m result
answerShelleyBasedQueryHF f idx cfgs q forker = case idx of
  IZ -> f idx cfgs q forker
  IS IZ -> f idx cfgs q forker
  IS (IS idx') -> case idx' of {}

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  BlockSupportsHFLedgerQuery '[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2]
  where
  answerBlockQueryHFLookup =
    answerShelleyBasedQueryHF
      ( \idx ->
          answerShelleyLookupQueries
            (injectLedgerTables idx)
            (ejectHardForkTxOutDefault idx)
            (ejectCanonicalTxIn idx)
      )

  answerBlockQueryHFTraverse =
    answerShelleyBasedQueryHF
      ( \idx ->
          answerShelleyTraversingQueries
            (ejectHardForkTxOutDefault idx)
            (ejectCanonicalTxIn idx)
            (queryLedgerGetTraversingFilter @('[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2]) idx)
      )

  queryLedgerGetTraversingFilter IZ q = \case
    Z (WrapTxOut x) -> shelleyQFTraverseTablesPredicate q x
    S (Z (WrapTxOut x)) -> shelleyQFTraverseTablesPredicate q x
  queryLedgerGetTraversingFilter (IS IZ) q = \case
    Z (WrapTxOut x) -> shelleyQFTraverseTablesPredicate q x
    S (Z (WrapTxOut x)) -> shelleyQFTraverseTablesPredicate q x
  queryLedgerGetTraversingFilter (IS (IS idx)) _q = case idx of {}

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoShelleyBasedHardFork ::
  forall m proto1 era1 proto2 era2.
  ( KESAgentContext (ProtoCrypto proto2) m
  , ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
  ) =>
  ProtocolParamsShelleyBased (ProtoCrypto proto1) ->
  SL.ProtVer ->
  SL.ProtVer ->
  L.TransitionConfig era2 ->
  TriggerHardFork ->
  ( ProtocolInfo (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
  , Tracer.Tracer m KESAgentClientTrace ->
    m [MkBlockForging m (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)]
  )
protocolInfoShelleyBasedHardFork
  protocolParamsShelleyBased
  protVer1
  protVer2
  transCfg2
  hardForkTrigger =
    protocolInfoBinary
      -- Era 1
      protocolInfo1
      blockForging1
      eraParams1
      tpraosParams
      toPartialLedgerConfig1
      -- Era 2
      protocolInfo2
      blockForging2
      eraParams2
      tpraosParams
      toPartialLedgerConfig2
   where
    ProtocolParamsShelleyBased
      { shelleyBasedInitialNonce
      , shelleyBasedLeaderCredentials
      } = protocolParamsShelleyBased

    -- Era 1

    genesis :: SL.ShelleyGenesis
    genesis = transCfg2 ^. L.tcShelleyGenesisL

    protocolInfo1 :: ProtocolInfo (ShelleyBlock proto1 era1)
    blockForging1 ::
      Tracer.Tracer m KESAgentClientTrace -> m [MkBlockForging m (ShelleyBlock proto1 era1)]
    (protocolInfo1, blockForging1) =
      protocolInfoTPraosShelleyBased
        protocolParamsShelleyBased
        (transCfg2 ^. L.tcPreviousEraConfigL)
        protVer1

    eraParams1 :: History.EraParams
    eraParams1 = shelleyEraParams genesis

    toPartialLedgerConfig1 ::
      LedgerConfig (ShelleyBlock proto1 era1) ->
      PartialLedgerConfig (ShelleyBlock proto1 era1)
    toPartialLedgerConfig1 cfg =
      ShelleyPartialLedgerConfig
        { shelleyLedgerConfig = cfg
        , shelleyTriggerHardFork = hardForkTrigger
        }

    -- Era 2

    protocolInfo2 :: ProtocolInfo (ShelleyBlock proto2 era2)
    blockForging2 ::
      Tracer.Tracer m KESAgentClientTrace -> m [MkBlockForging m (ShelleyBlock proto2 era2)]
    (protocolInfo2, blockForging2) =
      protocolInfoTPraosShelleyBased
        ProtocolParamsShelleyBased
          { shelleyBasedInitialNonce
          , shelleyBasedLeaderCredentials
          }
        transCfg2
        protVer2

    eraParams2 :: History.EraParams
    eraParams2 = shelleyEraParams genesis

    toPartialLedgerConfig2 ::
      LedgerConfig (ShelleyBlock proto2 era2) ->
      PartialLedgerConfig (ShelleyBlock proto2 era2)
    toPartialLedgerConfig2 cfg =
      ShelleyPartialLedgerConfig
        { shelleyLedgerConfig = cfg
        , shelleyTriggerHardFork = TriggerHardForkNotDuringThisExecution
        }

{-------------------------------------------------------------------------------
  TxGen instance
-------------------------------------------------------------------------------}

-- | Use a generic implementation for 'TxGen'
instance
  ( TxGen (ShelleyBlock proto1 era1)
  , TxGen (ShelleyBlock proto2 era2)
  , ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
  ) =>
  TxGen (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
  where
  type
    TxGenExtra (ShelleyBasedHardForkBlock proto1 era1 proto2 era2) =
      NP WrapTxGenExtra (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  testGenTxs = testGenTxsHfc

{-------------------------------------------------------------------------------
  Canonical TxIn
-------------------------------------------------------------------------------}

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  HasCanonicalTxIn (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  where
  newtype CanonicalTxIn (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
    = ShelleyHFCTxIn
    { getShelleyHFCTxIn :: SL.TxIn
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NoThunks, MemPack)

  injectCanonicalTxIn IZ txIn = ShelleyHFCTxIn txIn
  injectCanonicalTxIn (IS IZ) txIn = ShelleyHFCTxIn (coerce txIn)
  injectCanonicalTxIn (IS (IS idx')) _ = case idx' of {}

  ejectCanonicalTxIn IZ txIn = getShelleyHFCTxIn txIn
  ejectCanonicalTxIn (IS IZ) txIn = coerce (getShelleyHFCTxIn txIn)
  ejectCanonicalTxIn (IS (IS idx')) _ = case idx' of {}

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  HasHardForkTxOut (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  where
  type
    HardForkTxOut (ShelleyBasedHardForkEras proto1 era1 proto2 era2) =
      DefaultHardForkTxOut (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  injectHardForkTxOut = injectHardForkTxOutDefault
  ejectHardForkTxOut = ejectHardForkTxOutDefault

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  SerializeTablesWithHint
    (LedgerState (HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)))
  where
  encodeTablesWithHint ::
    LedgerState (HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)) EmptyMK ->
    LedgerTables
      (LedgerState (HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)))
      ValuesMK ->
    Encoding
  encodeTablesWithHint (HardForkLedgerState (HardForkState idx)) (LedgerTables (ValuesMK tbs)) =
    let
      np =
        (Fn $ const $ K $ encOne (Proxy @era1))
          :* (Fn $ const $ K $ encOne (Proxy @era2))
          :* Nil
     in
      hcollapse $ hap np $ Telescope.tip idx
   where
    encOne :: forall era. SL.Era era => Proxy era -> Encoding
    encOne _ =
      toPlainEncoding (SL.eraProtVerLow @era) $
        encodeMap
          (encodeMemPack . getShelleyHFCTxIn)
          ( \case
              Z txout -> encodeMemPack $ unwrapTxOut txout
              S (Z txout) -> encodeMemPack $ unwrapTxOut txout
          )
          tbs

  decodeTablesWithHint ::
    forall s.
    LedgerState (HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)) EmptyMK ->
    Decoder
      s
      ( LedgerTables
          (LedgerState (HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)))
          ValuesMK
      )
  decodeTablesWithHint (HardForkLedgerState (HardForkState idx)) =
    let
      np =
        (Fn $ Comp . fmap K . getOne (Z . WrapTxOut) . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne (S . Z . WrapTxOut) . unFlip . currentState)
          :* Nil
     in
      hcollapse <$> (hsequence' $ hap np $ Telescope.tip idx)
   where
    getOne ::
      forall proto era.
      ShelleyCompatible proto era =>
      ( TxOut (LedgerState (ShelleyBlock proto era)) ->
        TxOut (LedgerState (HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)))
      ) ->
      LedgerState (ShelleyBlock proto era) EmptyMK ->
      Decoder
        s
        ( LedgerTables
            (LedgerState (HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)))
            ValuesMK
        )
    getOne toShelleyTxOut st =
      let certInterns =
            internsFromMap $
              shelleyLedgerState st
                ^. SL.nesEsL
                  . SL.esLStateL
                  . SL.lsCertStateL
                  . SL.certDStateL
                  . SL.accountsL
                  . SL.accountsMapL
       in LedgerTables . ValuesMK
            <$> SL.eraDecoder @era
              (decodeMap (ShelleyHFCTxIn <$> decodeMemPack) (toShelleyTxOut <$> decShareCBOR certInterns))

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  IndexedMemPack
    (LedgerState (HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)) EmptyMK)
    (DefaultHardForkTxOut (ShelleyBasedHardForkEras proto1 era1 proto2 era2))
  where
  indexedTypeName _ =
    typeName @(DefaultHardForkTxOut (ShelleyBasedHardForkEras proto1 era1 proto2 era2))
  indexedPackedByteCount _ txout =
    hcollapse $
      hcmap
        (Proxy @MemPackTxOut)
        (K . packedByteCount . unwrapTxOut)
        txout
  indexedPackM _ =
    hcollapse
      . hcimap
        (Proxy @MemPackTxOut)
        ( \_ (WrapTxOut txout) -> K $ do
            packM txout
        )
  indexedUnpackM (HardForkLedgerState (HardForkState idx)) = do
    hsequence'
      $ hcmap
        (Proxy @MemPackTxOut)
        (const $ Comp $ WrapTxOut <$> unpackM)
      $ Telescope.tip idx
