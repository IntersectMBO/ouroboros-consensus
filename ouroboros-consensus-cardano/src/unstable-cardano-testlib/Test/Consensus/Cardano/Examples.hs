{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Consensus.Cardano.Examples (
    -- * Setup
    codecConfig
    -- * Examples
  , exampleApplyTxErrWrongEraByron
  , exampleApplyTxErrWrongEraShelley
  , exampleEraMismatchByron
  , exampleEraMismatchShelley
  , exampleQueryAnytimeShelley
  , exampleQueryEraMismatchByron
  , exampleQueryEraMismatchShelley
  , exampleResultAnytimeShelley
  , exampleResultEraMismatchByron
  , exampleResultEraMismatchShelley
  , examples
  ) where

import           Data.Coerce (Coercible, coerce)
import           Data.SOP.BasicFunctors
import           Data.SOP.Counting (Exactly (..))
import           Data.SOP.Index (Index (..), himap)
import           Data.SOP.Strict
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.ByronHFC
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Network.Block (Serialised (..))
import qualified Test.Consensus.Byron.Examples as Byron
import qualified Test.Consensus.Shelley.Examples as Shelley
import           Test.Util.Serialisation.Examples (Examples (..), Labelled,
                     labelled, prefixExamples)
import           Test.Util.Serialisation.SomeResult (SomeResult (..))

type Crypto = StandardCrypto

eraExamples :: NP Examples (CardanoEras Crypto)
eraExamples =
       Byron.examples
    :* Shelley.examplesShelley
    :* Shelley.examplesAllegra
    :* Shelley.examplesMary
    :* Shelley.examplesAlonzo
    :* Shelley.examplesBabbage
    :* Shelley.examplesConway
    :* Nil

-- | By using this function, we can't forget to update this test when adding a
-- new era to 'CardanoEras'.
combineEras ::
     NP Examples (CardanoEras Crypto)
  -> Examples (CardanoBlock Crypto)
combineEras perEraExamples = Examples {
        exampleBlock            = coerce $ viaInject @I                 (coerce exampleBlock)
      , exampleSerialisedBlock  =          viaInject                            exampleSerialisedBlock
      , exampleHeader           =          viaInject                            exampleHeader
      , exampleSerialisedHeader =          viaInject                            exampleSerialisedHeader
      , exampleHeaderHash       = coerce $ viaInject @WrapHeaderHash    (coerce exampleHeaderHash)
      , exampleGenTx            =          viaInject                            exampleGenTx
      , exampleGenTxId          = coerce $ viaInject @WrapGenTxId       (coerce exampleGenTxId)
      , exampleApplyTxErr       = coerce $ viaInject @WrapApplyTxErr    (coerce exampleApplyTxErr)
      , exampleQuery            =          viaInject                            exampleQuery
      , exampleResult           =          viaInject                            exampleResult
      , exampleAnnTip           =          viaInject                            exampleAnnTip
      , exampleLedgerState      =          viaInject                            exampleLedgerState
      , exampleChainDepState    = coerce $ viaInject @WrapChainDepState (coerce exampleChainDepState)
      , exampleExtLedgerState   =          viaInject                            exampleExtLedgerState
      , exampleSlotNo           = coerce $ viaInject @(K SlotNo)        (coerce exampleSlotNo)
      , exampleLedgerConfig     = exampleLedgerConfigCardano
      }
  where
    viaInject ::
         forall f. Inject f
      => (forall blk. Examples blk -> Labelled (f blk))
      -> Labelled (f (CardanoBlock Crypto))
    viaInject getExamples =
          mconcat
        $ hcollapse
        $ himap (\ix -> K . inj ix . getExamples) perEraExamplesPrefixed
      where
        inj :: forall blk. Index (CardanoEras Crypto) blk -> Labelled (f blk) -> Labelled (f (CardanoBlock Crypto))
        inj idx = fmap (fmap (inject $ oracularInjectionIndex exampleStartBounds idx))

    perEraExamplesPrefixed :: NP Examples (CardanoEras Crypto)
    perEraExamplesPrefixed = hzipWith (\(K eraName) es -> prefixExamples eraName es) perEraNames perEraExamples
      where
        perEraNames = K "Byron"
                   :* K "Shelley"
                   :* K "Allegra"
                   :* K "Mary"
                   :* K "Alonzo"
                   :* K "Babbage"
                   :* K "Conway"
                   :* Nil

    exampleLedgerConfigCardano ::
         Labelled (HardForkLedgerConfig (CardanoEras Crypto))
    exampleLedgerConfigCardano = [
        ( Nothing
        , HardForkLedgerConfig
            cardanoShape
            (PerEraLedgerConfig (
                 WrapPartialLedgerConfig (ByronPartialLedgerConfig   lcByron   (TriggerHardForkAtEpoch shelleyTransitionEpoch))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcShelley (TriggerHardForkAtEpoch (History.boundEpoch allegraStartBound)))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcAllegra (TriggerHardForkAtEpoch (History.boundEpoch maryStartBound)))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcMary    (TriggerHardForkAtEpoch (History.boundEpoch alonzoStartBound)))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcAlonzo  (TriggerHardForkAtEpoch (History.boundEpoch babbageStartBound)))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcBabbage (TriggerHardForkAtEpoch (History.boundEpoch conwayStartBound)))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcConway  TriggerHardForkNotDuringThisExecution)
              :* Nil))
        )
      | WrapLedgerConfig lcByron   <- labelledLcByron
      , WrapLedgerConfig lcShelley <- labelledLcShelley
      , WrapLedgerConfig lcAllegra <- labelledLcAllegra
      , WrapLedgerConfig lcMary    <- labelledLcMary
      , WrapLedgerConfig lcAlonzo  <- labelledLcAlonzo
      , WrapLedgerConfig lcBabbage <- labelledLcBabbage
      , WrapLedgerConfig lcConway  <- labelledLcConway
      ]
      where
        (    Comp labelledLcByron
          :* Comp labelledLcShelley
          :* Comp labelledLcAllegra
          :* Comp labelledLcMary
          :* Comp labelledLcAlonzo
          :* Comp labelledLcBabbage
          :* Comp labelledLcConway
          :* Nil
          ) = hmap (Comp . fmap (WrapLedgerConfig . snd) . exampleLedgerConfig) perEraExamples

{-------------------------------------------------------------------------------
  Inject instances
-------------------------------------------------------------------------------}

-- | In reality, an era tag would be prepended, but we're testing that the
-- encoder doesn't care what the bytes are.
instance Inject Serialised where
  inject _ (Serialised _) = Serialised "<CARDANO_BLOCK>"

instance Inject SomeResult where
  inject iidx (SomeResult q r) =
      SomeResult
        (QueryIfCurrent (injectQuery (forgetInjectionIndex iidx) q))
        (Right r)

instance Inject Examples where
  inject (iidx :: InjectionIndex xs x) Examples {..} = Examples {
        exampleBlock            = inj (Proxy @I)                       exampleBlock
      , exampleSerialisedBlock  = inj (Proxy @Serialised)              exampleSerialisedBlock
      , exampleHeader           = inj (Proxy @Header)                  exampleHeader
      , exampleSerialisedHeader = inj (Proxy @SerialisedHeader)        exampleSerialisedHeader
      , exampleHeaderHash       = inj (Proxy @WrapHeaderHash)          exampleHeaderHash
      , exampleGenTx            = inj (Proxy @GenTx)                   exampleGenTx
      , exampleGenTxId          = inj (Proxy @WrapGenTxId)             exampleGenTxId
      , exampleApplyTxErr       = inj (Proxy @WrapApplyTxErr)          exampleApplyTxErr
      , exampleQuery            = inj (Proxy @(SomeSecond BlockQuery)) exampleQuery
      , exampleResult           = inj (Proxy @SomeResult)              exampleResult
      , exampleAnnTip           = inj (Proxy @AnnTip)                  exampleAnnTip
      , exampleLedgerState      = inj (Proxy @LedgerState)             exampleLedgerState
      , exampleChainDepState    = inj (Proxy @WrapChainDepState)       exampleChainDepState
      , exampleExtLedgerState   = inj (Proxy @ExtLedgerState)          exampleExtLedgerState
      , exampleSlotNo           =                                      exampleSlotNo
        -- We cannot create a HF Ledger Config out of just one of the eras
      , exampleLedgerConfig     = mempty
      }
    where
      inj ::
           forall f a b.
           ( Inject f
           , Coercible a (f x)
           , Coercible b (f (HardForkBlock xs))
           )
        => Proxy f -> Labelled a -> Labelled b
      inj p = map (fmap (inject' p iidx))



{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

byronEraParams :: History.EraParams
byronEraParams = Byron.byronEraParams Byron.ledgerConfig

shelleyEraParams :: History.EraParams
shelleyEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

allegraEraParams :: History.EraParams
allegraEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

maryEraParams :: History.EraParams
maryEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

alonzoEraParams :: History.EraParams
alonzoEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

babbageEraParams :: History.EraParams
babbageEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

conwayEraParams :: History.EraParams
conwayEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

-- | We use 10, 20, 30, 40, ... as the transition epochs
shelleyTransitionEpoch :: EpochNo
shelleyTransitionEpoch = 10

byronStartBound :: History.Bound
byronStartBound = History.initBound

shelleyStartBound :: History.Bound
shelleyStartBound =
    History.mkUpperBound
      byronEraParams
      byronStartBound
      shelleyTransitionEpoch

allegraStartBound :: History.Bound
allegraStartBound =
    History.mkUpperBound
      shelleyEraParams
      shelleyStartBound
      20

maryStartBound :: History.Bound
maryStartBound =
    History.mkUpperBound
      allegraEraParams
      allegraStartBound
      30

alonzoStartBound :: History.Bound
alonzoStartBound =
    History.mkUpperBound
      maryEraParams
      maryStartBound
      40

babbageStartBound :: History.Bound
babbageStartBound =
    History.mkUpperBound
      alonzoEraParams
      alonzoStartBound
      50

conwayStartBound :: History.Bound
conwayStartBound =
    History.mkUpperBound
      alonzoEraParams
      alonzoStartBound
      60

exampleStartBounds :: Exactly (CardanoEras Crypto) History.Bound
exampleStartBounds = Exactly $
    (  K byronStartBound
    :* K shelleyStartBound
    :* K allegraStartBound
    :* K maryStartBound
    :* K alonzoStartBound
    :* K babbageStartBound
    :* K conwayStartBound
    :* Nil
    )

cardanoShape :: History.Shape (CardanoEras Crypto)
cardanoShape = History.Shape $ Exactly $
    (  K byronEraParams
    :* K shelleyEraParams
    :* K allegraEraParams
    :* K maryEraParams
    :* K alonzoEraParams
    :* K babbageEraParams
    :* K conwayEraParams
    :* Nil
    )

summary :: History.Summary (CardanoEras Crypto)
summary =
    State.reconstructSummary
      cardanoShape
      (State.TransitionKnown shelleyTransitionEpoch)
      (hardForkLedgerStatePerEra (ledgerStateByron byronLedger))
  where
    (_, byronLedger) = head $ exampleLedgerState Byron.examples

eraInfoByron :: SingleEraInfo ByronBlock
eraInfoByron = singleEraInfo (Proxy @ByronBlock)

eraInfoShelley :: SingleEraInfo (ShelleyBlock (TPraos StandardCrypto) ShelleyEra)
eraInfoShelley = singleEraInfo (Proxy @(ShelleyBlock (TPraos StandardCrypto) ShelleyEra))

codecConfig :: CardanoCodecConfig Crypto
codecConfig =
    CardanoCodecConfig
      Byron.codecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig

ledgerStateByron ::
     LedgerState ByronBlock
  -> LedgerState (CardanoBlock Crypto)
ledgerStateByron stByron =
    HardForkLedgerState $ HardForkState $ TZ cur
  where
    cur = State.Current {
          currentStart = History.initBound
        , currentState = stByron
        }

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- | Multi-era examples, e.g., applying a transaction to the wrong era.
multiEraExamples :: Examples (CardanoBlock Crypto)
multiEraExamples = mempty {
      exampleApplyTxErr = labelled [
          ("WrongEraByron",   exampleApplyTxErrWrongEraByron)
        , ("WrongEraShelley", exampleApplyTxErrWrongEraShelley)
        ]
    , exampleQuery = labelled [
          ("AnytimeByron",   exampleQueryAnytimeByron)
        , ("AnytimeShelley", exampleQueryAnytimeShelley)
        , ("HardFork",       exampleQueryHardFork)
        ]
    , exampleResult = labelled [
          ("EraMismatchByron",   exampleResultEraMismatchByron)
        , ("EraMismatchShelley", exampleResultEraMismatchShelley)
        , ("AnytimeByron",       exampleResultAnytimeByron)
        , ("AnytimeShelley",     exampleResultAnytimeShelley)
        , ("HardFork",           exampleResultHardFork)
        ]
    }

-- | The examples: the examples from each individual era lifted in to
-- 'CardanoBlock' /and/ the multi-era examples.
examples :: Examples (CardanoBlock Crypto)
examples = combineEras eraExamples <> multiEraExamples

-- | Applying a Shelley thing to a Byron ledger
exampleEraMismatchByron :: MismatchEraInfo (CardanoEras Crypto)
exampleEraMismatchByron =
    MismatchEraInfo $ MR (Z eraInfoShelley) (LedgerEraInfo eraInfoByron)

-- | Applying a Byron thing to a Shelley ledger
exampleEraMismatchShelley :: MismatchEraInfo (CardanoEras Crypto)
exampleEraMismatchShelley =
    MismatchEraInfo $ ML eraInfoByron (Z (LedgerEraInfo eraInfoShelley))

exampleApplyTxErrWrongEraByron :: ApplyTxErr (CardanoBlock Crypto)
exampleApplyTxErrWrongEraByron =
      HardForkApplyTxErrWrongEra exampleEraMismatchByron

exampleApplyTxErrWrongEraShelley :: ApplyTxErr (CardanoBlock Crypto)
exampleApplyTxErrWrongEraShelley =
      HardForkApplyTxErrWrongEra exampleEraMismatchShelley

exampleQueryEraMismatchByron :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryEraMismatchByron =
    SomeSecond (QueryIfCurrentShelley Shelley.GetLedgerTip)

exampleQueryEraMismatchShelley :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryEraMismatchShelley =
    SomeSecond (QueryIfCurrentByron Byron.GetUpdateInterfaceState)

exampleQueryAnytimeByron :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryAnytimeByron =
    SomeSecond (QueryAnytimeByron GetEraStart)

exampleQueryAnytimeShelley :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryAnytimeShelley =
    SomeSecond (QueryAnytimeShelley GetEraStart)

exampleQueryHardFork :: SomeSecond BlockQuery (CardanoBlock Crypto)
exampleQueryHardFork =
    SomeSecond (QueryHardFork GetInterpreter)

exampleResultEraMismatchByron :: SomeResult (CardanoBlock Crypto)
exampleResultEraMismatchByron =
    SomeResult
      (QueryIfCurrentShelley Shelley.GetLedgerTip)
      (Left exampleEraMismatchByron)

exampleResultEraMismatchShelley :: SomeResult (CardanoBlock Crypto)
exampleResultEraMismatchShelley =
    SomeResult
      (QueryIfCurrentByron Byron.GetUpdateInterfaceState)
      (Left exampleEraMismatchShelley)

exampleResultAnytimeByron :: SomeResult (CardanoBlock Crypto)
exampleResultAnytimeByron =
    SomeResult (QueryAnytimeByron GetEraStart) (Just byronStartBound)

exampleResultAnytimeShelley :: SomeResult (CardanoBlock Crypto)
exampleResultAnytimeShelley =
    SomeResult (QueryAnytimeShelley GetEraStart) (Just shelleyStartBound)

exampleResultHardFork :: SomeResult (CardanoBlock Crypto)
exampleResultHardFork =
    SomeResult (QueryHardFork GetInterpreter) (History.mkInterpreter summary)
