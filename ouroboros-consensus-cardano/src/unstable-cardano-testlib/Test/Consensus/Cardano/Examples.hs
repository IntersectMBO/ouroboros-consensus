{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
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

<<<<<<< HEAD:ouroboros-consensus-cardano/src/unstable-cardano-testlib/Test/Consensus/Cardano/Examples.hs
import           Data.Coerce (Coercible)
import           Data.SOP.BasicFunctors
import           Data.SOP.Counting (Exactly (..))
import           Data.SOP.Index (Index (..))
||||||| parent of 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs
import           Data.Coerce (Coercible)
=======
import           Data.Coerce (coerce)
>>>>>>> 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs
import           Data.SOP.Strict
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
<<<<<<< HEAD:ouroboros-consensus-cardano/src/unstable-cardano-testlib/Test/Consensus/Cardano/Examples.hs
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
||||||| parent of 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting (Exactly (..))
import           Ouroboros.Consensus.Util.SOP (Index (..))

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron

=======
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting (Exactly (..))
import           Ouroboros.Consensus.Util.SOP (Index (..), himap)

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron

>>>>>>> 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
<<<<<<< HEAD:ouroboros-consensus-cardano/src/unstable-cardano-testlib/Test/Consensus/Cardano/Examples.hs
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Network.Block (Serialised (..))
||||||| parent of 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork ()

import           Test.Util.Serialisation.Golden (Examples, Labelled, labelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

=======

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ByronPartialLedgerConfig (..),
                     ShelleyPartialLedgerConfig (..), TriggerHardFork (..))

import           Test.Util.Serialisation.Golden (Examples, Labelled, labelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

>>>>>>> 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs
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
mkCardanoExamples ::
     NP Examples (CardanoEras Crypto)
  -> Examples (CardanoBlock Crypto)
mkCardanoExamples perEraExamples = Golden.Examples {
        exampleBlock            = coerce $ viaInject @I                 (coerce Golden.exampleBlock)
      , exampleSerialisedBlock  =          viaInject                            Golden.exampleSerialisedBlock
      , exampleHeader           =          viaInject                            Golden.exampleHeader
      , exampleSerialisedHeader =          viaInject                            Golden.exampleSerialisedHeader
      , exampleHeaderHash       = coerce $ viaInject @WrapHeaderHash    (coerce Golden.exampleHeaderHash)
      , exampleGenTx            =          viaInject                            Golden.exampleGenTx
      , exampleGenTxId          = coerce $ viaInject @WrapGenTxId       (coerce Golden.exampleGenTxId)
      , exampleApplyTxErr       = coerce $ viaInject @WrapApplyTxErr    (coerce Golden.exampleApplyTxErr)
      , exampleQuery            =          viaInject                            Golden.exampleQuery
      , exampleResult           =          viaInject                            Golden.exampleResult
      , exampleAnnTip           =          viaInject                            Golden.exampleAnnTip
      , exampleLedgerState      =          viaInject                            Golden.exampleLedgerState
      , exampleChainDepState    = coerce $ viaInject @WrapChainDepState (coerce Golden.exampleChainDepState)
      , exampleExtLedgerState   =          viaInject                            Golden.exampleExtLedgerState
      , exampleSlotNo           = coerce $ viaInject @(K SlotNo)        (coerce Golden.exampleSlotNo)
      , exampleLedgerConfig     = exampleLedgerConfigCardano
      }
  where
<<<<<<< HEAD:ouroboros-consensus-cardano/src/unstable-cardano-testlib/Test/Consensus/Cardano/Examples.hs
    eraInjections :: NP (Examples -.-> K (Examples (CardanoBlock Crypto)))
                        (CardanoEras Crypto)
    eraInjections =
           fn (K . injExamples "Byron"   IZ)
        :* fn (K . injExamples "Shelley" (IS IZ))
        :* fn (K . injExamples "Allegra" (IS (IS IZ)))
        :* fn (K . injExamples "Mary"    (IS (IS (IS IZ))))
        :* fn (K . injExamples "Alonzo"  (IS (IS (IS (IS IZ)))))
        :* fn (K . injExamples "Babbage" (IS (IS (IS (IS (IS IZ))))))
        :* fn (K . injExamples "Conway"  (IS (IS (IS (IS (IS (IS IZ)))))))
        :* Nil

    injExamples ::
         String
      -> Index (CardanoEras Crypto) blk
      -> Examples blk
      -> Examples (CardanoBlock Crypto)
    injExamples eraName idx =
          prefixExamples eraName
        . inject exampleStartBounds idx
||||||| parent of 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs
    eraInjections :: NP (Examples -.-> K (Examples (CardanoBlock Crypto)))
                        (CardanoEras Crypto)
    eraInjections =
           fn (K . injExamples "Byron"   IZ)
        :* fn (K . injExamples "Shelley" (IS IZ))
        :* fn (K . injExamples "Allegra" (IS (IS IZ)))
        :* fn (K . injExamples "Mary"    (IS (IS (IS IZ))))
        :* fn (K . injExamples "Alonzo"  (IS (IS (IS (IS IZ)))))
        :* Nil

    injExamples ::
         String
      -> Index (CardanoEras Crypto) blk
      -> Examples blk
      -> Examples (CardanoBlock Crypto)
    injExamples eraName idx =
          Golden.prefixExamples eraName
        . inject exampleStartBounds idx
=======
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
        inj idx = fmap (fmap (inject exampleStartBounds idx))

    perEraExamplesPrefixed :: NP Examples (CardanoEras Crypto)
    perEraExamplesPrefixed = hzipWith (\(K eraName) es -> Golden.prefixExamples eraName es) perEraNames perEraExamples
      where
        perEraNames = K "Byron"
                   :* K "Shelley"
                   :* K "Allegra"
                   :* K "Mary"
                   :* K "Alonzo"
                   :* Nil

    exampleLedgerConfigCardano :: Labelled (HardForkLedgerConfig (CardanoEras Crypto))
    exampleLedgerConfigCardano = [
        ( Nothing
        , HardForkLedgerConfig
            cardanoShape
            (PerEraLedgerConfig (
                 WrapPartialLedgerConfig (ByronPartialLedgerConfig   lcByron   (TriggerHardForkAtEpoch shelleyTransitionEpoch))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcShelley (TriggerHardForkAtEpoch (History.boundEpoch allegraStartBound)))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcAllegra (TriggerHardForkAtEpoch (History.boundEpoch maryStartBound)))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcMary    (TriggerHardForkAtEpoch (History.boundEpoch alonzoStartBound)))
              :* WrapPartialLedgerConfig (ShelleyPartialLedgerConfig lcAlonzo  TriggerHardForkNever)
              :* Nil))
        )
      | WrapLedgerConfig lcByron   <- labelledLcByron
      , WrapLedgerConfig lcShelley <- labelledLcShelley
      , WrapLedgerConfig lcAllegra <- labelledLcAllegra
      , WrapLedgerConfig lcMary    <- labelledLcMary
      , WrapLedgerConfig lcAlonzo  <- labelledLcAlonzo
      ]
      where
        (    Comp labelledLcByron
          :* Comp labelledLcShelley
          :* Comp labelledLcAllegra
          :* Comp labelledLcMary
          :* Comp labelledLcAlonzo
          :* Nil
          ) = hmap (Comp . fmap (WrapLedgerConfig . snd) . Golden.exampleLedgerConfig) perEraExamples
>>>>>>> 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs

{-------------------------------------------------------------------------------
  Inject instances
-------------------------------------------------------------------------------}

-- | In reality, an era tag would be prepended, but we're testing that the
-- encoder doesn't care what the bytes are.
instance Inject Serialised where
  inject _ _ (Serialised _) = Serialised "<CARDANO_BLOCK>"

instance Inject SomeResult where
  inject _ idx (SomeResult q r) =
      SomeResult (QueryIfCurrent (injectQuery idx q)) (Right r)

<<<<<<< HEAD:ouroboros-consensus-cardano/src/unstable-cardano-testlib/Test/Consensus/Cardano/Examples.hs
instance Inject Examples where
  inject startBounds (idx :: Index xs x) Examples {..} = Examples {
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
      }
    where
      inj ::
           forall f a b.
           ( Inject f
           , Coercible a (f x)
           , Coercible b (f (HardForkBlock xs))
           )
        => Proxy f -> Labelled a -> Labelled b
      inj p = fmap (fmap (inject' p startBounds idx))

||||||| parent of 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs
instance Inject Examples where
  inject startBounds (idx :: Index xs x) Golden.Examples {..} = Golden.Examples {
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
      }
    where
      inj ::
           forall f a b.
           ( Inject f
           , Coercible a (f x)
           , Coercible b (f (HardForkBlock xs))
           )
        => Proxy f -> Labelled a -> Labelled b
      inj p = fmap (fmap (inject' p startBounds idx))

=======
>>>>>>> 4eff5de98... Add LedgerConfig golden test:ouroboros-consensus-cardano-test/src/Test/Consensus/Cardano/Examples.hs
{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

byronEraParams :: History.EraParams
byronEraParams = Byron.byronEraParams Byron.ledgerConfig

shelleyEraParams :: History.EraParams
shelleyEraParams = Shelley.shelleyEraParams @StandardCrypto Shelley.testShelleyGenesis

allegraEraParams :: History.EraParams
allegraEraParams = Shelley.shelleyEraParams @StandardCrypto Shelley.testShelleyGenesis

maryEraParams :: History.EraParams
maryEraParams = Shelley.shelleyEraParams @StandardCrypto Shelley.testShelleyGenesis

alonzoEraParams :: History.EraParams
alonzoEraParams = Shelley.shelleyEraParams @StandardCrypto Shelley.testShelleyGenesis

babbageEraParams :: History.EraParams
babbageEraParams = Shelley.shelleyEraParams @StandardCrypto Shelley.testShelleyGenesis

conwayEraParams :: History.EraParams
conwayEraParams = Shelley.shelleyEraParams @StandardCrypto Shelley.testShelleyGenesis

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

eraInfoShelley :: SingleEraInfo (ShelleyBlock (TPraos StandardCrypto) StandardShelley)
eraInfoShelley = singleEraInfo (Proxy @(ShelleyBlock (TPraos StandardCrypto) StandardShelley))

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
examples = mkCardanoExamples eraExamples <> multiEraExamples

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
