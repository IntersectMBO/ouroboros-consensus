{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'Arbitrary' instances intended for serialisation roundtrip tests for
-- 'CardanoBlock' and its related types.
--
-- Because the generated values are only used in serialisation roundtrip tests,
-- they don't need to be valid blocks, transactions, etc.
--
-- We combine the Byron and Shelley-based instances defined elsewhere into
-- Cardano instances by picking randomly from one of the eras.
module Test.Consensus.Cardano.Generators () where

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Counting (Exactly (..))
import Data.SOP.Index
import Data.SOP.NonEmpty
import Data.SOP.Sing
import Data.SOP.Strict
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.ByronHFC
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Serialisation
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Serialisation (Some (..))
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Block ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.TypeFamilyWrappers
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Consensus.Byron.Generators ()
import Test.Consensus.Protocol.Serialisation.Generators ()
import Test.Consensus.Shelley.Generators
import Test.Consensus.Shelley.MockCrypto (CanMock)
import Test.QuickCheck
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Serialisation.Roundtrip
  ( Coherent (..)
  , WithVersion (..)
  )

{-------------------------------------------------------------------------------
  Disk
-------------------------------------------------------------------------------}

instance Arbitrary (CardanoBlock StandardCrypto) where
  arbitrary =
    oneof $ catMaybes $ hcollapse generators
   where
    generators ::
      NP
        (K (Maybe (Gen (CardanoBlock StandardCrypto))))
        (CardanoEras StandardCrypto)
    generators =
      mk BlockByron
        :* mk BlockShelley
        :* mk BlockAllegra
        :* mk BlockMary
        :* mk BlockAlonzo
        :* mk BlockBabbage
        :* mk BlockConway
        :* mk BlockDijkstra
        :* Nil

    mk ::
      forall a x.
      Arbitrary a =>
      (a -> CardanoBlock StandardCrypto) ->
      K (Maybe (Gen (CardanoBlock StandardCrypto))) x
    mk f = K $ Just $ f <$> arbitrary

instance Arbitrary (Coherent (CardanoBlock StandardCrypto)) where
  arbitrary =
    fmap Coherent $ oneof $ catMaybes $ hcollapse generators
   where
    generators ::
      NP
        (K (Maybe (Gen (CardanoBlock StandardCrypto))))
        (CardanoEras StandardCrypto)
    generators =
      mk BlockByron
        :* mk BlockShelley
        :* mk BlockAllegra
        :* mk BlockMary
        :* mk BlockAlonzo
        :* mk BlockBabbage
        :* mk BlockConway
        :* mk BlockDijkstra
        :* Nil

    mk ::
      forall a x.
      Arbitrary (Coherent a) =>
      (a -> CardanoBlock StandardCrypto) ->
      K (Maybe (Gen (CardanoBlock StandardCrypto))) x
    mk f = K $ Just $ f . getCoherent <$> arbitrary

instance Arbitrary (CardanoHeader StandardCrypto) where
  arbitrary = getHeader <$> arbitrary

instance
  (CanMock (TPraos c) ShelleyEra, CardanoHardForkConstraints c) =>
  Arbitrary (OneEraHash (CardanoEras c))
  where
  arbitrary = inj <$> arbitrary
   where
    inj :: NS WrapHeaderHash (CardanoEras c) -> OneEraHash (CardanoEras c)
    inj = hcollapse . hcmap proxySingle aux

    aux ::
      forall blk.
      SingleEraBlock blk =>
      WrapHeaderHash blk -> K (OneEraHash (CardanoEras c)) blk
    aux = K . OneEraHash . toShortRawHash (Proxy @blk) . unwrapHeaderHash

instance
  (c ~ StandardCrypto, ShelleyBasedEra ShelleyEra) =>
  Arbitrary (AnnTip (CardanoBlock c))
  where
  arbitrary =
    AnnTip
      <$> (SlotNo <$> arbitrary)
      <*> arbitrary
      <*> (OneEraTipInfo <$> arbitrary)

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

instance
  CardanoHardForkConstraints c =>
  Arbitrary (HardForkNodeToNodeVersion (CardanoEras c))
  where
  arbitrary =
    elements $ Map.elems $ supportedNodeToNodeVersions (Proxy @(CardanoBlock c))

deriving instance
  Arbitrary (BlockNodeToNodeVersion blk) =>
  Arbitrary (WrapNodeToNodeVersion blk)

arbitraryNodeToNode ::
  ( Arbitrary (WithVersion ByronNodeToNodeVersion byron)
  , Arbitrary (WithVersion ShelleyNodeToNodeVersion shelley)
  , Arbitrary (WithVersion ShelleyNodeToNodeVersion allegra)
  , Arbitrary (WithVersion ShelleyNodeToNodeVersion mary)
  , Arbitrary (WithVersion ShelleyNodeToNodeVersion alonzo)
  , Arbitrary (WithVersion ShelleyNodeToNodeVersion babbage)
  , Arbitrary (WithVersion ShelleyNodeToNodeVersion conway)
  , Arbitrary (WithVersion ShelleyNodeToNodeVersion dijkstra)
  ) =>
  (byron -> cardano) ->
  (shelley -> cardano) ->
  (allegra -> cardano) ->
  (mary -> cardano) ->
  (alonzo -> cardano) ->
  (babbage -> cardano) ->
  (conway -> cardano) ->
  (dijkstra -> cardano) ->
  Gen (WithVersion (HardForkNodeToNodeVersion (CardanoEras c)) cardano)
arbitraryNodeToNode injByron injShelley injAllegra injMary injAlonzo injBabbage injConway injDijkstra =
  oneof
    [ -- Byron before HFC
      ( \(WithVersion versionByron b) ->
          WithVersion
            (HardForkNodeToNodeDisabled versionByron)
            (injByron b)
      )
        <$> arbitrary
    , -- Note that any value generated by the V1 Byron generator is also fine
      -- when using Byron V2.
      ( \(WithVersion versionByron x) versionShelley versionAllegra versionMary versionAlonzo versionBabbage versionConway versionDijkstra ->
          distrib
            versionByron
            versionShelley
            versionAllegra
            versionMary
            versionAlonzo
            versionBabbage
            versionConway
            versionDijkstra
            (injByron x)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , ( \versionByron (WithVersion versionShelley x) versionAllegra versionMary versionAlonzo versionBabbage versionConway versionDijkstra ->
          distrib
            versionByron
            versionShelley
            versionAllegra
            versionMary
            versionAlonzo
            versionBabbage
            versionConway
            versionDijkstra
            (injShelley x)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , ( \versionByron versionShelley (WithVersion versionAllegra x) versionMary versionAlonzo versionBabbage versionConway versionDijkstra ->
          distrib
            versionByron
            versionShelley
            versionAllegra
            versionMary
            versionAlonzo
            versionBabbage
            versionConway
            versionDijkstra
            (injAllegra x)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , ( \versionByron versionShelley versionAllegra (WithVersion versionMary x) versionAlonzo versionBabbage versionConway versionDijkstra ->
          distrib
            versionByron
            versionShelley
            versionAllegra
            versionMary
            versionAlonzo
            versionBabbage
            versionConway
            versionDijkstra
            (injMary x)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , ( \versionByron versionShelley versionAllegra versionMary (WithVersion versionAlonzo x) versionBabbage versionConway versionDijkstra ->
          distrib
            versionByron
            versionShelley
            versionAllegra
            versionMary
            versionAlonzo
            versionBabbage
            versionConway
            versionDijkstra
            (injAlonzo x)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , ( \versionByron versionShelley versionAllegra versionMary versionAlonzo (WithVersion versionBabbage x) versionConway versionDijkstra ->
          distrib
            versionByron
            versionShelley
            versionAllegra
            versionMary
            versionAlonzo
            versionBabbage
            versionConway
            versionDijkstra
            (injBabbage x)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , ( \versionByron versionShelley versionAllegra versionMary versionAlonzo versionBabbage (WithVersion versionConway x) versionDijkstra ->
          distrib
            versionByron
            versionShelley
            versionAllegra
            versionMary
            versionAlonzo
            versionBabbage
            versionConway
            versionDijkstra
            (injConway x)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , ( \versionByron versionShelley versionAllegra versionMary versionAlonzo versionBabbage versionConway (WithVersion versionDijkstra x) ->
          distrib
            versionByron
            versionShelley
            versionAllegra
            versionMary
            versionAlonzo
            versionBabbage
            versionConway
            versionDijkstra
            (injDijkstra x)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    ]
 where
  distrib
    versionByron
    versionShelley
    versionAllegra
    versionMary
    versionAlonzo
    versionBabbage
    versionConway
    versionDijkstra
    x =
      WithVersion
        ( HardForkNodeToNodeEnabled
            maxBound
            ( WrapNodeToNodeVersion versionByron
                :* WrapNodeToNodeVersion versionShelley
                :* WrapNodeToNodeVersion versionAllegra
                :* WrapNodeToNodeVersion versionMary
                :* WrapNodeToNodeVersion versionAlonzo
                :* WrapNodeToNodeVersion versionBabbage
                :* WrapNodeToNodeVersion versionConway
                :* WrapNodeToNodeVersion versionDijkstra
                :* Nil
            )
        )
        x

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToNodeVersion (CardanoEras c))
        (SomeSecond (NestedCtxt Header) (CardanoBlock c))
    )
  where
  arbitrary =
    arbitraryNodeToNode
      injByron
      injShelley
      injAllegra
      injMary
      injAlonzo
      injBabbage
      injConway
      injDijkstra
   where
    injByron = mapSomeNestedCtxt NCZ
    injShelley = mapSomeNestedCtxt (NCS . NCZ)
    injAllegra = mapSomeNestedCtxt (NCS . NCS . NCZ)
    injMary = mapSomeNestedCtxt (NCS . NCS . NCS . NCZ)
    injAlonzo = mapSomeNestedCtxt (NCS . NCS . NCS . NCS . NCZ)
    injBabbage = mapSomeNestedCtxt (NCS . NCS . NCS . NCS . NCS . NCZ)
    injConway = mapSomeNestedCtxt (NCS . NCS . NCS . NCS . NCS . NCS . NCZ)
    injDijkstra = mapSomeNestedCtxt (NCS . NCS . NCS . NCS . NCS . NCS . NCS . NCZ)

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToNodeVersion (CardanoEras c))
        (CardanoBlock c)
    )
  where
  arbitrary =
    arbitraryNodeToNode
      BlockByron
      BlockShelley
      BlockAllegra
      BlockMary
      BlockAlonzo
      BlockBabbage
      BlockConway
      BlockDijkstra

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToNodeVersion (CardanoEras c))
        (CardanoHeader c)
    )
  where
  arbitrary =
    arbitraryNodeToNode
      HeaderByron
      HeaderShelley
      HeaderAllegra
      HeaderMary
      HeaderAlonzo
      HeaderBabbage
      HeaderConway
      HeaderDijkstra

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToNodeVersion (CardanoEras c))
        (CardanoGenTx c)
    )
  where
  arbitrary =
    arbitraryNodeToNode
      GenTxByron
      GenTxShelley
      GenTxAllegra
      GenTxMary
      GenTxAlonzo
      GenTxBabbage
      GenTxConway
      GenTxDijkstra

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToNodeVersion (CardanoEras c))
        (CardanoGenTxId c)
    )
  where
  arbitrary =
    arbitraryNodeToNode
      GenTxIdByron
      GenTxIdShelley
      GenTxIdAllegra
      GenTxIdMary
      GenTxIdAlonzo
      GenTxIdBabbage
      GenTxIdConway
      GenTxIdDijkstra

{-------------------------------------------------------------------------------
  NodeToClient
-------------------------------------------------------------------------------}

instance
  CardanoHardForkConstraints c =>
  Arbitrary (HardForkNodeToClientVersion (CardanoEras c))
  where
  arbitrary =
    elements $ Map.elems $ supportedNodeToClientVersions (Proxy @(CardanoBlock c))

newtype HardForkEnabledNodeToClientVersion c = HardForkEnabledNodeToClientVersion
  { getHardForkEnabledNodeToClientVersion :: HardForkNodeToClientVersion (CardanoEras c)
  }

deriving newtype instance
  CardanoHardForkConstraints c =>
  Eq (HardForkEnabledNodeToClientVersion c)
deriving newtype instance
  CardanoHardForkConstraints c =>
  Show (HardForkEnabledNodeToClientVersion c)

instance
  CardanoHardForkConstraints c =>
  Arbitrary (HardForkEnabledNodeToClientVersion c)
  where
  arbitrary =
    elements
      . map HardForkEnabledNodeToClientVersion
      . filter isHardForkNodeToClientEnabled
      . Map.elems
      . supportedNodeToClientVersions
      $ Proxy @(CardanoBlock c)

-- | Generate a supported 'HardForkNodeToClientVersion' of which the
-- 'HardForkSpecificNodeToClientVersion' satisfies the given predicate.
--
-- PRECONDITION: 'supportedNodeToClientVersions' must include a version that
-- satisfies this condition.
_genWithHardForkSpecificNodeToClientVersion ::
  forall c.
  CardanoHardForkConstraints c =>
  (HardForkSpecificNodeToClientVersion -> Bool) ->
  Gen (HardForkNodeToClientVersion (CardanoEras c))
_genWithHardForkSpecificNodeToClientVersion p =
  elements
    . filter p'
    . Map.elems
    . supportedNodeToClientVersions
    $ Proxy @(CardanoBlock c)
 where
  p' :: HardForkNodeToClientVersion (CardanoEras c) -> Bool
  p' (HardForkNodeToClientEnabled v _) = p v
  p' (HardForkNodeToClientDisabled{}) = False

instance
  Arbitrary (BlockNodeToClientVersion blk) =>
  Arbitrary (EraNodeToClientVersion blk)
  where
  arbitrary =
    frequency
      [ (1, pure EraNodeToClientDisabled)
      , (9, EraNodeToClientEnabled <$> arbitrary)
      ]

arbitraryNodeToClient ::
  ( Arbitrary (WithVersion ByronNodeToClientVersion byron)
  , Arbitrary (WithVersion ShelleyNodeToClientVersion shelley)
  , Arbitrary (WithVersion ShelleyNodeToClientVersion allegra)
  , Arbitrary (WithVersion ShelleyNodeToClientVersion mary)
  , Arbitrary (WithVersion ShelleyNodeToClientVersion alonzo)
  , Arbitrary (WithVersion ShelleyNodeToClientVersion babbage)
  , Arbitrary (WithVersion ShelleyNodeToClientVersion conway)
  , Arbitrary (WithVersion ShelleyNodeToClientVersion dijkstra)
  ) =>
  (byron -> cardano) ->
  (shelley -> cardano) ->
  (allegra -> cardano) ->
  (mary -> cardano) ->
  (alonzo -> cardano) ->
  (babbage -> cardano) ->
  (conway -> cardano) ->
  (dijkstra -> cardano) ->
  Gen (WithVersion (HardForkNodeToClientVersion (CardanoEras c)) cardano)
arbitraryNodeToClient injByron injShelley injAllegra injMary injAlonzo injBabbage injConway injDijkstra =
  oneof
    -- Byron + HardFork disabled
    [ ( \(WithVersion versionByron b) ->
          WithVersion
            (HardForkNodeToClientDisabled versionByron)
            (injByron b)
      )
        <$> arbitrary
    , -- Byron + HardFork enabled.
      ( \(WithVersion versionByron b) versionShelley versionAllegra versionMary versionAlonzo versionBabbage versionConway versionDijkstra ->
          WithVersion
            ( HardForkNodeToClientEnabled
                maxBound
                ( EraNodeToClientEnabled versionByron
                    :* EraNodeToClientEnabled versionShelley
                    :* versionAllegra
                    :* versionMary
                    :* versionAlonzo
                    :* versionBabbage
                    :* versionConway
                    :* versionDijkstra
                    :* Nil
                )
            )
            (injByron b)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , -- Shelley + HardFork enabled
      ( \versionByron (WithVersion versionShelley s) versionAllegra versionMary versionAlonzo versionBabbage versionConway versionDijkstra ->
          WithVersion
            ( HardForkNodeToClientEnabled
                maxBound
                ( EraNodeToClientEnabled versionByron
                    :* EraNodeToClientEnabled versionShelley
                    :* versionAllegra
                    :* versionMary
                    :* versionAlonzo
                    :* versionBabbage
                    :* versionConway
                    :* versionDijkstra
                    :* Nil
                )
            )
            (injShelley s)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , -- Allegra + HardFork enabled
      ( \versionByron versionShelley (WithVersion versionAllegra a) versionMary versionAlonzo versionBabbage versionConway versionDijkstra ->
          WithVersion
            ( HardForkNodeToClientEnabled
                maxBound
                ( EraNodeToClientEnabled versionByron
                    :* EraNodeToClientEnabled versionShelley
                    :* EraNodeToClientEnabled versionAllegra
                    :* versionMary
                    :* versionAlonzo
                    :* versionBabbage
                    :* versionConway
                    :* versionDijkstra
                    :* Nil
                )
            )
            (injAllegra a)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , -- Mary + HardFork enabled
      ( \versionByron versionShelley versionAllegra (WithVersion versionMary m) versionAlonzo versionBabbage versionConway versionDijkstra ->
          WithVersion
            ( HardForkNodeToClientEnabled
                maxBound
                ( EraNodeToClientEnabled versionByron
                    :* EraNodeToClientEnabled versionShelley
                    :* EraNodeToClientEnabled versionAllegra
                    :* EraNodeToClientEnabled versionMary
                    :* versionAlonzo
                    :* versionBabbage
                    :* versionConway
                    :* versionDijkstra
                    :* Nil
                )
            )
            (injMary m)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , -- Alonzo + HardFork enabled
      ( \versionByron versionShelley versionAllegra versionMary (WithVersion versionAlonzo a) versionBabbage versionConway versionDijkstra ->
          WithVersion
            ( HardForkNodeToClientEnabled
                maxBound
                ( EraNodeToClientEnabled versionByron
                    :* EraNodeToClientEnabled versionShelley
                    :* EraNodeToClientEnabled versionAllegra
                    :* EraNodeToClientEnabled versionMary
                    :* EraNodeToClientEnabled versionAlonzo
                    :* versionBabbage
                    :* versionConway
                    :* versionDijkstra
                    :* Nil
                )
            )
            (injAlonzo a)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , -- Babbage + HardFork enabled
      ( \versionByron versionShelley versionAllegra versionMary versionAlonzo (WithVersion versionBabbage a) versionConway versionDijkstra ->
          WithVersion
            ( HardForkNodeToClientEnabled
                maxBound
                ( EraNodeToClientEnabled versionByron
                    :* EraNodeToClientEnabled versionShelley
                    :* EraNodeToClientEnabled versionAllegra
                    :* EraNodeToClientEnabled versionMary
                    :* EraNodeToClientEnabled versionAlonzo
                    :* EraNodeToClientEnabled versionBabbage
                    :* versionConway
                    :* versionDijkstra
                    :* Nil
                )
            )
            (injBabbage a)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , -- Conway + HardFork enabled
      ( \versionByron versionShelley versionAllegra versionMary versionAlonzo versionBabbage (WithVersion versionConway a) versionDijkstra ->
          WithVersion
            ( HardForkNodeToClientEnabled
                maxBound
                ( EraNodeToClientEnabled versionByron
                    :* EraNodeToClientEnabled versionShelley
                    :* EraNodeToClientEnabled versionAllegra
                    :* EraNodeToClientEnabled versionMary
                    :* EraNodeToClientEnabled versionAlonzo
                    :* EraNodeToClientEnabled versionBabbage
                    :* EraNodeToClientEnabled versionConway
                    :* versionDijkstra
                    :* Nil
                )
            )
            (injConway a)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    , -- Dijkstra + HardFork enabled
      ( \versionByron versionShelley versionAllegra versionMary versionAlonzo versionBabbage versionConway (WithVersion versionDijkstra a) ->
          WithVersion
            ( HardForkNodeToClientEnabled
                maxBound
                ( EraNodeToClientEnabled versionByron
                    :* EraNodeToClientEnabled versionShelley
                    :* EraNodeToClientEnabled versionAllegra
                    :* EraNodeToClientEnabled versionMary
                    :* EraNodeToClientEnabled versionAlonzo
                    :* EraNodeToClientEnabled versionBabbage
                    :* EraNodeToClientEnabled versionConway
                    :* EraNodeToClientEnabled versionDijkstra
                    :* Nil
                )
            )
            (injDijkstra a)
      )
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    ]

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToClientVersion (CardanoEras c))
        (CardanoBlock c)
    )
  where
  arbitrary =
    arbitraryNodeToClient
      BlockByron
      BlockShelley
      BlockAllegra
      BlockMary
      BlockAlonzo
      BlockBabbage
      BlockConway
      BlockDijkstra

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToClientVersion (CardanoEras c))
        (CardanoGenTx c)
    )
  where
  arbitrary =
    arbitraryNodeToClient
      GenTxByron
      GenTxShelley
      GenTxAllegra
      GenTxMary
      GenTxAlonzo
      GenTxBabbage
      GenTxConway
      GenTxDijkstra

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToClientVersion (CardanoEras c))
        (CardanoApplyTxErr c)
    )
  where
  arbitrary =
    frequency
      [
        ( 8
        , arbitraryNodeToClient
            ApplyTxErrByron
            ApplyTxErrShelley
            ApplyTxErrAllegra
            ApplyTxErrMary
            ApplyTxErrAlonzo
            ApplyTxErrBabbage
            ApplyTxErrConway
            ApplyTxErrDijkstra
        )
      ,
        ( 2
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (HardForkApplyTxErrWrongEra <$> arbitrary)
        )
      ]
  shrink = traverse aux
   where
    aux ::
      CardanoApplyTxErr StandardCrypto ->
      [CardanoApplyTxErr StandardCrypto]
    aux (HardForkApplyTxErrFromEra (OneEraApplyTxErr x)) =
      HardForkApplyTxErrFromEra . OneEraApplyTxErr <$> shrink x
    aux (HardForkApplyTxErrWrongEra x) =
      HardForkApplyTxErrWrongEra <$> shrink x

instance Arbitrary (Some QueryAnytime) where
  arbitrary = return $ Some GetEraStart

instance
  CardanoHardForkConstraints c =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToClientVersion (CardanoEras c))
        (Some (QueryHardFork (CardanoEras c)))
    )
  where
  arbitrary =
    frequency
      [
        ( 1
        , do
            version <- getHardForkEnabledNodeToClientVersion <$> arbitrary
            return $ WithVersion version (Some GetInterpreter)
        )
      ,
        ( 1
        , do
            version <- getHardForkEnabledNodeToClientVersion <$> arbitrary
            return $ WithVersion version (Some GetCurrentEra)
        )
      ]

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToClientVersion (CardanoEras c))
        (SomeBlockQuery (BlockQuery (CardanoBlock c)))
    )
  where
  arbitrary =
    frequency
      [
        ( 1
        , arbitraryNodeToClient
            injByron
            injShelley
            injAllegra
            injMary
            injAlonzo
            injBabbage
            injConway
            injDijkstra
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (injAnytimeByron <$> arbitrary)
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (injAnytimeShelley <$> arbitrary)
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (injAnytimeAllegra <$> arbitrary)
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (injAnytimeMary <$> arbitrary)
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (injAnytimeAlonzo <$> arbitrary)
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (injAnytimeBabbage <$> arbitrary)
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (injAnytimeConway <$> arbitrary)
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (injAnytimeDijkstra <$> arbitrary)
        )
      , (1, fmap injHardFork <$> arbitrary)
      ]
   where
    injByron (SomeBlockQuery query) = SomeBlockQuery (QueryIfCurrentByron query)
    injShelley (SomeBlockQuery query) = SomeBlockQuery (QueryIfCurrentShelley query)
    injAllegra (SomeBlockQuery query) = SomeBlockQuery (QueryIfCurrentAllegra query)
    injMary (SomeBlockQuery query) = SomeBlockQuery (QueryIfCurrentMary query)
    injAlonzo (SomeBlockQuery query) = SomeBlockQuery (QueryIfCurrentAlonzo query)
    injBabbage (SomeBlockQuery query) = SomeBlockQuery (QueryIfCurrentBabbage query)
    injConway (SomeBlockQuery query) = SomeBlockQuery (QueryIfCurrentConway query)
    injDijkstra (SomeBlockQuery query) = SomeBlockQuery (QueryIfCurrentDijkstra query)
    injAnytimeByron (Some query) = SomeBlockQuery (QueryAnytimeByron query)
    injAnytimeShelley (Some query) = SomeBlockQuery (QueryAnytimeShelley query)
    injAnytimeAllegra (Some query) = SomeBlockQuery (QueryAnytimeAllegra query)
    injAnytimeMary (Some query) = SomeBlockQuery (QueryAnytimeMary query)
    injAnytimeAlonzo (Some query) = SomeBlockQuery (QueryAnytimeAlonzo query)
    injAnytimeBabbage (Some query) = SomeBlockQuery (QueryAnytimeBabbage query)
    injAnytimeConway (Some query) = SomeBlockQuery (QueryAnytimeConway query)
    injAnytimeDijkstra (Some query) = SomeBlockQuery (QueryAnytimeDijkstra query)
    injHardFork (Some query) = SomeBlockQuery (QueryHardFork query)

instance Arbitrary History.EraEnd where
  arbitrary =
    oneof
      [ History.EraEnd <$> arbitrary
      , return History.EraUnbounded
      ]

instance Arbitrary History.EraSummary where
  arbitrary =
    History.EraSummary
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Arbitrary a, SListI xs) => Arbitrary (NonEmpty xs a) where
  arbitrary = do
    let nbXs = lengthSList (Proxy @xs)
    len <- choose (1, nbXs)
    xs <- vectorOf len arbitrary
    return $ fromMaybe (error "nonEmptyFromList failed") $ nonEmptyFromList xs

instance Arbitrary (History.Interpreter (CardanoEras c)) where
  arbitrary =
    History.mkInterpreter . History.Summary . enforceInvariant <$> arbitrary
   where
    -- Enforce the invariant that when the last era in the summary is the
    -- final era, it is unbounded. The decoder relies on this.
    enforceInvariant xs
      | length (nonEmptyToList xs) == lengthSList (Proxy @(CardanoEras c)) =
          fixEndBound xs
      | otherwise =
          xs

    fixEndBound ::
      NonEmpty xs History.EraSummary ->
      NonEmpty xs History.EraSummary
    fixEndBound (NonEmptyCons e es) = NonEmptyCons e (fixEndBound es)
    fixEndBound (NonEmptyOne e) =
      NonEmptyOne e{History.eraEnd = History.EraUnbounded}

instance Arbitrary (EraIndex (CardanoEras c)) where
  arbitrary = do
    let nbEras = lengthSList (Proxy @(CardanoEras c))
    index <- choose (0, fromIntegral nbEras - 1)
    case nsFromIndex index of
      Nothing -> error $ "nsFromIndex failed for " <> show index
      Just ns -> return $ eraIndexFromNS ns

instance
  c ~ StandardCrypto =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToClientVersion (CardanoEras c))
        (SomeResult (CardanoBlock c))
    )
  where
  arbitrary =
    frequency
      [
        ( 1
        , arbitraryNodeToClient
            injByron
            injShelley
            injAllegra
            injMary
            injAlonzo
            injBabbage
            injConway
            injDijkstra
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> genQueryIfCurrentResultEraMismatch
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> genQueryAnytimeResultByron
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> genQueryAnytimeResultShelley
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> genQueryAnytimeResultAllegra
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> genQueryAnytimeResultMary
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> genQueryAnytimeResultAlonzo
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> genQueryAnytimeResultBabbage
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> genQueryAnytimeResultConway
        )
      ,
        ( 1
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> genQueryAnytimeResultDijkstra
        )
      , (1, genQueryHardForkResult)
      ]
   where
    injByron (SomeResult q r) = SomeResult (QueryIfCurrentByron q) (QueryResultSuccess r)
    injShelley (SomeResult q r) = SomeResult (QueryIfCurrentShelley q) (QueryResultSuccess r)
    injAllegra (SomeResult q r) = SomeResult (QueryIfCurrentAllegra q) (QueryResultSuccess r)
    injMary (SomeResult q r) = SomeResult (QueryIfCurrentMary q) (QueryResultSuccess r)
    injAlonzo (SomeResult q r) = SomeResult (QueryIfCurrentAlonzo q) (QueryResultSuccess r)
    injBabbage (SomeResult q r) = SomeResult (QueryIfCurrentBabbage q) (QueryResultSuccess r)
    injConway (SomeResult q r) = SomeResult (QueryIfCurrentConway q) (QueryResultSuccess r)
    injDijkstra (SomeResult q r) = SomeResult (QueryIfCurrentDijkstra q) (QueryResultSuccess r)

    -- In practice, when sending a Byron query you'll never get a mismatch
    -- saying that your query is from the Shelley era while the ledger is
    -- from Byron. Only the inverse. We ignore that in this generator, as it
    -- doesn't matter for serialisation purposes, we just generate a random
    -- 'MismatchEraInfo'.
    genQueryIfCurrentResultEraMismatch :: Gen (SomeResult (CardanoBlock c))
    genQueryIfCurrentResultEraMismatch =
      oneof
        [ ( \(SomeResult q (_ :: result)) mismatch ->
              SomeResult (QueryIfCurrentByron q) (Left @_ @result mismatch)
          )
            <$> arbitrary
            <*> arbitrary
        , ( \(SomeResult q (_ :: result)) mismatch ->
              SomeResult (QueryIfCurrentShelley q) (Left @_ @result mismatch)
          )
            <$> arbitrary
            <*> arbitrary
        , ( \(SomeResult q (_ :: result)) mismatch ->
              SomeResult (QueryIfCurrentAllegra q) (Left @_ @result mismatch)
          )
            <$> arbitrary
            <*> arbitrary
        , ( \(SomeResult q (_ :: result)) mismatch ->
              SomeResult (QueryIfCurrentMary q) (Left @_ @result mismatch)
          )
            <$> arbitrary
            <*> arbitrary
        , ( \(SomeResult q (_ :: result)) mismatch ->
              SomeResult (QueryIfCurrentAlonzo q) (Left @_ @result mismatch)
          )
            <$> arbitrary
            <*> arbitrary
        , ( \(SomeResult q (_ :: result)) mismatch ->
              SomeResult (QueryIfCurrentBabbage q) (Left @_ @result mismatch)
          )
            <$> arbitrary
            <*> arbitrary
        , ( \(SomeResult q (_ :: result)) mismatch ->
              SomeResult (QueryIfCurrentConway q) (Left @_ @result mismatch)
          )
            <$> arbitrary
            <*> arbitrary
        , ( \(SomeResult q (_ :: result)) mismatch ->
              SomeResult (QueryIfCurrentDijkstra q) (Left @_ @result mismatch)
          )
            <$> arbitrary
            <*> arbitrary
        ]

    genQueryAnytimeResultByron :: Gen (SomeResult (CardanoBlock c))
    genQueryAnytimeResultByron =
      SomeResult (QueryAnytimeByron GetEraStart) <$> arbitrary

    genQueryAnytimeResultShelley :: Gen (SomeResult (CardanoBlock c))
    genQueryAnytimeResultShelley =
      SomeResult (QueryAnytimeShelley GetEraStart) <$> arbitrary

    genQueryAnytimeResultAllegra :: Gen (SomeResult (CardanoBlock c))
    genQueryAnytimeResultAllegra =
      SomeResult (QueryAnytimeAllegra GetEraStart) <$> arbitrary

    genQueryAnytimeResultMary :: Gen (SomeResult (CardanoBlock c))
    genQueryAnytimeResultMary =
      SomeResult (QueryAnytimeMary GetEraStart) <$> arbitrary

    genQueryAnytimeResultAlonzo :: Gen (SomeResult (CardanoBlock c))
    genQueryAnytimeResultAlonzo =
      SomeResult (QueryAnytimeAlonzo GetEraStart) <$> arbitrary

    genQueryAnytimeResultBabbage :: Gen (SomeResult (CardanoBlock c))
    genQueryAnytimeResultBabbage =
      SomeResult (QueryAnytimeBabbage GetEraStart) <$> arbitrary

    genQueryAnytimeResultConway :: Gen (SomeResult (CardanoBlock c))
    genQueryAnytimeResultConway =
      SomeResult (QueryAnytimeConway GetEraStart) <$> arbitrary

    genQueryAnytimeResultDijkstra :: Gen (SomeResult (CardanoBlock c))
    genQueryAnytimeResultDijkstra =
      SomeResult (QueryAnytimeDijkstra GetEraStart) <$> arbitrary

    genQueryHardForkResult ::
      Gen
        ( WithVersion
            (HardForkNodeToClientVersion (CardanoEras c))
            (SomeResult (CardanoBlock c))
        )
    genQueryHardForkResult =
      oneof
        [ WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (SomeResult (QueryHardFork GetInterpreter) <$> arbitrary)
        , WithVersion
            <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
            <*> (SomeResult (QueryHardFork GetCurrentEra) <$> arbitrary)
        ]

{------------------------------------------------------------------------------
  Ledger Config
------------------------------------------------------------------------------}

-- | See 'encodeNodeToClientNP' and 'decodeNodeToClientNP'.
instance
  CardanoHardForkConstraints c =>
  Arbitrary
    ( WithVersion
        (HardForkNodeToClientVersion (CardanoEras c))
        (HardForkLedgerConfig (CardanoEras c))
    )
  where
  arbitrary =
    WithVersion
      -- Use a version that enables all eras. We assume that all eras are
      -- enabled in the maximum supported version.
      (snd $ fromMaybe err $ Map.lookupMax $ supportedNodeToClientVersions (Proxy @(CardanoBlock c)))
      <$> arbitrary
   where
    err =
      error
        "Expected at least 1 supported note-to-client version, but `supportedNodeToClientVersions` has none"

instance
  CardanoHardForkConstraints c =>
  Arbitrary (HardForkLedgerConfig (CardanoEras c))
  where
  arbitrary = HardForkLedgerConfig <$> arbitrary <*> arbitrary

instance SListI xs => Arbitrary (History.Shape xs) where
  arbitrary = History.Shape . Exactly <$> hsequenceK (hpure (K arbitrary))

instance
  CardanoHardForkConstraints c =>
  Arbitrary (PerEraLedgerConfig (CardanoEras c))
  where
  arbitrary =
    fmap PerEraLedgerConfig . hsequence' $
      hcpure (Proxy @(Compose Arbitrary WrapPartialLedgerConfig)) (Comp arbitrary)

instance Arbitrary (PartialLedgerConfig blk) => Arbitrary (WrapPartialLedgerConfig blk) where
  arbitrary = WrapPartialLedgerConfig <$> arbitrary

instance Arbitrary ByronPartialLedgerConfig where
  arbitrary = ByronPartialLedgerConfig <$> arbitrary <*> arbitrary

instance
  Arbitrary (ShelleyLedgerConfig era) =>
  Arbitrary (ShelleyPartialLedgerConfig era)
  where
  arbitrary = ShelleyPartialLedgerConfig <$> arbitrary <*> arbitrary

instance Arbitrary TriggerHardFork where
  arbitrary =
    oneof
      [ TriggerHardForkAtVersion <$> arbitrary
      , TriggerHardForkAtEpoch <$> arbitrary
      , pure TriggerHardForkNotDuringThisExecution
      ]
