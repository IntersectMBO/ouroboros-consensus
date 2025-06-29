{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ThreadNet.TxGen.Cardano (CardanoTxGenExtra (..)) where

import qualified Cardano.Chain.Common as Byron
import Cardano.Chain.Genesis (GeneratedSecrets (..))
import Cardano.Crypto (toVerification)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Ledger.Address as SL (BootstrapAddress (..))
import qualified Cardano.Ledger.Hashes as SL
import Cardano.Ledger.Keys (DSIGN)
import qualified Cardano.Ledger.Keys.Bootstrap as SL (makeBootstrapWitness)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Core as SL
import qualified Cardano.Ledger.Shelley.Tx as SL
import Cardano.Ledger.Val ((<->))
import Cardano.Protocol.Crypto (VRF)
import Control.Exception (assert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.SOP.Strict
import Data.SOP.Telescope as Tele
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Cardano
import Ouroboros.Consensus.Cardano.Block
  ( CardanoEras
  , GenTx (..)
  , ShelleyEra
  )
import Ouroboros.Consensus.Cardano.Node (CardanoHardForkConstraints)
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Ledger
  ( getFlipTickedLedgerState
  , tickedHardForkLedgerStatePerEra
  )
import Ouroboros.Consensus.HardFork.Combinator.State.Types
  ( currentState
  , getHardForkState
  )
import Ouroboros.Consensus.Ledger.Basics
  ( ComputeLedgerEvents (..)
  , LedgerConfig
  , LedgerState
  , TickedLedgerState
  , applyChainTick
  )
import Ouroboros.Consensus.Ledger.Tables (ValuesMK)
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( applyDiffs
  , forgetLedgerTables
  )
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, mkShelleyTx)
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( tickedShelleyLedgerState
  )
import qualified Test.Cardano.Ledger.Core.KeyPair as TL (mkWitnessVKey)
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import Test.ThreadNet.TxGen

data CardanoTxGenExtra c = CardanoTxGenExtra
  { ctgeByronGenesisKeys :: GeneratedSecrets
  , ctgeNetworkMagic :: Byron.NetworkMagic
  , ctgeShelleyCoreNodes :: [Shelley.CoreNode c]
  }

instance CardanoHardForkConstraints c => TxGen (CardanoBlock c) where
  type TxGenExtra (CardanoBlock c) = CardanoTxGenExtra c

  -- TODO also generate " typical " Byron and Shelley transactions
  testGenTxs (CoreNodeId i) _ncn curSlot cfg extra ls =
    pure $ maybeToList $ migrateUTxO migrationInfo curSlot lcfg ls
   where
    lcfg = topLevelConfigLedger cfg

    CardanoTxGenExtra
      { ctgeByronGenesisKeys
      , ctgeNetworkMagic
      , ctgeShelleyCoreNodes
      } = extra

    GeneratedSecrets
      { gsRichSecrets
      } = ctgeByronGenesisKeys

    migrationInfo =
      MigrationInfo
        { byronMagic = ctgeNetworkMagic
        , byronSK
        , paymentSK
        , poolSK
        , stakingSK
        , vrfSK
        }

    byronSK :: Byron.SigningKey
    byronSK = gsRichSecrets !! fromIntegral i

    Shelley.CoreNode
      { Shelley.cnDelegateKey = paymentSK
      , Shelley.cnStakingKey = stakingSK
      , Shelley.cnVRF = vrfSK
      } = ctgeShelleyCoreNodes !! fromIntegral i

    -- Reuse the payment key as the pool key, since it's an individual
    -- stake pool and the namespaces are separate.
    poolSK :: DSIGN.SignKeyDSIGN DSIGN
    poolSK = paymentSK

-- | See 'migrateUTxO'
data MigrationInfo c = MigrationInfo
  { byronMagic :: Byron.NetworkMagic
  -- ^ Needed for creating a Byron address.
  , byronSK :: Byron.SigningKey
  -- ^ The core node's Byron secret.
  , paymentSK :: DSIGN.SignKeyDSIGN DSIGN
  , poolSK :: DSIGN.SignKeyDSIGN DSIGN
  , stakingSK :: DSIGN.SignKeyDSIGN DSIGN
  , vrfSK :: VRF.SignKeyVRF (VRF c)
  -- ^ To be re-used by the individual pool.
  }

-- | Convert a core node's utxo from Byron to an active Shelley stake pool.
--
-- Returns a transaction that registers a staking key, registers an individual
-- stake pool, delegates that stake key to that stake pool, and transfers all
-- utxo from the Byron 'byronAddr' to the Shelley address corresponding to the
-- pair of 'paymentSK' and 'stakingSK'.
--
-- It returns 'Nothing' if the core node does not have any utxo in its
-- 'byronAddr' (eg if this transaction has already been applied).
migrateUTxO ::
  forall c.
  CardanoHardForkConstraints c =>
  MigrationInfo c ->
  SlotNo ->
  LedgerConfig (CardanoBlock c) ->
  LedgerState (CardanoBlock c) ValuesMK ->
  Maybe (GenTx (CardanoBlock c))
migrateUTxO migrationInfo curSlot lcfg lst
  | Just utxo <- mbUTxO =
      let picked :: Map SL.TxIn (SL.TxOut ShelleyEra)
          picked = Map.filter pick $ SL.unUTxO utxo
           where
            pick (SL.ShelleyTxOut addr _) =
              addr == SL.AddrBootstrap (SL.BootstrapAddress byronAddr)

          -- Total held by 'byronAddr'
          pickedCoin :: SL.Coin
          pickedCoin = foldMap (\(SL.ShelleyTxOut _ coin) -> coin) picked

          -- NOTE: The Cardano ThreadNet tests use the
          -- ouroboros-consensus-shelley-test infra's genesis config, which sets
          -- relevant protocol params to 0.
          fee, deposits, spentCoin :: SL.Coin
          fee = SL.Coin 0
          deposits = SL.Coin 0
          spentCoin = deposits <> fee

          unspentCoin :: SL.Coin
          unspentCoin =
            assert (pickedCoin > spentCoin) $
              pickedCoin <-> spentCoin

          body :: SL.TxBody ShelleyEra
          body =
            SL.mkBasicTxBody
              & SL.certsTxBodyL
                .~ StrictSeq.fromList
                  [ SL.RegTxCert $ Shelley.mkCredential stakingSK
                  , SL.RegPoolTxCert $ poolParams unspentCoin
                  , SL.DelegStakeTxCert
                      (Shelley.mkCredential stakingSK)
                      (Shelley.mkKeyHash poolSK)
                  ]
              & SL.inputsTxBodyL .~ Map.keysSet picked
              & SL.outputsTxBodyL
                .~ StrictSeq.singleton (SL.ShelleyTxOut shelleyAddr unspentCoin)
              & SL.ttlTxBodyL .~ SlotNo maxBound
              & SL.feeTxBodyL .~ fee

          bodyHash :: SL.SafeHash SL.EraIndependentTxBody
          bodyHash = SL.hashAnnotated body

          -- Witness the use of bootstrap address's utxo.
          byronWit :: SL.BootstrapWitness
          byronWit =
            SL.makeBootstrapWitness (SL.extractHash bodyHash) byronSK $
              Byron.addrAttributes byronAddr

          -- Witness the stake delegation.
          delegWit :: SL.WitVKey 'SL.Witness
          delegWit =
            TL.mkWitnessVKey
              bodyHash
              (Shelley.mkKeyPair stakingSK)

          -- Witness the pool registration.
          poolWit :: SL.WitVKey 'SL.Witness
          poolWit =
            TL.mkWitnessVKey
              bodyHash
              (Shelley.mkKeyPair poolSK)
       in if Map.null picked
            then Nothing
            else
              (Just . GenTxShelley . mkShelleyTx) $
                SL.mkBasicTx body
                  & SL.witsShelleyTxL . SL.addrTxWitsL .~ Set.fromList [delegWit, poolWit]
                  & SL.witsShelleyTxL . SL.bootAddrTxWitsL .~ Set.singleton byronWit
  | otherwise = Nothing
 where
  mbUTxO :: Maybe (SL.UTxO ShelleyEra)
  mbUTxO =
    fmap getUTxOShelley
      . ejectShelleyTickedLedgerState
      . applyDiffs lst
      . applyChainTick OmitLedgerEvents lcfg curSlot
      . forgetLedgerTables
      $ lst

  MigrationInfo
    { byronMagic
    , byronSK
    , paymentSK
    , poolSK
    , stakingSK
    , vrfSK
    } = migrationInfo

  byronAddr :: Byron.Address
  byronAddr =
    Byron.makeVerKeyAddress byronMagic $ toVerification byronSK

  -- We use a base reference for the stake so that we can refer to it in the
  -- same tx that registers it.
  shelleyAddr :: SL.Addr
  shelleyAddr =
    SL.Addr
      Shelley.networkId
      (Shelley.mkCredential paymentSK)
      (SL.StakeRefBase $ Shelley.mkCredential stakingSK)

  -- A simplistic individual pool
  poolParams :: SL.Coin -> SL.PoolParams
  poolParams pledge =
    SL.PoolParams
      { SL.ppCost = SL.Coin 1
      , SL.ppMetadata = SL.SNothing
      , SL.ppMargin = minBound
      , SL.ppOwners = Set.singleton $ Shelley.mkKeyHash poolSK
      , SL.ppPledge = pledge
      , SL.ppId = Shelley.mkKeyHash poolSK
      , SL.ppRewardAccount =
          SL.RewardAccount Shelley.networkId $ Shelley.mkCredential poolSK
      , SL.ppRelays = StrictSeq.empty
      , SL.ppVrf = Shelley.mkKeyHashVrf @c vrfSK
      }

-----

ejectShelleyNS ::
  NS f (CardanoEras c) ->
  Maybe (f (ShelleyBlock (TPraos c) ShelleyEra))
ejectShelleyNS = \case
  S (Z x) -> Just x
  _ -> Nothing

getUTxOShelley ::
  TickedLedgerState (ShelleyBlock proto era) mk ->
  SL.UTxO era
getUTxOShelley tls =
  SL.utxosUtxo $
    SL.lsUTxOState $
      SL.esLState $
        SL.nesEs $
          tickedShelleyLedgerState tls

ejectShelleyTickedLedgerState ::
  TickedLedgerState (CardanoBlock c) mk ->
  Maybe (TickedLedgerState (ShelleyBlock (TPraos c) ShelleyEra) mk)
ejectShelleyTickedLedgerState ls =
  fmap (getFlipTickedLedgerState . currentState) $
    ejectShelleyNS $
      Tele.tip $
        getHardForkState $
          tickedHardForkLedgerStatePerEra $
            ls
