{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Cardano.Tools.DBSynthesizer.Tx
  ( makeGenTxs
  , OwnedTxIn(..)
  , UTxOSetSpec
  , TxOutsSpec(..)
  , TxOutSpec(..)
  , DelegationSpec(..)
  , NativeAssetMintsSpec(..)
  , DatumSpec(..)
  , TxGenCompatibleEra
  , NoV2Support(..)
  , AddTxException(..)
  ) where

import Cardano.Ledger.Plutus.ExUnits
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import Cardano.Ledger.Api.Tx (calcMinFeeTx)
import Cardano.Ledger.Api.Tx.Wits
import Cardano.Ledger.Api.Tx.Out
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Keys
import Cardano.Ledger.Crypto
import Control.Monad.Trans.Except
import Data.Proxy
import Data.SOP.Classes
import Data.SOP.BasicFunctors
import Ouroboros.Consensus.Byron.Ledger.Block
import Ouroboros.Consensus.Cardano.CanHardFork
import Control.Exception
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Config
import Cardano.Tools.DBSynthesizer.Forging
import Ouroboros.Consensus.Ledger.Basics
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Alonzo.Tx
import Data.Group
import Universe
import Data.Default
import Cardano.Ledger.Credential
import Control.Concurrent.MVar
import Cardano.Ledger.Coin
import Cardano.Ledger.Shelley.LedgerState hiding (LedgerState)
import           Ouroboros.Consensus.Shelley.Ledger
import Cardano.Ledger.Api.PParams
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.Alonzo.Scripts
import Data.Word
import Cardano.Ledger.Api.Scripts
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator
import Data.Coerce
import Lens.Micro
import qualified Data.Map.Strict as Map
import Cardano.Ledger.TxIn
import Cardano.Ledger.UTxO hiding (balance)
import Cardano.Ledger.Core
import Ouroboros.Consensus.Protocol.Praos
import qualified Data.Set as Set
import Cardano.Ledger.Mary.Value
import Data.Sequence.Strict as Seq
import Numeric.Natural
import PlutusLedgerApi.V1 hiding (TxOut, ScriptHash, Value)
import PlutusCore.Version
import UntypedPlutusCore hiding (Constr)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS
import Cardano.Ledger.Plutus.Data as PData

-- TODO: Distributions
-- TODO: reference scripts
data TxOutSpec = TxOutSpec
  { nativeAssets :: ![NativeAssetMintsSpec]
  , datum :: !(Maybe DatumSpec)
  , delegation :: !DelegationSpec
  } deriving stock Show

-- Intentionally omit Pointers
data DelegationSpec = DelegateHash | NoDelegate deriving stock Show

newtype NativeAssetMintsSpec = NativeAssetMintsSpec
  { nameLengths :: Set.Set Natural
  } deriving stock Show

data DatumSpec = ByHash | Inline !Natural deriving stock Show

data TxOutsSpec = TxOutsSpec
  { duplicates :: !Natural
  , txOut :: !TxOutSpec
  } deriving stock Show

type UTxOSetSpec = [TxOutsSpec]

data NoV2Support = NoV2Support deriving stock Show

instance Exception NoV2Support

mkMint :: (AlonzoEraScript era) => Natural -> (ScriptHash (EraCrypto era), Script era)
mkMint n = (hashScript script, script)
  where
    script = fromPlutusScript pScript
    pScript = case mkPlutusScript taggedSerialized of
      Nothing -> throw NoV2Support
      Just s -> s
    taggedSerialized = Plutus @'PlutusV2 $ PlutusBinary serialized
    serialized = serialiseUPLC prog
    prog = Program () plcVersion100 $ LamAbs () (DeBruijn 0)
      (LamAbs () (DeBruijn 0) (Constant () (someValue $ toInteger n)))

mkAssetName :: Natural -> AssetName
mkAssetName n = coerce (SBS.replicate (fromIntegral n) 0)

txGenLedgerState :: forall c. (CardanoHardForkConstraints c) => TickedLedgerState (CardanoBlock c) -> Maybe (TxGenLedgerState c)
txGenLedgerState = hcollapse . hcmap (Proxy @(MaybeTxGenLedgerState c)) (K . maybeTxGenLedgerState @c . unComp) . tickedHardForkLedgerStatePerEra

utxoLookupLedgerState :: forall c. (CardanoHardForkConstraints c) => TickedLedgerState (CardanoBlock c) -> TxIn c -> Maybe (Addr c, Coin)
utxoLookupLedgerState = hcollapse . hcmap (Proxy @(UTxOLookup c)) (K . utxoLookup @c . unComp) . tickedHardForkLedgerStatePerEra

-- | At least Babbage
type TxGenCompatibleEra era =
  ( ShelleyBasedEra era
  , Value era ~ MaryValue (EraCrypto era)
  , AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , BabbageEraTxOut era
  , EraUTxO era
  )
data TxGenLedgerState c = forall era proto . (c ~ EraCrypto era, TxGenCompatibleEra era) => TxGenLedgerState
  { nes :: !(NewEpochState era)
  , mkCardanoTx :: !(GenTx (ShelleyBlock proto era) -> CardanoGenTx c)
  , mkMintingPurpose :: !(forall f. f Word32  (PolicyID (EraCrypto era)) -> PlutusPurpose f era)
  }

data AddTxException c = AddTxException
  { tx :: !(GenTx (CardanoBlock c))
  , err :: !(ApplyTxErr (CardanoBlock c))
  }

deriving stock instance (CardanoHardForkConstraints c) => Show (AddTxException c)

instance (CardanoHardForkConstraints c) => Exception (AddTxException c)

class MaybeTxGenLedgerState c blk where
  maybeTxGenLedgerState :: Ticked (LedgerState blk) -> Maybe (TxGenLedgerState c)

instance MaybeTxGenLedgerState c ByronBlock where
  maybeTxGenLedgerState = const Nothing

class ShelleyTxGenLedgerState era where
  maybeShelleyTxGenLedgerState :: NewEpochState era -> Maybe (TxGenLedgerState (EraCrypto era))

-- Duplicate instead of an overlappable not-supported case so we get an error in new eras
instance ShelleyTxGenLedgerState (ShelleyEra c) where
  maybeShelleyTxGenLedgerState = const Nothing
instance ShelleyTxGenLedgerState (AllegraEra c) where
  maybeShelleyTxGenLedgerState = const Nothing
instance ShelleyTxGenLedgerState (MaryEra c) where
  maybeShelleyTxGenLedgerState = const Nothing
instance ShelleyTxGenLedgerState (AlonzoEra c) where
  maybeShelleyTxGenLedgerState = const Nothing
instance (PraosCrypto c) => ShelleyTxGenLedgerState (BabbageEra c) where
  maybeShelleyTxGenLedgerState nes = Just $ TxGenLedgerState
    { nes
    , mkCardanoTx = GenTxBabbage
    , mkMintingPurpose = AlonzoMinting
    }
instance (PraosCrypto c) => ShelleyTxGenLedgerState (ConwayEra c) where
  maybeShelleyTxGenLedgerState nes = Just $ TxGenLedgerState
    { nes
    , mkCardanoTx = GenTxConway
    , mkMintingPurpose = ConwayMinting
    }

instance (ShelleyTxGenLedgerState era, c ~ EraCrypto era) => MaybeTxGenLedgerState c (ShelleyBlock proto era) where
  maybeTxGenLedgerState st = maybeShelleyTxGenLedgerState st.tickedShelleyLedgerState

class UTxOLookup c blk where
  utxoLookup :: Ticked (LedgerState blk) -> TxIn c -> Maybe (Addr c, Coin)

instance UTxOLookup c ByronBlock where
  utxoLookup _ _ = Nothing

instance (c ~ EraCrypto era, EraTxOut era) => UTxOLookup c (ShelleyBlock proto era) where
  utxoLookup tls txIn = extract <$> Map.lookup txIn utxoMap
    where
      utxoMap = unUTxO . utxosUtxo . lsUTxOState . esLState . nesEs $ tls.tickedShelleyLedgerState
      extract txOut = (txOut ^. addrTxOutL, txOut ^. coinTxOutL)

data OwnedTxIn c = OwnedTxIn
  { owned :: !(TxIn c)
  , skey :: !(Crypto.SignKeyDSIGN (DSIGN c))
  }

deriving stock instance (Crypto c) => Show (OwnedTxIn c)

data ProducingState c = ProducingState
  { spec :: !UTxOSetSpec
  , uTxOIn :: !(TxIn c)
  , addr :: !(Addr c)
  , balance :: !Coin
  } deriving stock Show

data InitState c = InitState
  { initSpec :: !UTxOSetSpec
  , initIn :: !(TxIn c)
  } deriving stock Show

data MakeGenTxsState c
  = InitUTxONotFound !(InitState c)
  | Producing !(ProducingState c) deriving stock Show

makeGenTxs :: forall c. (CardanoHardForkConstraints c) => OwnedTxIn c -> UTxOSetSpec -> IO (TopLevelConfig (CardanoBlock c) -> GenTxs (CardanoBlock c))
makeGenTxs (OwnedTxIn { owned, skey }) initSpec = do
    stVar <- newMVar . InitUTxONotFound $ InitState { initIn = owned, initSpec }
    pure $ \cfg slot tls -> modifyMVar stVar $ pure . \case
      InitUTxONotFound st -> tryInitialize tls cfg slot st
      Producing st -> go tls cfg slot st
  where
    vkey = VKey @Witness @c $ Crypto.deriveVerKeyDSIGN skey
    -- A dummy txbody signature to initialize the tx with the appropriate size
    dummySig = signedDSIGN @c skey . extractHash . hashAnnotated $ mkBasicTxBody @(BabbageEra c)
    tryInitialize tls cfg slot st = case utxoLookupLedgerState tls uTxOIn of
        Nothing -> (InitUTxONotFound st, [])
        Just (addr, balance) -> go tls cfg slot $ ProducingState
          { spec = st.initSpec
          , uTxOIn
          , addr
          , balance
          }
      where
        uTxOIn = st.initIn
    go tls cfg slot st = case txGenLedgerState tls of
      Nothing -> (Producing st, [])
      Just TxGenLedgerState {nes, mkCardanoTx, mkMintingPurpose} ->
        let
          utxos = utxosUtxo . lsUTxOState . esLState $ nesEs nes
          params = getPParams nes
          langViews = Set.singleton (getLanguageView params PlutusV2)
          net = getNetwork $ st.addr
          undelegAddr = Addr net def StakeRefNull
          delegAddr = Addr net def (StakeRefBase def)

          -- Create as many transactions as we can fit in this block (unless we finish everything
          -- our specs request) and update our state accordingly
          makeTxs currSt currTls remainingBodySize =
              if txFits
              then (lastSt, vtx : lastTxs)
              else (currSt, [])
            where
              -- The transaction fits if we were able to add any outputs at all, and if we have outputs we need some min-ADA for them
              txFits = minAda /= mempty

              -- Make more transactions, if there's room
              (lastSt, lastTxs) = makeTxs nextSt nextTls nextBodySize

              nextSt = currSt
                { spec = nextSpec
                , -- Use the change output as the input to the next transaction
                  -- Note that this will fail disastrously if we have a transaction
                  -- rejected (e.g. because we miscalulated block limits) or there
                  -- is a rollback. Oh well, we'll get an exception when validating
                  -- in that case.
                  uTxOIn = TxIn (txIdTx tx) minBound
                , balance = change
                }

              -- Validate the transaction we've constructed
              (nextTls, vtx) = case runExcept eVtx of
                Left err -> throw $ AddTxException { tx = gtx, err } -- TODO put makeTxs in IO and throwIO here?
                Right x -> x
              eVtx = applyTx
                (configLedger cfg)
                Intervene
                slot
                gtx
                currTls
              gtx = mkCardanoTx $ mkShelleyTx tx

              -- Initialize the transaction
              -- We add a dummy tx witness so our size checks are accurate
              initTx = mkBasicTx initTxBody
                & witsTxL . addrTxWitsL .~ Set.singleton (WitVKey vkey dummySig)
              initTxBody = mkBasicTxBody
                & inputsTxBodyL .~ Set.singleton currSt.uTxOIn -- Take all of our ADA from the pool
                & outputsTxBodyL .~ Seq.singleton initChange -- Add a change output, which we'll adjust later
                & collateralInputsTxBodyL .~ Set.singleton currSt.uTxOIn -- Use the same input for collateral (which we'll never use, but may need to put up if we're minting)
              -- A change output sent back to the pool owner addr
              initChange = mkBasicTxOut currSt.addr $ MaryValue currSt.balance mempty

              -- Add as many outputs as will fit in this tx
              -- (bounded by both the tx size limit and the amount
              -- of space left in this block)
              (noFeeTx, minAda, nextSpec) = addOuts initTx currSt.spec
              addOuts txSoFar [] = (txSoFar, mempty, []) -- Our spec doesn't need any more outputs
              addOuts txSoFar specSoFar@(TxOutsSpec { duplicates, txOut } : outss) =
                  if outFits
                  then (finalTx, finalAda <> minOut, finalSpecOuts)
                  else (txSoFar, mempty, specSoFar)
                where
                  -- We can add an output if the transaction with that output fits in the max tx size and the remaining block space
                  outFits = params ^. ppMaxTxSizeL > (fromIntegral $ txStep ^. sizeTxF) && remainingBodySize > (txInBlockSize . mkCardanoTx $ mkShelleyTx txStep)

                  -- Add more UTxOs, if we have room
                  (finalTx, finalAda, finalSpecOuts) =
                    addOuts txStep specStep

                  -- What does our spec look like after adding one instance
                  -- of txOut?
                  specStep = if duplicates == 0
                    then outss
                    else TxOutsSpec { duplicates = duplicates - 1, txOut } : outss

                  -- The transaction after adding this output
                  txStep = txNoOut
                    & bodyTxL . outputsTxBodyL %~ (|> nextOut)

                  -- TODO The txout is a function of pparams (for min-ADA) and txOut,
                  -- but we're constantly recalculating it. We should cache it.

                  -- Add the min-ADA to the output
                  nextOut = setMinCoinTxOut params noMinOut
                  minOut = nextOut ^. coinTxOutL

                  -- Set the output's native assets
                  noMinOut = noMA
                    & valueTxOutL .~ MaryValue mempty (MultiAsset nextMA)

                  -- Set the destination address of this UTxO by our spec
                  nextAddr = case txOut.delegation of
                    DelegateHash -> delegAddr
                    NoDelegate -> undelegAddr

                  -- Initialize the UTxO with the specified address and datum
                  noMA = mkBasicTxOut nextAddr mempty
                    & datumTxOutL .~ case txOut.datum of
                                       Nothing -> NoDatum
                                       Just ByHash -> PData.DatumHash def
                                       Just (Inline sz) ->
                                         -- Set the datum to a bytestring of zeroes of the requested length
                                         -- TODO There is probably some overhead for serializing a bytestring, if so we need to subtract that out
                                         PData.Datum . dataToBinaryData . PData.Data . B $
                                         BS.replicate (fromIntegral sz) 0

                  -- Fold over the native asset specification, accumulating the needed amounts to
                  -- add to this UTxO and adding needed minting configuration to the transaction
                  (txNoOut, nextMA, _) = foldr accMA (txSoFar, mempty, 0) txOut.nativeAssets
                  accMA nams (txAcc, maAcc, idx) = (txAcc', maAcc', idx + 1)
                    where
                      -- Use a redeemer of (), execution units determined empirically
                      -- TODO actually calculate the needed execution units
                      rdmr = (PData.Data $ Constr 0 [], ExUnits 800 162400)
                      -- Modify the transaction with needed mint scripts/redeemers and update the
                      -- mint field
                      txNoIntegrity = txAcc
                        & witsTxL . scriptTxWitsL %~ Map.insert mintHash mintBytes
                        & witsTxL . rdmrsTxWitsL %~ (\(Redeemers rdmrs) -> Redeemers $ Map.insert (mkMintingPurpose . AsIndex $ fromIntegral idx) rdmr rdmrs)
                        & bodyTxL . mintTxBodyL %~ \(MultiAsset m) -> MultiAsset $ Map.alter mintAlter (PolicyID mintHash) m
                      mintAlter Nothing = Just mintUpdates
                      mintAlter (Just orig) = Just $ Map.unionWith (+) mintUpdates orig

                      -- Update the script integrity hash to include the current scripts
                      -- TODO cache/defer this somehow?
                      txAcc' = txNoIntegrity
                        & bodyTxL . scriptIntegrityHashTxBodyL .~ hashScriptIntegrity langViews (txNoIntegrity ^. witsTxL . rdmrsTxWitsL) (TxDats mempty)

                      -- Add the requested native assets to this txout
                      maAcc' = Map.insert (PolicyID mintHash) mintUpdates maAcc
                      -- For each requested asset name length, create 1 new token whose name has that length
                      mintUpdates = Map.fromSet (const 1) $ Set.map mkAssetName nams.nameLengths

                      -- Make a new always-succeeding mint script (whose hash depends on the index in the native assets specification)
                      (mintHash, mintBytes) = mkMint idx

              -- Finalize the transaction with the fee and setting the change output to balance
              setMinFee prevTx = if prevTx ^. bodyTxL . feeTxBodyL == neededFee
                  then (neededChange, nextTx)
                  else setMinFee nextTx
                where
                  neededFee = calcMinFeeTx utxos params prevTx 0
                  neededChange = currSt.balance ~~ (minAda <> neededFee)
                  nextTx = prevTx
                    & bodyTxL . feeTxBodyL .~ neededFee
                    & bodyTxL . outputsTxBodyL %~ setChange neededChange
              (change, unsignedTx) = setMinFee noFeeTx
              setChange _ Seq.Empty = Seq.Empty
              setChange c (x :<| outs) = (x & coinTxOutL .~ c) :<| outs

              -- Witness the transaction
              tx = unsignedTx
                & witsTxL . addrTxWitsL .~ Set.singleton (WitVKey vkey sig)
              sig = signedDSIGN @c skey hash
              hash = extractHash . hashAnnotated $ unsignedTx ^. bodyTxL

              -- How much space is left in the block after this transaction?
              nextBodySize = remainingBodySize - txInBlockSize gtx
          (finalSt, txs) = makeTxs st tls (txsMaxBytes tls)
        in (Producing finalSt, txs)
