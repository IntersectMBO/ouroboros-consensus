{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Transaction generation for 'Cardano.Tools.DBSynthesizer.Run.synthesize'.
--
-- The tool fills each block with a chain of transactions. Each transaction
-- spends the output that the transaction before it made.
module Cardano.Tools.DBSynthesizer.TxGen
  ( mkRespendTxGen
  ) where

import Cardano.Api.Any (displayError)
import Cardano.Api.Key (AsType (AsSigningKey), Key (SigningKey))
import Cardano.Api.KeysShelley (AsType (AsPaymentKey), PaymentKey, SigningKey (PaymentSigningKey))
import Cardano.Api.SerialiseTextEnvelope (readFileTextEnvelope)
import Cardano.Crypto.DSIGN (SignKeyDSIGN)
import Cardano.Ledger.Api
  ( Addr (Addr)
  , EraTx
  , EraTxOut
  , Tx
  , TxOut
  , addrTxOutL
  , bodyTxL
  , coinTxOutL
  , feeTxBodyL
  , inputsTxBodyL
  , mkBasicTx
  , mkBasicTxBody
  , mkBasicTxOut
  , outputsTxBodyL
  )
import Cardano.Ledger.Api.Tx.In (TxIn (TxIn))
import Cardano.Ledger.BaseTypes (TxIx (TxIx))
import Cardano.Ledger.Coin (Coin (Coin), unCoin)
import Cardano.Ledger.Core (TopTx, txIdTx)
import qualified Cardano.Ledger.Keys as LK
import Cardano.Ledger.Val (inject)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Tools.DBSynthesizer.Forging (GenTxs)
import Control.DeepSeq (force)
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Measure as Measure
import Data.Proxy (Proxy (Proxy))
import Data.SOP.BasicFunctors (K (K))
import Data.SOP.Dict (Dict (Dict))
import Data.SOP.Index (Index (IS, IZ), dictIndexAll, hcimap, injectNS)
import Data.SOP.Strict (hcollapse)
import Data.Sequence.Strict ((|>))
import qualified Data.Set as Set
import Lens.Micro ((%~), (.~), (^.))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CardanoEras, LedgerState)
import Ouroboros.Consensus.Cardano.Ledger ()
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock (proxySingle)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraGenTx (OneEraGenTx))
import Ouroboros.Consensus.HardFork.Combinator.Ledger
  ( HasCanonicalTxIn (injectCanonicalTxIn)
  , Ticked (TickedHardForkLedgerState)
  , ejectLedgerTables
  )
import Ouroboros.Consensus.HardFork.Combinator.Mempool (GenTx (HardForkGenTx))
import Ouroboros.Consensus.Ledger.Basics (TickedLedgerState)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( TxMeasure
  , Validated
  , WhetherToIntervene (DoNotIntervene)
  , applyTx
  , blockCapacityTxMeasure
  , ebCapacityTxMeasure
  , txMeasure
  )
import Ouroboros.Consensus.Ledger.Tables
  ( KeysMK (KeysMK)
  , LedgerTables (LedgerTables)
  , ValuesMK
  , castLedgerTables
  , getLedgerTables
  , getValuesMK
  , ltliftA2
  )
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( applyDiffForKeysOnTables
  , applyDiffs
  , emptyLedgerTables
  , ltprj
  , unionValues
  )
import Ouroboros.Consensus.Shelley.Ledger (IsShelleyBlock, ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( BigEndianTxIn (BigEndianTxIn, getOriginalTxIn)
  , ShelleyBasedEra
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( RangeQueryPrevious (NoPreviousQuery, PreviousQueryWasUpTo)
  , ReadOnlyForker (roforkerRangeReadTables, roforkerReadTables)
  )
import Test.ThreadNet.Infra.Shelley (mkCredential, signTx)

type Cardano = CardanoBlock StandardCrypto

-- | The fee that each generated transaction pays.
--
-- The tool pays a fixed fee and does not compute the minimum. If the fee is
-- too low, 'applyTx' rejects the transaction. The ledger error names the fee
-- that it needs.
txFee :: Coin
txFee = Coin 1_000_000

-- | Build the generator that the forge loop runs on each slot that the tool
-- leads.
mkRespendTxGen ::
  Maybe FilePath ->
  IO (Either String (TopLevelConfig Cardano -> GenTxs Cardano))
mkRespendTxGen Nothing =
  pure $ Right $ \_cfg _slot _certifies _forker _ticked -> pure ([], [])
mkRespendTxGen (Just keyFile) = do
  lastOutput <- newIORef Nothing
  fmap (respendTxGen lastOutput) <$> readPaymentSigningKey keyFile

-- | Read a payment signing key from the JSON key file that
-- @cardano-cli address key-gen@ writes.
readPaymentSigningKey :: FilePath -> IO (Either String (SigningKey PaymentKey))
readPaymentSigningKey path =
  first displayError <$> readFileTextEnvelope (AsSigningKey AsPaymentKey) path

-- | The two inputs that a forged block leaves unspent.
data Leftovers = Leftovers
  { unspentNow :: TxIn
  -- ^ The last transaction of the ranking block made this output. The next
  -- block spends it. The transactions of the endorser block did not apply. So
  -- the ledger holds none of their outputs.
  , unspentIfCertified :: TxIn
  -- ^ The last transaction of the endorser block made this output. If a later
  -- block certifies the endorser block, its transactions apply. The next block
  -- then spends this output instead. If the block announced no endorser block,
  -- this equals 'unspentNow'.
  }

-- | A position in the sequence of transactions that the generator makes.
--
-- Each transaction spends the output that the one before it made. A position is
-- that output plus the ledger state that holds it, because the next transaction
-- needs both.
data Cursor era = Cursor
  { cursorState :: TickedLedgerState Cardano ValuesMK
  -- ^ The ledger state after every transaction up to this position applied.
  , cursorEntry :: (TxIn, TxOut era)
  -- ^ The output that the next transaction spends.
  }

-- | The result of 'fillBatch'.
data Batch era = Batch
  { batchTxs :: [Validated (GenTx Cardano)]
  -- ^ The transactions it accepted, in the order the ledger applies them.
  , batchUsedTotal :: TxMeasure Cardano
  -- ^ The total measure of the transactions made for this forging opportunity,
  -- including this batch. It spans the ranking block and the endorser block,
  -- because 'fillEb' bounds their sum.
  , batchCursor :: Cursor era
  -- ^ The position where it stopped. That position's output is unspent.
  }

-- | Fill the block of this slot with transactions.
--
-- The genesis must hold an initialFunds entry for the address of the key. If
-- the entry is absent, the generator stops the run. No later slot adds it.
--
-- The 'IORef' holds what the block before this one left unspent. It is empty on
-- the first slot of a run.
respendTxGen ::
  IORef (Maybe Leftovers) ->
  SigningKey PaymentKey ->
  TopLevelConfig Cardano ->
  GenTxs Cardano
respendTxGen lastOutput (PaymentSigningKey signKey) cfg slot certifies forker ticked
  -- The certified endorser block's transactions apply with this block. This
  -- block drops its own. So the ledger state holds the output that the last
  -- transaction of that endorser block made.
  --
  -- It announces no endorser block of its own. Such a block would spend outputs
  -- that the certified endorser block makes. Those outputs are absent from the
  -- ledger state that this generator reads.
  | certifies = do
      modifyIORef' lastOutput . fmap $ \previous ->
        Leftovers
          { unspentNow = unspentIfCertified previous
          , unspentIfCertified = unspentIfCertified previous
          }
      pure ([], [])
  | otherwise = case ticked of
      TickedHardForkLedgerState _transition perEra ->
        hcollapse $ hcimap proxySingle (\idx _state -> K (genForEra idx)) perEra
 where
  lcfg = configLedger cfg

  -- The era must come from the ticked state, because that is the era of the
  -- block that this slot forges. The era of the parent block is one behind
  -- across a hard fork. The hard-fork combinator translates a transaction of
  -- the era before, but the translation gives it a different id, and the
  -- 'IORef' below then holds an input that no block ever made.
  --
  -- The match on the index gives the ledger types of that era, and it passes
  -- the same index on, so no pairing here can go wrong.
  genForEra ::
    Index (CardanoEras StandardCrypto) x ->
    IO ([Validated (GenTx Cardano)], [Validated (GenTx Cardano)])
  genForEra = \case
    IZ ->
      throwIO . userError $
        "db-synthesizer: transaction generation not supported in the Byron era."
    IS idx -> case dictIndexAll (Proxy @IsShelleyBlock) idx of
      Dict -> genFor (IS idx)

  genFor ::
    forall proto era.
    ShelleyBasedEra era =>
    Index (CardanoEras StandardCrypto) (ShelleyBlock proto era) ->
    IO ([Validated (GenTx Cardano)], [Validated (GenTx Cardano)])
  genFor idx = do
    (keys, valuesAtParent) <- readCandidates idx
    -- The forker is at the point of the parent block, so its values are those
    -- of the ledger state after the parent block. The tick to this slot adds
    -- the diffs that 'ticked' holds. The UTxO of this slot needs both.
    let keysForSlot :: LedgerTables (TickedLedgerState Cardano) KeysMK
        keysForSlot = castLedgerTables keys

        valuesForSlot :: LedgerTables (TickedLedgerState Cardano) ValuesMK
        valuesForSlot = castLedgerTables valuesAtParent

        stateAtSlot :: TickedLedgerState Cardano ValuesMK
        stateAtSlot = applyDiffForKeysOnTables valuesForSlot keysForSlot ticked

        tablesAtSlot :: LedgerTables (LedgerState Cardano) ValuesMK
        tablesAtSlot = ltprj stateAtSlot

        -- The table of a CardanoBlock keeps a hard-fork wrapper on each key and
        -- on each value. 'ejectLedgerTables' removes both wrappers, and gives
        -- the types of one era. It also translates an output that an earlier
        -- era made. A chain that stays in one era holds no such output. A chain
        -- that crosses a hard fork holds outputs of the eras before the fork.
        utxo :: Map.Map TxIn (TxOut era)
        utxo =
          Map.mapKeys getOriginalTxIn . getValuesMK . getLedgerTables $
            ejectLedgerTables idx tablesAtSlot

    case Map.toList (Map.filter (ownedBy signKey) utxo) of
      [] ->
        throwIO . userError $
          "db-synthesizer: the payment signing key owns no output at slot "
            ++ show slot
            ++ ". Add an initialFunds entry for its address to the Shelley genesis."
      entry : _ -> fillBlock (wrapGenTx idx) stateAtSlot entry

  wrapGenTx ::
    Index (CardanoEras StandardCrypto) x ->
    GenTx x ->
    GenTx Cardano
  wrapGenTx idx = HardForkGenTx . OneEraGenTx . injectNS idx

  -- The ledger applies the transactions of a block in order. So a transaction
  -- can spend an output that an earlier transaction of the same block made.
  fillBlock ::
    forall proto era.
    ShelleyBasedEra era =>
    (GenTx (ShelleyBlock proto era) -> GenTx Cardano) ->
    TickedLedgerState Cardano ValuesMK ->
    (TxIn, TxOut era) ->
    IO ([Validated (GenTx Cardano)], [Validated (GenTx Cardano)])
  fillBlock wrap stateAtSlot start = do
    rb <- fillRb Cursor{cursorState = stateAtSlot, cursorEntry = start}
    when (null (batchTxs rb)) $
      throwIO . userError $
        "db-synthesizer: not one generated transaction fits in the ranking block at slot "
          ++ show slot
          ++ "."
    (ebTxs, cursorAfterEb) <- fillEb (batchUsedTotal rb) (batchCursor rb)
    writeIORef
      lastOutput
      ( Just
          Leftovers
            { unspentNow = fst (cursorEntry (batchCursor rb))
            , unspentIfCertified = fst (cursorEntry cursorAfterEb)
            }
      )
    pure (batchTxs rb, ebTxs)
   where
    rbCapacity :: TxMeasure Cardano
    rbCapacity = blockCapacityTxMeasure lcfg stateAtSlot

    -- Fill the ranking block, from the start of the block.
    fillRb :: Cursor era -> IO (Batch era)
    fillRb = fillBatch rbCapacity Measure.zero

    -- Resume where the ranking block stopped, and fill the endorser block. The
    -- endorser block takes what the ranking block could not, up to the two
    -- capacities added and counted from the start of the block. This is what
    -- 'partitionMempool' does in the production forge loop.
    fillEb ::
      TxMeasure Cardano ->
      Cursor era ->
      IO ([Validated (GenTx Cardano)], Cursor era)
    fillEb usedByRb cursor = case ebCapacityTxMeasure lcfg stateAtSlot of
      -- The era has no Leios, so this block announces no endorser block.
      Nothing -> pure ([], cursor)
      Just ebCap -> do
        batch <- fillBatch (rbCapacity `Measure.plus` ebCap) usedByRb cursor
        -- No batch follows the endorser block, so its measure has no consumer.
        pure (batchTxs batch, batchCursor batch)

    -- Build one transaction at a time, each spending the output that the one
    -- before it made. Stop at the first transaction that takes the total over
    -- the bound, and leave that transaction out.
    fillBatch ::
      -- The bound on the total measure.
      TxMeasure Cardano ->
      -- The total measure of the transactions made for this forging opportunity
      -- before this batch.
      TxMeasure Cardano ->
      Cursor era ->
      IO (Batch era)
    fillBatch bound = go []
     where
      go accepted used cursor = do
        let Cursor{cursorState = state, cursorEntry = (txIn, txOut)} = cursor
        (tx, madeOut) <- respendTx signKey txIn txOut
        let genTx = wrap (mkShelleyTx tx)
        measured <- case runExcept (txMeasure lcfg state genTx) of
          Left err ->
            throwIO . userError $
              "db-synthesizer: a generated transaction breaks a per-transaction limit at slot "
                ++ show slot
                ++ ": "
                ++ show err
          Right measured -> pure measured
        let used' = used `Measure.plus` measured
        if not (used' Measure.<= bound)
          then
            pure
              Batch
                { batchTxs = reverse accepted
                , batchUsedTotal = used
                , batchCursor = cursor
                }
          else case runExcept (applyTx lcfg DoNotIntervene slot genTx state) of
            Left err ->
              throwIO . userError $
                "db-synthesizer: the ledger rejected a generated transaction at slot "
                  ++ show slot
                  ++ ": "
                  ++ show err
            Right (stateAfterTx, validatedTx) ->
              go
                (validatedTx : accepted)
                used'
                Cursor
                  { cursorState = applyDiffs state stateAfterTx
                  , cursorEntry = (TxIn (txIdTx tx) (TxIx 0), madeOut)
                  }

  -- If the 'IORef' holds nothing, the generator reads the whole table. The
  -- 'IORef' is empty on the first slot of a run.
  --
  -- The ledger must hold 'unspentNow'. This tool forges one chain and has no
  -- competitor. So the ChainDB adopts every block that the tool makes. If the
  -- input is absent, the ChainDB rejected a block.
  readCandidates ::
    forall proto era.
    Index (CardanoEras StandardCrypto) (ShelleyBlock proto era) ->
    IO
      ( LedgerTables (ExtLedgerState Cardano) KeysMK
      , LedgerTables (ExtLedgerState Cardano) ValuesMK
      )
  readCandidates idx =
    readIORef lastOutput >>= \case
      Nothing -> do
        values <- readWholeUtxo forker
        pure (keysOf values, values)
      Just leftovers -> do
        let keys = oneKey idx (unspentNow leftovers)
        values <- roforkerReadTables forker keys
        if Map.null (getValuesMK (getLedgerTables values))
          then
            throwIO . userError $
              "db-synthesizer: the ChainDB did not adopt the block before slot "
                ++ show slot
                ++ ", so the output that it made is absent from the ledger."
          else pure (keys, values)
   where
    keysOf values =
      LedgerTables . KeysMK . Map.keysSet . getValuesMK . getLedgerTables $ values

  oneKey ::
    forall proto era.
    Index (CardanoEras StandardCrypto) (ShelleyBlock proto era) ->
    TxIn ->
    LedgerTables (ExtLedgerState Cardano) KeysMK
  oneKey idx txIn =
    LedgerTables . KeysMK . Set.singleton $ injectCanonicalTxIn idx (BigEndianTxIn txIn)

-- | Spend one output and pay the rest of it back to the same address.
--
-- The caller spends the returned output in the next transaction of the same
-- block.
respendTx ::
  EraTx era =>
  SignKeyDSIGN LK.DSIGN ->
  TxIn ->
  TxOut era ->
  IO (Tx TopTx era, TxOut era)
respendTx signKey txIn txOut
  | unCoin change <= 0 =
      throwIO . userError $
        "db-synthesizer: the output that the payment signing key owns holds "
          ++ show (unCoin inputValue)
          ++ " lovelace, which does not cover the fee of "
          ++ show (unCoin txFee)
          ++ " lovelace."
  | otherwise =
      -- The forge loop runs NoThunks over the state it buffers.
      pure . force $
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL %~ Set.insert txIn
            & bodyTxL . outputsTxBodyL %~ (|> madeOut)
            & bodyTxL . feeTxBodyL .~ txFee
            & signTx signKey
        , madeOut
        )
 where
  inputValue = txOut ^. coinTxOutL
  change = Coin (unCoin inputValue - unCoin txFee)
  madeOut = mkBasicTxOut (txOut ^. addrTxOutL) (inject change)

ownedBy :: EraTxOut era => SignKeyDSIGN LK.DSIGN -> TxOut era -> Bool
ownedBy signKey txOut = case txOut ^. addrTxOutL of
  Addr _ credential _ -> credential == mkCredential signKey
  _ -> False

-- | Read every entry of the UTxO, one page at a time.
--
-- If the table holds more than 'maxUtxoEntries' entries, the read stops with an
-- error. A partial read hides the entry that the key owns. The generator then
-- reports the wrong error.
readWholeUtxo ::
  ReadOnlyForker IO (ExtLedgerState Cardano) ->
  IO (LedgerTables (ExtLedgerState Cardano) ValuesMK)
readWholeUtxo forker = go emptyLedgerTables NoPreviousQuery
 where
  go acc prev = do
    (page, mLastKey) <- roforkerRangeReadTables forker prev
    let acc' = ltliftA2 unionValues acc page
    case mLastKey of
      Nothing -> pure acc'
      Just lastKey
        | Map.size (getValuesMK (getLedgerTables acc')) > maxUtxoEntries ->
            throwIO . userError $
              "db-synthesizer: the UTxO holds more than "
                ++ show maxUtxoEntries
                ++ " entries. Raise the limit in Cardano.Tools.DBSynthesizer.TxGen."
        | otherwise -> go acc' (PreviousQueryWasUpTo lastKey)

  maxUtxoEntries = 100_000 :: Int
