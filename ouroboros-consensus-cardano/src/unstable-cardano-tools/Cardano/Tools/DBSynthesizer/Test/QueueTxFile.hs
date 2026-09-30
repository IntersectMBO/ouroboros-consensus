{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Write the transaction file that
-- 'Cardano.Tools.DBSynthesizer.TxGen.File.mkFileTxGen' replays.
--
-- The @tools-test@ suite is the only consumer. It sits in this library rather
-- than beside that suite because it uses 'Cardano.Api.KeysShelley' and
-- 'Cardano.Api.SerialiseTextEnvelope', which are private to this library.
--
-- It keeps the outputs the payment key owns in a queue, takes two off the head
-- for each transaction and appends the two that transaction makes to the tail.
-- It needs neither the forker nor the ticked ledger state, because the queue is
-- closed over itself: the outputs it starts from are the genesis @initialFunds@, whose
-- inputs 'initialFundsPseudoTxIn' derives from the address alone, and every
-- later output is one this tool made and can name.
--
-- It does not need to know where blocks begin or end either. With @N@ outputs
-- in the queue, transaction @k@ spends what transaction @k - N\/2@ made, so no
-- window of fewer than @N\/2@ consecutive transactions holds a transaction that
-- spends an output another transaction of that window made. Wherever the reader
-- puts the block boundaries, the transactions of a block are independent, so
-- long as a ranking block and the endorser block it announces take fewer than
-- @N\/2@ between them. This tool reports that budget.
--
-- The one thing it cannot work out for itself is the era, because a
-- transaction's bytes are era-specific. It writes Dijkstra transactions, which
-- is what the fixture 'Cardano.Tools.DBSynthesizer.Test.QueueFixture' writes runs
-- in. A chain that crosses a hard fork mid-file would need this to follow it.
module Cardano.Tools.DBSynthesizer.Test.QueueTxFile
  ( writeQueueTxs
  ) where

import Cardano.Api.KeysShelley (SigningKey (PaymentSigningKey))
import Cardano.Crypto.DSIGN (SignKeyDSIGN)
import Cardano.Ledger.Api
  ( Addr
  , EraTx
  , Tx
  , bodyTxL
  , feeTxBodyL
  , inputsTxBodyL
  , mkBasicTx
  , mkBasicTxBody
  , mkBasicTxOut
  , outputsTxBodyL
  )
import Cardano.Ledger.Api.Tx.In (TxIn (TxIn))
import Cardano.Ledger.BaseTypes (TxIx (TxIx))
import Cardano.Ledger.Binary.Plain (serialize')
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Core (TopTx, txIdTx)
import qualified Cardano.Ledger.Keys as LK
import Cardano.Ledger.Shelley.Genesis (initialFundsPseudoTxIn, sgInitialFunds)
import Cardano.Ledger.Val (inject)
import Cardano.Tools.DBSynthesizer.TxGen (ownsAddr, readPaymentSigningKey)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR.Write
import Control.DeepSeq (NFData, force)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.List (sortOn)
import qualified Data.ListMap as ListMap
import Data.Sequence.Strict (StrictSeq ((:<|)), (|>))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((.~))
-- The 'ShelleyCompatible' instance that gives a Dijkstra transaction its
-- encoding. The slimmed 'Cardano.Tools.DBSynthesizer.TxGen' no longer brings
-- it in, so name it here rather than rely on another module's imports.

import Ouroboros.Consensus.Shelley.Eras (DijkstraEra)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis)
import System.Exit (die)
import System.IO (IOMode (WriteMode), withBinaryFile)
import Test.ThreadNet.Infra.Shelley (signTx)

-- | Write a stream of @count@ transactions to the file.
--
-- The stream is one CBOR indefinite-length array -- @9f@, the transactions,
-- @ff@ -- which is the shape
-- 'Cardano.Tools.DBSynthesizer.TxGen.File.mkFileTxGen' reads. Each transaction
-- is the ledger's own encoding of it, with no envelope: the same bytes the
-- LeiosDb stores. The break at the end says the stream is finished, which is
-- what tells a reader that ran out of transactions that none are coming.
writeQueueTxs :: FilePath -> FilePath -> FilePath -> Int -> IO ()
writeQueueTxs genesisPath keyPath outPath count = do
  PaymentSigningKey signKey <-
    either die pure =<< readPaymentSigningKey keyPath
  genesis <-
    either (die . ((genesisPath ++ ": ") ++)) pure
      =<< Aeson.eitherDecodeFileStrict' @ShelleyGenesis genesisPath

  let queue = initialQueue signKey genesis
      held = StrictSeq.length queue
  when (held < 2) $
    die $
      "writeQueueTxs: the payment signing key owns "
        ++ show held
        ++ " of the outputs in "
        ++ genesisPath
        ++ ", and a transaction spends two."

  withBinaryFile outPath WriteMode $ \h -> do
    BS.hPut h (frame CBOR.encodeListLenIndef)
    mapM_ (BS.hPut h . serialize') (take count (queueTxs signKey queue))
    BS.hPut h (frame CBOR.encodeBreak)

  putStrLn $
    "wrote "
      ++ show count
      ++ " transactions to "
      ++ outPath
      ++ "; the queue holds "
      ++ show held
      ++ " outputs, so a ranking block and the endorser block it announces may"
      ++ " take up to "
      ++ show (held `div` 2 - 1)
      ++ " transactions between them"
 where
  frame = CBOR.Write.toStrictByteString

-- | The outputs the genesis gives the key, oldest first.
--
-- Ordered by input, which is the order the ledger's UTxO map has them in.
initialQueue :: SignKeyDSIGN LK.DSIGN -> ShelleyGenesis -> StrictSeq Entry
initialQueue signKey genesis =
  StrictSeq.fromList
    . sortOn entryIn
    $ [ Entry{entryIn = initialFundsPseudoTxIn addr, entryAddr = addr, entryCoin = coin}
      | (addr, coin) <- ListMap.toList (sgInitialFunds genesis)
      , ownsAddr signKey addr
      ]

-- | The transactions the queue makes, for as long as it is asked for them.
--
-- Two entries off the head make one transaction, whose two outputs go on the
-- tail. The queue therefore keeps its length, and the addresses and values in
-- it are the ones the genesis set up, for as long as this runs.
queueTxs :: SignKeyDSIGN LK.DSIGN -> StrictSeq Entry -> [Tx TopTx DijkstraEra]
queueTxs signKey = go
 where
  go queue = case queue of
    in1 :<| in2 :<| rest ->
      let (tx, out1, out2) = pairTx signKey in1 in2
       in tx : go (rest |> out1 |> out2)
    _ -> []

-- | One unspent output that the queue holds.
--
-- The entry keeps the address and the value rather than the ledger's @TxOut@,
-- because @TxOut@ is indexed by the era and the queue outlives a hard fork: the
-- era is fixed per slot, by the ticked state of the block being forged, while
-- the queue is one value for the whole run. 'TxIn', 'Addr' and 'Coin' are the
-- same types in every era, and they are all a transaction needs — the input
-- side names the entry, and the output side is @mkBasicTxOut addr (inject
-- coin)@, rebuilt at whatever era the slot turns out to be in.
data Entry = Entry
  { entryIn :: !TxIn
  , entryAddr :: !Addr
  , entryCoin :: !Coin
  }
  deriving (Generic, NFData)

-- | Build the one shape of transaction this generator makes: two inputs, two
-- outputs, no fee.
--
-- Output @i@ carries the address and the value of input @i@, so the two entries
-- it returns hold what the two it took held. With no fee the values are exact,
-- and the queue's addresses and values never change over a run.
pairTx ::
  EraTx era =>
  SignKeyDSIGN LK.DSIGN ->
  Entry ->
  Entry ->
  (Tx TopTx era, Entry, Entry)
pairTx signKey in1 in2 =
  -- The forge loop runs NoThunks over the state it buffers.
  force (tx, made 0 in1, made 1 in2)
 where
  txOutOf e = mkBasicTxOut (entryAddr e) (inject (entryCoin e))
  tx =
    mkBasicTx mkBasicTxBody
      & bodyTxL . inputsTxBodyL .~ Set.fromList [entryIn in1, entryIn in2]
      & bodyTxL . outputsTxBodyL .~ StrictSeq.fromList [txOutOf in1, txOutOf in2]
      & bodyTxL . feeTxBodyL .~ Coin 0
      & signTx signKey
  made ix e = e{entryIn = TxIn (txIdTx tx) (TxIx ix)}
