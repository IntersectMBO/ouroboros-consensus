{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Build the node configuration that @db-synthesizer --tx-generator file@
-- needs, out of the one the @tools-test@ fixture already holds.
--
-- The @tools-test@ suite is the only consumer. It sits in this library rather
-- than beside that suite because it uses 'Cardano.Api.KeysShelley' and
-- 'Cardano.Api.SerialiseTextEnvelope', which are private to this library.
--
-- The file generator, and the queue that 'writeQueueTxs' runs to write its
-- file, want more of a genesis than the respend generator does, and none of it
-- can be written by hand:
--
-- * __Many outputs under one key.__ @initialFunds@ maps an address to a coin
--   and the ledger derives the input from the address, so one address gives one
--   output. This writes @UTXO-COUNT@ base addresses that share the payment
--   credential of the key it generates and differ only in the staking part.
--   'Cardano.Tools.DBSynthesizer.TxGen.ownsAddr' looks at the payment
--   credential alone, so that one key owns all of them and signs every
--   transaction.
--
-- * __A voting key.__ The endorser block a block announces reaches the ledger
--   only once a later block certifies it, which needs a pool holding a
--   committee seat with a registered @blsKey@ whose proof of possession
--   verifies. This generates the key, registers it on a pool, and writes the
--   signing key for @--shelley-bls-key@.
--
-- * __Leios turned on.__ The fixture's Dijkstra genesis zeroes every Leios
--   parameter, so no endorser block holds a transaction and no committee is
--   seated. This gives the endorser block a capacity and seats the committee,
--   and leaves the announcement, vote and diffusion periods at zero so that the
--   block after an announcement may already certify it -- the alternation the
--   file generator requires.
--
-- The keys come from fixed seeds, so the fixture is the same on every run.
module Cardano.Tools.DBSynthesizer.Test.QueueFixture
  ( writeQueueFixture
  ) where

import Cardano.Api.KeysShelley (SigningKey (PaymentSigningKey))
import Cardano.Api.SerialiseTextEnvelope (serialiseToTextEnvelope)
import Cardano.Crypto.DSIGN
  ( BLS12381MinSigDSIGN
  , DSIGNAlgorithm
  , SignKeyDSIGN
  , createPossessionProofDSIGN
  , deriveVerKeyDSIGN
  , genKeyDSIGN
  , seedSizeDSIGN
  )
import Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import Cardano.Ledger.Address (serialiseAddr)
import qualified Cardano.Ledger.Keys as LK
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.State (BlsKey (BlsKey))
import Cardano.Tools.DBSynthesizer.BlsKey (BlsSigningKey (BlsSigningKey))
import Data.Aeson (Value (Number, Object))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath ((</>))
import Test.ThreadNet.Infra.Shelley (mkCredential, networkId)

-- | What the generated outputs hold between them. Split evenly, this is the
-- amount the fixture's payment key held in one output before.
paymentFunds :: Integer
paymentFunds = 9_000_000_000_000

-- | Write the configuration directory. The source directory is the fixture
-- the tools test already uses; the count is how many outputs the payment key
-- should own.
writeQueueFixture :: FilePath -> FilePath -> Int -> IO ()
writeQueueFixture srcDir outDir count = do
  createDirectoryIfMissing True outDir

  -- Keys. The payment key owns every generated output; the BLS key votes.
  let paySk :: SignKeyDSIGN LK.DSIGN
      paySk = genKeyDSIGN (seedFor @LK.DSIGN "queue-fixture-payment-key")

      blsSk :: SignKeyDSIGN BLS12381MinSigDSIGN
      blsSk = genKeyDSIGN (seedFor @BLS12381MinSigDSIGN "queue-fixture-bls-key")

  writeJson (outDir </> "payment.skey") $
    serialiseToTextEnvelope Nothing (PaymentSigningKey paySk)
  writeJson (outDir </> "bls.skey") $
    serialiseToTextEnvelope Nothing (BlsSigningKey blsSk)

  -- The addresses. One payment credential, one staking credential per output,
  -- so the ledger's pseudo-input differs for each and the key owns them all.
  let addrs =
        [ addrHex $
            SL.Addr
              networkId
              (mkCredential paySk)
              (SL.StakeRefBase (mkCredential (stakeKeyFor i)))
        | i <- [1 .. count]
        ]
      perAddr = paymentFunds `div` fromIntegral count

  shelley <- readJsonObject (srcDir </> "shelley-genesis.json")
  dijkstra <- readJsonObject (srcDir </> "dijkstra-genesis.json")
  config <- readJsonObject (srcDir </> "config.json")

  writeJson (outDir </> "shelley-genesis.json") . Object $
    KeyMap.insert "initialFunds" (fundsOf addrs perAddr (shelley KeyMap.!? "initialFunds")) $
      KeyMap.insert "staking" (withBlsKey blsSk (shelley KeyMap.!? "staking")) shelley

  writeJson (outDir </> "dijkstra-genesis.json") . Object $
    KeyMap.union leiosParams dijkstra

  -- Conway and Dijkstra have no Test*HardForkAtEpoch in the fixture, so they
  -- trigger on a protocol version the chain never reaches. Leios lives in
  -- Dijkstra, so the run has to start there.
  writeJson (outDir </> "config.json") . Object $
    KeyMap.union
      (KeyMap.fromList [("TestConwayHardForkAtEpoch", Number 0), ("TestDijkstraHardForkAtEpoch", Number 0)])
      config

  mapM_
    (\f -> copyFile (srcDir </> f) (outDir </> f))
    ["byron-genesis.json", "alonzo-genesis.json", "conway-genesis.json", "bulk-creds-k2.json"]

  putStrLn $
    "wrote "
      ++ outDir
      ++ ": "
      ++ show count
      ++ " outputs of "
      ++ show perAddr
      ++ " lovelace each"

-- | The Leios parameters the fixture zeroes.
--
-- The three period lengths stay at zero, so 'minCertificationGap' is zero
-- slots and the block after an announcement may certify it. The execution unit
-- and reference script limits stay at zero too: the generated transactions
-- carry no scripts.
leiosParams :: KeyMap.KeyMap Value
leiosParams =
  KeyMap.fromList
    [ -- Room for the endorser block's transactions: one block body's worth.
      ("maxEndorserBlockTxsSize", Number 81920)
    , -- Bounds how many transactions an endorser block may reference.
      ("maxEndorserBlockReferencesSize", Number 100000)
    , -- Seat both of the fixture's pools. Only the one below carries our key;
      -- the other is seated keyless and never votes.
      ("leiosCommitteeSize", Number 2)
    , -- Any vote that carries certifies. One forger holds one seat, so a real
      -- quorum would never be reached.
      ("leiosQuorumStakeThreshold", Number 0)
    ]

-- | Register the BLS verification key, with its proof of possession, on the
-- first pool of the genesis staking section.
withBlsKey :: SignKeyDSIGN BLS12381MinSigDSIGN -> Maybe Value -> Value
withBlsKey blsSk = \case
  Just (Object staking)
    | Just (Object pools) <- staking KeyMap.!? "pools"
    , (poolId, Object params) : _ <- KeyMap.toAscList pools ->
        Object $
          KeyMap.insert
            "pools"
            ( Object $
                KeyMap.insert
                  poolId
                  (Object (KeyMap.insert "blsKey" blsKeyJson params))
                  pools
            )
            staking
  other -> error $ "writeQueueFixture: unexpected staking section: " ++ show other
 where
  blsKeyJson =
    Aeson.toJSON $
      BlsKey (deriveVerKeyDSIGN blsSk) (createPossessionProofDSIGN blsSk)

-- | The generated addresses, plus the two the fixture already had.
--
-- Those two are base addresses that carry the pools' stake, so dropping them
-- would leave no pool with stake and no slot with a leader. The third entry the
-- fixture holds is the enterprise address of a payment key it does not ship;
-- this drops it, and tells the two apart by length, a base address being 28
-- bytes longer than an enterprise one.
fundsOf :: [Text.Text] -> Integer -> Maybe Value -> Value
fundsOf addrs perAddr existing =
  Object $
    KeyMap.union
      (KeyMap.fromList [(Key.fromText a, Aeson.toJSON perAddr) | a <- addrs])
      (KeyMap.filterWithKey (\k _ -> Text.length (Key.toText k) == baseAddrHexLength) stakeFunds)
 where
  -- A base address is a header byte and two 28-byte key hashes, where an
  -- enterprise address is the header and one; the keys here are hex, which
  -- doubles both.
  baseAddrHexLength = 2 * (1 + 28 + 28)

  stakeFunds = case existing of
    Just (Object o) -> o
    other -> error $ "writeQueueFixture: unexpected initialFunds: " ++ show other

addrHex :: SL.Addr -> Text.Text
addrHex = Text.decodeLatin1 . B16.encode . serialiseAddr

stakeKeyFor :: Int -> SignKeyDSIGN LK.DSIGN
stakeKeyFor i = genKeyDSIGN (seedFor @LK.DSIGN ("queue-fixture-stake-" ++ show i))

-- | A seed of the size the algorithm wants, filled from the label.
seedFor :: forall v. DSIGNAlgorithm v => String -> Seed
seedFor label =
  mkSeedFromBytes
    . BS.take (fromIntegral (seedSizeDSIGN (Proxy @v)))
    . BS.concat
    . replicate 64
    $ BS8.pack label

readJsonObject :: FilePath -> IO (KeyMap.KeyMap Value)
readJsonObject path =
  Aeson.eitherDecodeFileStrict' path >>= \case
    Right (Object o) -> pure o
    Right other -> die $ path ++ ": expected a JSON object, got " ++ show other
    Left err -> die $ path ++ ": " ++ err

-- | Keys sorted and one value per line, so the fixture reads and diffs like the
-- one it is generated from.
writeJson :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeJson path =
  BSL.writeFile path
    . Pretty.encodePretty' Pretty.defConfig{Pretty.confCompare = compare}
