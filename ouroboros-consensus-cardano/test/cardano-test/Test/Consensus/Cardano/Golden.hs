{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Cardano.Golden (tests) where

import Cardano.Crypto.DSIGN (genKeyDSIGN, seedSizeDSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))
import Data.Word (Word8)
import LeiosDemoTypes
  ( EbHash (..)
  , LeiosDSIGN
  , LeiosPoint (..)
  , LeiosSigningKey
  , LeiosVote
  , VoterId (MkVoterId)
  , encodeLeiosVote
  , signLeiosVote
  )
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.HardFork.Combinator.Serialisation
import Ouroboros.Consensus.Ledger.Query (QueryVersion)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import System.FilePath ((</>))
import Test.Consensus.Cardano.Examples
import Test.Tasty
import Test.Util.Paths
import Test.Util.Serialisation.CDDL
import Test.Util.Serialisation.Golden

tests :: TestTree
tests =
  testGroup
    "Cardano"
    [ goldenTest_all
        codecConfig
        ($(getGoldenDir) </> "cardano")
        ( Just $
            CDDLsForNodeToNode
              ("ntnblock.cddl", "serialisedCardanoBlock")
              ("ntnheader.cddl", "header")
              ("ntntx.cddl", "tx")
              ("ntntxid.cddl", "txId")
        )
        examples
    , goldenTestCBOR
        "LeiosVote"
        typicalVote
        encodeLeiosVote
        ($(getGoldenDir) </> "cardano" </> "leios" </> "LeiosVote")
        (Just ("leiosnotify.cddl", "vote"))
    ]

-- | A typical 'LeiosVote' at voter index 1000, signed with a deterministic
-- key. Pinned by the corresponding golden file.
typicalVote :: LeiosVote
typicalVote =
  signLeiosVote (mkLeiosSigningKey 0x42) (MkVoterId 1000) point
 where
  point = MkLeiosPoint (SlotNo 42) (MkEbHash (BS.pack [0 .. 31]))

-- | Deterministic signing key, seeded by repeating a single byte.
mkLeiosSigningKey :: Word8 -> LeiosSigningKey
mkLeiosSigningKey b =
  genKeyDSIGN $ mkSeedFromBytes $ BS.replicate sz b
 where
  sz = fromIntegral (seedSizeDSIGN (Proxy @LeiosDSIGN))

instance
  CardanoHardForkConstraints c =>
  ToGoldenDirectory (HardForkNodeToNodeVersion (CardanoEras c))
  where
  toGoldenDirectory v = case v of
    CardanoNodeToNodeVersion1 -> "CardanoNodeToNodeVersion1"
    CardanoNodeToNodeVersion2 -> "CardanoNodeToNodeVersion2"
    _ -> error $ "Unknown version: " <> show v

instance
  CardanoHardForkConstraints c =>
  ToGoldenDirectory (QueryVersion, HardForkNodeToClientVersion (CardanoEras c))
  where
  toGoldenDirectory (queryVersion, blockVersion) =
    show queryVersion </> case blockVersion of
      CardanoNodeToClientVersion12 -> "CardanoNodeToClientVersion12"
      CardanoNodeToClientVersion13 -> "CardanoNodeToClientVersion13"
      CardanoNodeToClientVersion14 -> "CardanoNodeToClientVersion14"
      CardanoNodeToClientVersion15 -> "CardanoNodeToClientVersion15"
      CardanoNodeToClientVersion16 -> "CardanoNodeToClientVersion16"
      CardanoNodeToClientVersion17 -> "CardanoNodeToClientVersion17"
      CardanoNodeToClientVersion18 -> "CardanoNodeToClientVersion18"
      CardanoNodeToClientVersion19 -> "CardanoNodeToClientVersion19"
      _ -> error $ "Unknown version: " <> show blockVersion
