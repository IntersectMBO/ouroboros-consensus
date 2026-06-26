{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Cardano.Golden (tests) where

import qualified Data.ByteString as BS
import LeiosDemoTypes
  ( LeiosVote
  , RbHash (..)
  , VoterId (..)
  , encodeLeiosVote
  , signLeiosVote
  )
import Test.Cardano.Crypto.Leios.Gen (genLeiosSigningKey, generateWith)
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
  signLeiosVote (generateWith genLeiosSigningKey 42) (VoterId 1000) rbHash
 where
  rbHash = MkRbHash (BS.pack [0 .. 31])

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
