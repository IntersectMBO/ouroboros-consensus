{-# LANGUAGE OverloadedStrings #-}

module Test.Consensus.Node.Recovery (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Binary (DecoderError (DecoderErrorCustom))
import Control.Exception (SomeException, toException)
import qualified Data.ByteString.Short as SBS
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..))
import Ouroboros.Consensus.Node.Exit (ExitReason (DatabaseCorruption), toExitReason)
import Ouroboros.Consensus.Node.RethrowPolicy (consensusRethrowPolicy)
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDbFailure (ChainDbMissingEndorserBlock))
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ExnUndecodableEndorserBlock (..))
import Ouroboros.Network.RethrowPolicy
  ( ErrorCommand (ShutdownNode)
  , ErrorContext (..)
  , RethrowPolicy (..)
  )
import Test.Consensus.Leios.TestBlock (LeiosTestBlock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Leios storage failure policy"
    [ testGroup description $
        testCase
          "requires database recovery"
          ( case toExitReason proxy err of
              DatabaseCorruption -> pure ()
              _ -> assertFailure "Leios storage failure was not classified as database corruption"
          )
          : [ testCase ("shuts down the node for " <> show context) $
                case runRethrowPolicy (consensusRethrowPolicy proxy) context err of
                  ShutdownNode -> pure ()
                  _ -> assertFailure "Leios storage failure only disconnected the peer"
            | context <- [InboundError, OutboundError]
            ]
    | (description, err) <- failures
    ]
 where
  proxy = Proxy :: Proxy LeiosTestBlock
  point = MkLeiosPoint (SlotNo 1) (UnsafeHash (SBS.pack (replicate 32 0)))

  failures :: [(String, SomeException)]
  failures =
    [
      ( "missing endorser block"
      , toException (ChainDbMissingEndorserBlock point :: ChainDbFailure LeiosTestBlock)
      )
    ,
      ( "undecodable endorser block"
      , toException (ExnUndecodableEndorserBlock point (DecoderErrorCustom "Tx" "corrupt bytes"))
      )
    ]
