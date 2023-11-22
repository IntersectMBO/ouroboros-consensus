module Test.Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.JSONRoundtrip (test) where

import           Data.Aeson as Aeson
import           Test.Tasty.QuickCheck (Property, (===))

test :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Property
test a = Aeson.eitherDecode (Aeson.encode a) === Right a
