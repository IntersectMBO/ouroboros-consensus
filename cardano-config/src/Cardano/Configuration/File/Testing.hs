-- | Values related to testing, which are unused by a real node
module Cardano.Configuration.File.Testing
  ( TestingConfiguration (..)
  ) where

import Cardano.Configuration.File.Protocol
import Data.Aeson
import Data.Word
import GHC.Generics (Generic)

-- | The testing configuration: knobs for forcing era transitions at specific
-- epochs/versions and for enabling the experimental era.
data TestingConfiguration = TestingConfiguration
  { experimentalHardForksEnabled :: Bool
  , testShelleyHardForkAtEpoch :: Maybe Word64
  , testShelleyHardForkAtVersion :: Maybe Word
  , testAllegraHardForkAtEpoch :: Maybe Word64
  , testAllegraHardForkAtVersion :: Maybe Word
  , testMaryHardForkAtEpoch :: Maybe Word64
  , testMaryHardForkAtVersion :: Maybe Word
  , testAlonzoHardForkAtEpoch :: Maybe Word64
  , testAlonzoHardForkAtVersion :: Maybe Word
  , testBabbageHardForkAtEpoch :: Maybe Word64
  , testBabbageHardForkAtVersion :: Maybe Word
  , testConwayHardForkAtEpoch :: Maybe Word64
  , testConwayHardForkAtVersion :: Maybe Word
  , testDijkstraHardForkAtEpoch :: Maybe Word64
  , testDijkstraHardForkAtVersion :: Maybe Word
  , experimentalGenesis :: Maybe (Hashed FilePath)
  }
  deriving (Generic, Show)

instance FromJSON TestingConfiguration where
  parseJSON =
    withObject "Configuration" $ \v -> do
      -- The experimental era's hard-fork knobs and genesis only apply when the
      -- experimental eras are enabled, matching the node.
      enabled <- v .:? "ExperimentalHardForksEnabled" .!= False
      TestingConfiguration enabled
        <$> v .:? "TestShelleyHardForkAtEpoch"
        <*> v .:? "TestShelleyHardForkAtVersion"
        <*> v .:? "TestAllegraHardForkAtEpoch"
        <*> v .:? "TestAllegraHardForkAtVersion"
        <*> v .:? "TestMaryHardForkAtEpoch"
        <*> v .:? "TestMaryHardForkAtVersion"
        <*> v .:? "TestAlonzoHardForkAtEpoch"
        <*> v .:? "TestAlonzoHardForkAtVersion"
        <*> v .:? "TestBabbageHardForkAtEpoch"
        <*> v .:? "TestBabbageHardForkAtVersion"
        <*> v .:? "TestConwayHardForkAtEpoch"
        <*> v .:? "TestConwayHardForkAtVersion"
        <*> (if enabled then v .:? "TestDijkstraHardForkAtEpoch" else pure Nothing)
        <*> (if enabled then v .:? "TestDijkstraHardForkAtVersion" else pure Nothing)
        <*> (if enabled then Just <$> parseEraGenesis "Dijkstra" v else pure Nothing)
