{-# LANGUAGE EmptyDataDeriving #-}

-- TODO DUPLICATE? -- as-if adapted? from: cardano-node/src/Cardano/Node/Protocol/Conway.hs

module Cardano.Node.Protocol.Conway (
    ConwayProtocolInstantiationError
    -- * Reusable parts
  , readGenesis
  , validateGenesis
  ) where

import qualified Cardano.Ledger.Conway.Genesis as Conway
import           Cardano.Node.Protocol.Shelley (GenesisReadError,
                     readGenesisAny)
import           Cardano.Node.Types
import           Cardano.Prelude

--
-- Conway genesis
--

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Conway.ConwayGenesis, GenesisHash)
readGenesis = readGenesisAny

validateGenesis :: Conway.ConwayGenesis
                -> ExceptT ConwayProtocolInstantiationError IO ()
validateGenesis _ = return () --TODO conway: do the validation

data ConwayProtocolInstantiationError
{- TODO
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | ConwayCostModelFileError !(FileError ())
  | ConwayCostModelDecodeError !FilePath !String
-}
  deriving Show

{- TODO
instance Error ConwayProtocolInstantiationError where
  displayError (InvalidCostModelError fp) =
    "Invalid cost model: " <> show fp
  displayError (CostModelExtractionError fp) =
    "Error extracting the cost model at: " <> show fp
  displayError (ConwayCostModelFileError err) =
    displayError err
  displayError (ConwayCostModelDecodeError fp err) =
    "Error decoding cost model at: " <> show fp <> " Error: " <> err

-}
