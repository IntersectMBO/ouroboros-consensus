module Ouroboros.Consensus.Node.SanityCheck
  ( SanityCheckError (..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Ouroboros.Consensus.Config.SecurityParam

class ConfigSupportsSanityCheck

data SanityCheckError
  = InconsistentSecurityParam (NonEmpty SecurityParam)
  deriving (Show, Eq)
