module Ouroboros.Consensus.Node.StartupWarning
  ( StartupWarning (..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Ouroboros.Consensus.Config.SecurityParam

data StartupWarning
  = InconsistentSecurityParam (NonEmpty SecurityParam)
  deriving (Show, Eq)
