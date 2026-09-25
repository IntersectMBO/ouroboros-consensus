module Cardano.Tools.DBSynthesizer.Types (module Cardano.Tools.DBSynthesizer.Types) where

import Data.Word (Word64)
import Ouroboros.Consensus.Block.Abstract (SlotNo)

data ForgeLimit
  = ForgeLimitBlock !Word64
  | ForgeLimitSlot !SlotNo
  | ForgeLimitEpoch !Word64
  deriving (Eq, Show)

newtype ForgeResult = ForgeResult {resultForged :: Int}
  deriving (Eq, Show)

data DBSynthesizerOpenMode
  = OpenCreate
  | OpenCreateForce
  | OpenAppend
  deriving (Eq, Show)

data DBSynthesizerOptions = DBSynthesizerOptions
  { synthLimit :: !ForgeLimit
  , synthOpenMode :: !DBSynthesizerOpenMode
  }
  deriving Show
