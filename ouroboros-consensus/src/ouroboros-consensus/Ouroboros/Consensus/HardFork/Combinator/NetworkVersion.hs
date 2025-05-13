{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Defines the different NTC and NTN versions for the HardFork Combinator.

module Ouroboros.Consensus.HardFork.Combinator.NetworkVersion (
    EraNodeToClientVersion (..)
  , HardForkNodeToClientVersion (..)
  , HardForkNodeToNodeVersion (..)
  , HardForkSpecificNodeToClientVersion (..)
  , HardForkSpecificNodeToNodeVersion (..)
  , isHardForkNodeToClientEnabled
  , isHardForkNodeToNodeEnabled
  ) where

import           Data.SOP.Constraint
import           Data.SOP.Strict
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Versioning
-------------------------------------------------------------------------------}

-- | Versioning of the specific additions made by the HFC to the @NodeToNode@
-- protocols, e.g., the era tag.
data HardForkSpecificNodeToNodeVersion =
    HardForkSpecificNodeToNodeVersion1
    -- | Represent GenTxId as an era-agnostic ShortByteStrings
  | HardForkSpecificNodeToNodeVersion2
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Versioning of the specific additions made by the HFC to the @NodeToClient@
-- protocols, e.g., the era tag or the hard-fork specific queries.
data HardForkSpecificNodeToClientVersion =
    -- | Include the Genesis window in 'EraParams'.
    HardForkSpecificNodeToClientVersion3
    -- | Represent GenTxId as an era-agnostic ShortByteString
  | HardForkSpecificNodeToClientVersion4
  deriving (Eq, Ord, Show, Enum, Bounded)

data HardForkNodeToNodeVersion xs where
  -- | Disable the HFC
  --
  -- This means that only the first era (@x@) is supported, and moreover, is
  -- compatible with serialisation used if the HFC would not be present at all.
  HardForkNodeToNodeDisabled ::
       BlockNodeToNodeVersion x
    -> HardForkNodeToNodeVersion (x ': xs)

  -- | Enable the HFC
  --
  -- Serialised values will always include tags inserted by the HFC to
  -- distinguish one era from another. We version the hard-fork specific parts
  -- with 'HardForkSpecificNodeToNodeVersion'.
  HardForkNodeToNodeEnabled ::
       HardForkSpecificNodeToNodeVersion
    -> NP WrapNodeToNodeVersion xs
    -> HardForkNodeToNodeVersion xs

data HardForkNodeToClientVersion xs where
  -- | Disable the HFC
  --
  -- See 'HardForkNodeToNodeDisabled'
  HardForkNodeToClientDisabled ::
       BlockNodeToClientVersion x
    -> HardForkNodeToClientVersion (x ': xs)

  -- | Enable the HFC
  --
  -- See 'HardForkNodeToNodeEnabled'
  HardForkNodeToClientEnabled ::
       HardForkSpecificNodeToClientVersion
    -> NP EraNodeToClientVersion xs
    -> HardForkNodeToClientVersion xs

data EraNodeToClientVersion blk =
    EraNodeToClientEnabled !(BlockNodeToClientVersion blk)
  | EraNodeToClientDisabled

deriving instance Show (BlockNodeToClientVersion blk) => Show (EraNodeToClientVersion blk)

deriving instance Eq (BlockNodeToClientVersion blk) => Eq (EraNodeToClientVersion blk)

deriving instance (All HasNetworkProtocolVersion xs, All (Compose Show WrapNodeToNodeVersion) xs) => Show (HardForkNodeToNodeVersion xs)
deriving instance (All HasNetworkProtocolVersion xs, All (Compose Show EraNodeToClientVersion) xs) => Show (HardForkNodeToClientVersion xs)

deriving instance (All HasNetworkProtocolVersion xs, All (Compose Eq WrapNodeToNodeVersion) xs) => Eq (HardForkNodeToNodeVersion xs)
deriving instance (All HasNetworkProtocolVersion xs, All (Compose Eq EraNodeToClientVersion) xs) => Eq (HardForkNodeToClientVersion xs)

instance ( All (Compose Show WrapNodeToNodeVersion) xs
         , All (Compose Eq WrapNodeToNodeVersion) xs
         , All (Compose Show EraNodeToClientVersion) xs
         , All (Compose Eq EraNodeToClientVersion) xs
         , All HasNetworkProtocolVersion xs)
    => HasNetworkProtocolVersion (HardForkBlock xs) where
  type BlockNodeToNodeVersion   (HardForkBlock xs) = HardForkNodeToNodeVersion   xs
  type BlockNodeToClientVersion (HardForkBlock xs) = HardForkNodeToClientVersion xs

isHardForkNodeToNodeEnabled :: HardForkNodeToNodeVersion xs -> Bool
isHardForkNodeToNodeEnabled HardForkNodeToNodeEnabled {} = True
isHardForkNodeToNodeEnabled _                            = False

isHardForkNodeToClientEnabled :: HardForkNodeToClientVersion xs -> Bool
isHardForkNodeToClientEnabled HardForkNodeToClientEnabled {} = True
isHardForkNodeToClientEnabled _                              = False
