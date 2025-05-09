{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.Serialise () where

import Codec.Serialise (Serialise)
import Ouroboros.Network.SizeInBytes

deriving newtype instance Serialise SizeInBytes
