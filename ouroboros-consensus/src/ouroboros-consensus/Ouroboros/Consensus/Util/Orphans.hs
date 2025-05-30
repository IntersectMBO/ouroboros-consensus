{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Util.Orphans () where

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import Cardano.Crypto.Hash (Hash, SizeHash)
import Cardano.Ledger.Genesis (NoGenesis (..))
import Codec.CBOR.Decoding (Decoder)
import Codec.Serialise (Serialise (..))
import Control.Tracer (Tracer)
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.SOP.BasicFunctors
import GHC.TypeLits (KnownNat)
import NoThunks.Class
  ( InspectHeap (..)
  , InspectHeapNamed (..)
  , NoThunks (..)
  , OnlyCheckWhnfNamed (..)
  , allNoThunks
  )
import Ouroboros.Network.Util.ShowProxy
import System.FS.API (SomeHasFS)
import System.FS.API.Types (FsPath, Handle)
import System.FS.CRC (CRC (CRC))

{-------------------------------------------------------------------------------
  Serialise
-------------------------------------------------------------------------------}

instance KnownNat (SizeHash h) => Serialise (Hash h a)

instance Serialise (VerKeyDSIGN MockDSIGN) where
  encode = encodeVerKeyDSIGN
  decode = decodeVerKeyDSIGN

{-------------------------------------------------------------------------------
  NoThunks
-------------------------------------------------------------------------------}

instance NoThunks (NoGenesis era) where
  showTypeOf _ = "NoGenesis"
  wNoThunks _ NoGenesis = return Nothing

instance
  ( NoThunks p
  , NoThunks v
  , Ord p
  ) =>
  NoThunks (IntPSQ p v)
  where
  showTypeOf _ = "IntPSQ"
  wNoThunks ctxt =
    allNoThunks
      . concatMap
        ( \(k, p, v) ->
            [ noThunks ctxt k
            , noThunks ctxt p
            , noThunks ctxt v
            ]
        )
      . PSQ.toList

deriving via OnlyCheckWhnfNamed "Decoder" (Decoder s a) instance NoThunks (Decoder s a)

deriving via OnlyCheckWhnfNamed "Tracer" (Tracer m ev) instance NoThunks (Tracer m ev)

instance NoThunks a => NoThunks (K a b) where
  showTypeOf _ = showTypeOf (Proxy @a)
  wNoThunks ctxt (K a) = wNoThunks ("K" : ctxt) a

instance NoThunks a => NoThunks (MultiSet a) where
  showTypeOf _ = "MultiSet"
  wNoThunks ctxt = wNoThunks ctxt . MultiSet.toMap

{-------------------------------------------------------------------------------
  fs-api
-------------------------------------------------------------------------------}

deriving via InspectHeap FsPath instance NoThunks FsPath
deriving newtype instance NoThunks CRC
deriving via
  InspectHeapNamed "Handle" (Handle h)
  instance
    NoThunks (Handle h)
deriving via
  OnlyCheckWhnfNamed "SomeHasFS" (SomeHasFS m)
  instance
    NoThunks (SomeHasFS m)
