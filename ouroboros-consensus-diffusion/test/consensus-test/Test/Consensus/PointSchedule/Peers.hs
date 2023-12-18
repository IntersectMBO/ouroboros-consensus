{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | This module contains the definition of point schedule _peers_ as well as
-- all kind of utilities to manipulate them.

module Test.Consensus.PointSchedule.Peers (
    Peer (..)
  , PeerId (..)
  , Peers (..)
  , getPeerIds
  , mkPeers
  , peersList
  , peersOnlyHonest
  ) where

import           Data.Hashable (Hashable)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString (fromString))
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))

-- | Identifier used to index maps and specify which peer is active during a tick.
data PeerId =
  HonestPeer
  |
  PeerId String
  deriving (Eq, Generic, Show, Ord)

instance IsString PeerId where
  fromString "honest" = HonestPeer
  fromString i        = PeerId i

instance Condense PeerId where
  condense = \case
    HonestPeer -> "honest"
    PeerId name -> name

instance Hashable PeerId

-- | General-purpose functor associated with a peer.
data Peer a =
  Peer {
    name  :: PeerId,
    value :: a
  }
  deriving (Eq, Show)

instance Functor Peer where
  fmap f Peer {name, value} = Peer {name, value = f value}

instance Foldable Peer where
  foldr step z (Peer _ a) = step a z

instance Traversable Peer where
  sequenceA (Peer name fa) =
    Peer name <$> fa

instance Condense a => Condense (Peer a) where
  condense Peer {name, value} = condense name ++ ": " ++ condense value

-- | General-purpose functor for a set of peers.
--
-- REVIEW: There is a duplicate entry for the honest peer, here. We should
-- probably either have only the 'Map' or have the keys of the map be 'String'?
--
-- Alternatively, we could just have 'newtype PeerId = PeerId String' with an
-- alias for 'HonestPeer = PeerId "honest"'?
data Peers a =
  Peers {
    honest :: Peer a,
    others :: Map PeerId (Peer a)
  }
  deriving (Eq, Show)

instance Functor Peers where
  fmap f Peers {honest, others} = Peers {honest = f <$> honest, others = fmap f <$> others}

-- | A set of peers with only one honest peer carrying the given value.
peersOnlyHonest :: a -> Peers a
peersOnlyHonest value =
  Peers {
    honest = Peer {name = HonestPeer, value},
    others = Map.empty
    }

-- | Extract all 'PeerId's.
getPeerIds :: Peers a -> NonEmpty PeerId
getPeerIds peers = HonestPeer :| Map.keys (others peers)

-- | Convert 'Peers' to a list of 'Peer'.
peersList :: Peers a -> NonEmpty (Peer a)
peersList Peers {honest, others} =
  honest :| Map.elems others

-- | Construct 'Peers' from values, adding adversary names based on the default schema.
-- A single adversary gets the ID @adversary@, multiple get enumerated as @adversary N@.
mkPeers :: a -> [a] -> Peers a
mkPeers h as =
  Peers (Peer HonestPeer h) (Map.fromList (mkPeer <$> advs as))
  where
    mkPeer (pid, a) = (pid, Peer pid a)
    advs [a] = [("adversary", a)]
    advs _   = zip enumAdvs as
    enumAdvs = (\ n -> PeerId ("adversary " ++ show n)) <$> [1 :: Int ..]
