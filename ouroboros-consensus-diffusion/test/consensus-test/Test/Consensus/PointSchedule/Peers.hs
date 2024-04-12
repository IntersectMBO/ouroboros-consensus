{-# LANGUAGE DeriveAnyClass        #-}
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
  , enumerateAdversaries
  , fromMap
  , fromMap'
  , getPeer
  , getPeerIds
  , mkPeers
  , mkPeers'
  , peersFromPeerIdList
  , peersFromPeerIdList'
  , peersFromPeerList
  , peersList
  , peersOnlyHonest
  , toMap
  , toMap'
  , updatePeer
  ) where

import           Data.Hashable (Hashable)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString (fromString))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Util.Condense (Condense (..),
                     CondenseList (..), PaddingDirection (..),
                     condenseListWithPadding)

-- | Identifier used to index maps and specify which peer is active during a tick.
data PeerId =
  HonestPeer
  |
  PeerId String
  deriving (Eq, Generic, Show, Ord, NoThunks)

instance IsString PeerId where
  fromString "honest" = HonestPeer
  fromString i        = PeerId i

instance Condense PeerId where
  condense = \case
    HonestPeer -> "honest"
    PeerId name -> name

instance CondenseList PeerId where
  condenseList = condenseListWithPadding PadRight

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

instance CondenseList a => CondenseList (Peer a) where
  condenseList peers =
    zipWith
      (\name value -> name ++ ": " ++ value)
      (condenseList $ name <$> peers)
      (condenseList $ value <$> peers)

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

instance Foldable Peers where
  foldMap f Peers {honest, others} = (f . value) honest <> foldMap (f . value) others

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

getPeer :: PeerId -> Peers a -> Peer a
getPeer pid peers
  | HonestPeer <- pid
  = honest peers
  | otherwise
  = others peers Map.! pid

updatePeer :: (a -> (a, b)) -> PeerId -> Peers a -> (Peers a, b)
updatePeer f pid Peers {honest, others}
  | HonestPeer <- pid
  , let (a, b) = f (value honest)
  = (Peers {honest = a <$ honest, others}, b)
  | otherwise
  , let p = others Map.! pid
        (a, b) = f (value p)
  = (Peers {honest, others = Map.adjust (a <$) pid others}, b)

-- | Convert 'Peers' to a list of 'Peer'.
peersList :: Peers a -> NonEmpty (Peer a)
peersList Peers {honest, others} =
  honest :| Map.elems others

enumerateAdversaries :: [PeerId]
enumerateAdversaries =
  (\ n -> PeerId ("adversary " ++ show n)) <$> [1 :: Int ..]

-- | Construct 'Peers' from values, adding adversary names based on the default schema.
-- A single adversary gets the ID @adversary@, multiple get enumerated as @adversary N@.
mkPeers :: a -> [a] -> Peers a
mkPeers h as =
  Peers (Peer HonestPeer h) (Map.fromList (mkPeer <$> advs as))
  where
    mkPeer (pid, a) = (pid, Peer pid a)
    advs [a] = [("adversary", a)]
    advs _   = zip enumerateAdversaries as

-- | Make a 'Peers' structure from the honest value and the other peers. Fail if
-- one of the other peers is the 'HonestPeer'.
mkPeers' :: a -> [Peer a] -> Peers a
mkPeers' value prs =
    Peers (Peer HonestPeer value) (Map.fromList $ dupAdvPeerId <$> prs)
  where
    -- | Duplicate an adversarial peer id; fail if honest.
    dupAdvPeerId :: Peer a -> (PeerId, Peer a)
    dupAdvPeerId (Peer HonestPeer _) = error "cannot be the honest peer"
    dupAdvPeerId peer@(Peer pid _)   = (pid, peer)

-- | Make a 'Peers' structure from a non-empty list of peers. Fail if the honest
-- peer is not exactly once in the list.
peersFromPeerList :: NonEmpty (Peer a) -> Peers a
peersFromPeerList =
    uncurry mkPeers' . extractHonestPeer . NonEmpty.toList
  where
    -- | Return the value associated with the honest peer and the list of peers
    -- excluding the honest one.
    extractHonestPeer :: [Peer a] -> (a, [Peer a])
    extractHonestPeer [] = error "could not find honest peer"
    extractHonestPeer (Peer HonestPeer value : peers) = (value, peers)
    extractHonestPeer (peer : peers) = (peer :) <$> extractHonestPeer peers

-- | Make a 'Peers' structure from a non-empty list of peer ids and a default
-- value. Fails if the honest peer is not exactly once in the list.
peersFromPeerIdList :: NonEmpty PeerId -> a -> Peers a
peersFromPeerIdList = flip $ \val -> peersFromPeerList . fmap (flip Peer val)

-- | Like 'peersFromPeerIdList' with @()@.
peersFromPeerIdList' :: NonEmpty PeerId -> Peers ()
peersFromPeerIdList' = flip peersFromPeerIdList ()

toMap :: Peers a -> Map PeerId (Peer a)
toMap Peers{honest, others} = Map.insert HonestPeer honest others

-- | Same as 'toMap' but the map contains unwrapped values.
toMap' :: Peers a -> Map PeerId a
toMap' = fmap (\(Peer _ v) -> v) . toMap

fromMap :: Map PeerId (Peer a) -> Peers a
fromMap peers = Peers{
    honest = peers Map.! HonestPeer,
    others = Map.delete HonestPeer peers
  }

-- | Same as 'fromMap' but the map contains unwrapped values.
fromMap' :: Map PeerId a -> Peers a
fromMap' = fromMap . Map.mapWithKey Peer
