{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module contains peer state management and error policies.
--
module Ouroboros.Network.Subscription.PeerState
  ( SuspendDecision (..)
  , suspend
  -- * PeerStates and its operations
  , PeerState (..)
  , threadsToCancel
  , PeerStates (..)
  , newPeerStatesVar
  , cleanPeerStates
  , runSuspendDecision
  , registerConsumer
  , unregisterConsumer
  , registerProducer
  , unregisterProducer
  , BeforeConnect
  , ConnectDecision (..)
  , runBeforeConnect
  , beforeConnectTx

  -- * Re-exports
  , DiffTime

  -- * Auxiliary functions
  , alterAndLookup
  ) where

import           Control.Exception (Exception, SomeException (..), assert)
import           Control.Monad.State
import           Data.Functor ((<$))
import           Data.Map.Strict (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable ( (:~:) (..)
                               , eqT
                               , Proxy (..)
                               )

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Data.Semigroup.Action

-- | Semigroup of commands which acts on 'PeerState'.  The @t@ variable might
-- be initiated to 'DiffTime' or @Time m@.
--
-- This semigroup allows to either suspend both consumer and producer or just
-- the consumer part.
--
data SuspendDecision t
    = SuspendPeer !t !t
    -- ^ peer is suspend; The first @t@ is the time until which a local
    -- producer is suspended, the second one is the time until which a local
    -- consumer is suspended.
    | SuspendConsumer !t
    -- ^ suspend local consumer \/ initiator side until @t@ (this mean we are
    -- not allowing to communicate with the producer \/ responder of a remote
    -- peer).
    | Throw
    -- ^ throw an error from the main thread.
    deriving (Eq, Ord, Show, Functor)

consumerSuspendedUntil :: SuspendDecision t -> Maybe t
consumerSuspendedUntil (SuspendPeer _ consT)   = Just consT
consumerSuspendedUntil (SuspendConsumer consT) = Just consT
consumerSuspendedUntil Throw                   = Nothing

producerSuspendedUntil :: SuspendDecision t -> Maybe t
producerSuspendedUntil (SuspendPeer prodT _) = Just prodT
producerSuspendedUntil (SuspendConsumer _) = Nothing
producerSuspendedUntil Throw               = Nothing

-- | The semigroup instance.  Note that composing 'SuspendPeer' with
-- 'SuspendConsumer' gives 'SuspendPeer'.  'SuspendPeer' and 'SuspendConsumer'
-- form a sub-semigroup.
--
instance Ord t => Semigroup (SuspendDecision t) where
    Throw <> _ = Throw
    _ <> Throw = Throw
    SuspendPeer prodT consT <> SuspendPeer prodT' consT'
      = SuspendPeer (prodT `max` prodT') (consT `max` consT')
    SuspendConsumer consT <> SuspendPeer prodT consT'
      = SuspendPeer prodT (consT `max` consT')
    SuspendPeer prodT consT <> SuspendConsumer consT'
      = SuspendPeer prodT (consT `max` consT')
    SuspendConsumer consT <> SuspendConsumer consT'
      = SuspendConsumer (consT `max` consT')


data PeerState m
  = HotPeer !(Set (Async m ())) !(Set (Async m ()))
  -- ^ active peer with its producers and consumer threads
  | SuspendedConsumer !(Set (Async m ())) !Time
  -- ^ suspended consumer: with producer threads and time until the consumer is
  -- suspended
  | SuspendedPeer !Time !Time
  -- ^ suspended peer: producer & consumer suspend time
  | ColdPeer
  -- ^ peer with no opened connections in either direction

instance ( MonadAsync m
         , Show (ThreadId m)
         , Ord (ThreadId m)
         )  => Show (PeerState m) where
    show (HotPeer producers consumers)
       = "HotPeer"
      ++ " "
      ++ show (Set.map (asyncThreadId (Proxy :: Proxy m)) producers)
      ++ " "
      ++ show (Set.map (asyncThreadId (Proxy :: Proxy m)) consumers)
    show (SuspendedConsumer producers consT)
       = "SuspendedConsumer"
      ++ " "
      ++ show (Set.map (asyncThreadId (Proxy :: Proxy m)) producers)
      ++ " "
      ++ show consT
    show (SuspendedPeer prodT consT)
       = "SuspendedPeer"
      ++ " "
      ++ show prodT
      ++ " "
      ++ show consT
    show ColdPeer = "ColdPeer"

deriving instance Eq (Async m ()) => Eq (PeerState m)

deriving instance Ord (Async m ()) => Ord (PeerState m)

-- | Action of 'SuspendDecision' on @Maybe 'PeerState'@.  We use this action
-- together with 'Map.alter' function.
--
-- Note: 'SuspendDecision' does not act on 'PeerState', only the sub-semigroup
-- generated by 'SuspendConsumer' and 'SuspendPeer' does.
--
--
instance SAct (SuspendDecision Time) (Maybe (PeerState m)) where

    -- this means we will remove the entry from the state map; this is fine
    -- since we are about to throw an exception to kill a node.
    _ <| Throw   = Nothing
    Nothing <| _ = Nothing

    -- this might apply when a connection to a 'ColdPeer' thrown an
    -- exception.
    (Just ColdPeer) <| (SuspendConsumer consT)
      = Just $ SuspendedConsumer Set.empty consT
    (Just ColdPeer) <| (SuspendPeer prodT consT)
      = Just (SuspendedPeer prodT consT)

    (Just (HotPeer producers _consumers)) <| (SuspendConsumer consT)
      = Just $ SuspendedConsumer producers consT
    (Just (HotPeer _prodcuers _consumers)) <| (SuspendPeer prodT consT)
      = Just $ SuspendedPeer prodT consT

    (Just (SuspendedConsumer producers consT)) <| (SuspendConsumer consT')
      = Just $ SuspendedConsumer producers (consT `max` consT')
    (Just (SuspendedConsumer _producers consT)) <| (SuspendPeer prodT consT')
      = Just $ SuspendedPeer prodT (consT `max` consT')

    (Just (SuspendedPeer prodT consT)) <| cmd
      = case producerSuspendedUntil cmd of
          Nothing ->
            Just $ SuspendedPeer
                    prodT
                    (maybe consT (consT `max`) $ consumerSuspendedUntil cmd)
          Just prodT' ->
            Just $ SuspendedPeer
                    (prodT `max` prodT')
                    (maybe consT (consT `max`) $ consumerSuspendedUntil cmd)

-- | Threads which needs to be cancelled when updating the 'PeerState' with
-- 'SuspendDecision'.
--
threadsToCancel :: Ord (Async m ())
                => PeerState m
                -> SuspendDecision diffTime
                -> Set (Async m ())
threadsToCancel _ Throw
    = Set.empty
threadsToCancel ColdPeer _
    = Set.empty
threadsToCancel (HotPeer _producers consumers) SuspendConsumer{}
    = consumers
threadsToCancel (HotPeer consumers producers) SuspendPeer{}
    = consumers <> producers
threadsToCancel SuspendedConsumer{} SuspendConsumer{}
    = Set.empty
threadsToCancel (SuspendedConsumer producers _consT) SuspendPeer{}
    = producers
threadsToCancel SuspendedPeer{} _cmd
    = Set.empty


-- | Action of 'SuspendDecision' on @Maybe 'PeerState'@.  Action laws are only
-- satisfied for the submonoid form by 'SuspendPeer' and 'SuspendConsumer'.
--
suspend :: Ord (Async m ())
        => Maybe (PeerState m)
        -> SuspendDecision Time
        -> ( Set (Async m ())
           , Maybe (PeerState m)
           )
suspend mbps cmd = ( maybe Set.empty (`threadsToCancel` cmd) mbps
                   , mbps <| cmd
                   )


-- | Map from addresses to 'PeerState's; it will be be shared in a 'StrictTVar'.
--
-- Abstracting @t@ is useful for tests, the @IO@ version will use @Time IO@.
--
data PeerStates m addr where
     -- | Map of peer states
     PeerStates :: !(Map addr (PeerState m))
                -> PeerStates m addr

     -- | Or an exception to throw
     ThrowException :: Exception e
                    => e
                    -> PeerStates m addr

instance Show addr
         => Show (PeerStates IO addr) where
    show (PeerStates ps)    = "PeerStates "     ++ show ps
    show (ThrowException e) = "ThrowException " ++ show e

-- TODO: move to Test.PeerStates as eqPeerStates
instance Eq addr
         => Eq (PeerStates IO addr) where
    ThrowException (_ :: e) == ThrowException (_ :: e') =
      case eqT :: Maybe (e :~: e') of
        Nothing   -> False
        Just Refl -> True
    PeerStates ps == PeerStates ps' = ps == ps'
    _ == _ = False


newPeerStatesVar :: MonadSTM m => m (StrictTVar m (PeerStates m addr))
newPeerStatesVar = newTVarM (PeerStates Map.empty)


-- | Periodically clean 'PeerState'.  It will stop when 'PeerState' becomes
-- 'ThrowException'.
--
cleanPeerStates :: ( MonadSTM   m
                   , MonadAsync m
                   , MonadTime  m
                   , MonadTimer m
                   )
                => DiffTime
                -> StrictTVar m (PeerStates m addr)
                -> m ()
cleanPeerStates interval v = go
  where
    go = do
      threadDelay interval
      t <- getMonotonicTime
      continue <- atomically $ do
        s <- readTVar v
        case s of
          ThrowException _
            -> pure False
          PeerStates ps
            -> True <$ (writeTVar v $! (PeerStates $ Map.mapMaybe (cleanPeerState t) ps))

      if continue
        then go
        else pure ()


    cleanPeerState :: Time -> PeerState m -> Maybe (PeerState m)
    cleanPeerState _t ColdPeer{}   = Nothing
    cleanPeerState _  ps@HotPeer{} = Just ps
    cleanPeerState  t ps@(SuspendedConsumer producers consT)
      |      Set.null producers  && consT >= t
      -- the consumer is not suspended anymore, but there is no producer thread
      -- running, we can safely remove the peer from 'PeerStates'
      = Nothing

      | not (Set.null producers) && consT >= t
      -- the consumer is not suspended anymore, there are running producer
      -- threads, and thus return a 'HotPeer'.
      = Just (HotPeer producers Set.empty)

      | otherwise
      -- otherwise the consumer is still supsended
      = Just ps

    cleanPeerState  t ps@(SuspendedPeer prodT consT)
      | prodT < t
      -- the producer is still suspended
      = Just ps

      | consT < t
      -- only the consumer is still not suspended
      = Just (SuspendedConsumer Set.empty consT)

      | otherwise
      -- the peer is not suspended any more
      = Nothing



-- | Update 'PeerStates' for a given 'addr', using 'suspend', and return
-- threads which must be cancelled.
--
-- This is more efficient that using the action of 'SuspendDecision' on
-- 'PeerStates', since it only uses a single dictionary lookup to update the
-- state and return the set of threads to be cancelled.
--
runSuspendDecision
    :: forall m addr e.
       ( Monad m
       , Ord addr
       , Ord (Async m ())
       , Exception e
       )
    => Time
    -> addr
    -> e
    -> SuspendDecision DiffTime
    -> PeerStates m addr
    -> ( PeerStates m addr
       , Set (Async m ())
       )
runSuspendDecision _t _addr _e _cmd ps0@ThrowException{} =
    ( ps0
    , Set.empty
    )
runSuspendDecision _t _addr  e  Throw _ =
    ( ThrowException (SomeException e)
    , Set.empty
    )
runSuspendDecision  t  addr _e  cmd (PeerStates ps0) =
    gn $ alterAndLookup fn addr ps0
  where
    fn :: Maybe (PeerState m)
       -> ( Set (Async m ())
          , Maybe (PeerState m)
          )
    fn mbps = ( maybe Set.empty (`threadsToCancel` cmd) mbps
              , mbps <| (flip addTime t <$> cmd)
              )

    gn :: ( Map addr (PeerState m)
          , Maybe (Set (Async m ()))
          )
       -> ( PeerStates m addr
          , Set (Async m ())
          )
    gn (ps, Nothing) = (PeerStates ps, Set.empty)
    gn (ps, Just s)  = (PeerStates ps, s)



-- Using pure 'State' monad and 'alterF' to avoid searching the 'PeerState'
-- twice.
alterAndLookup
    :: forall k s a.
       Ord k
    => (Maybe a -> (s, Maybe a))
    -> k
    -> Map k a
    -> ( Map k a
       , Maybe s
       )
alterAndLookup f k m = runState (Map.alterF g k m) Nothing
  where
    g :: Maybe a -> State (Maybe s) (Maybe a)
    g mba = case f mba of
      (s, mba') -> mba' <$ modify' (const (Just s))


--
-- Various callbacks
--


-- | Register producer in PeerStates.  This is a partial function which assumes
-- that the 'PeerState' is either 'HotPeer' or 'SuspendedConsumer'.
--
registerProducer :: forall m addr.
                    ( Ord addr
                    , Ord (Async m ())
                    )
                 => addr
                 -> Async m ()
                 -> PeerStates m addr
                 -> PeerStates m addr
registerProducer _addr _tid ps@ThrowException{} = ps
registerProducer addr  tid  (PeerStates peerStates) =
    PeerStates $ Map.alter fn addr peerStates
  where
    fn :: Maybe (PeerState m) -> Maybe (PeerState m)
    fn Nothing =
        Just (HotPeer (Set.singleton tid) Set.empty)
    fn (Just (HotPeer producers consumers)) =
        Just (HotPeer (tid `Set.insert` producers) consumers)
    fn (Just ColdPeer) =
        Just (HotPeer (Set.singleton tid) Set.empty)
    fn (Just (SuspendedConsumer producers consT)) =
        Just (SuspendedConsumer (tid `Set.insert` producers) consT)
    fn (Just ps@SuspendedPeer{}) =
        -- registerProducer on a suspended peer
        assert False $ Just ps

unregisterProducer :: forall m addr.
                      ( Ord addr
                      , Ord (Async m ())
                      )
                   => addr
                   -> Async m ()
                   -> PeerStates m addr
                   -> PeerStates m addr
unregisterProducer _addr _tid ps@ThrowException{} = ps
unregisterProducer addr  tid  (PeerStates peerStates) =
    PeerStates $ Map.alter fn addr peerStates
  where
    fn :: Maybe (PeerState m) -> Maybe (PeerState m)
    fn Nothing = Nothing
    fn (Just (HotPeer producers consumers)) =
        let producers' = tid `Set.delete` producers
        in if Set.null producers' && Set.null consumers
             then Nothing
             else Just (HotPeer producers' consumers)
    fn (Just ColdPeer) = Nothing
    fn (Just p@SuspendedPeer{}) = Just p
    fn (Just (SuspendedConsumer producers consT)) =
        Just (SuspendedConsumer (tid `Set.delete` producers) consT)


-- | Register consumer in 'PeerState'.  This is a partial function which
-- assumes that the 'PeerState' is 'HotPeer'.
--
registerConsumer :: forall m addr.
                    ( Ord addr
                    , Ord (Async m ())
                    )
                 => addr
                 -> Async m ()
                 -> PeerStates m addr
                 -> PeerStates m addr
registerConsumer _addr _tid ps@ThrowException{} = ps
registerConsumer addr  tid  (PeerStates peerStates) =
    PeerStates $ Map.alter fn addr peerStates
  where
    fn :: Maybe (PeerState m) -> Maybe (PeerState m)
    fn  Nothing =
        Just (HotPeer Set.empty (Set.singleton tid))
    fn  (Just (HotPeer producers consumers)) =
        Just (HotPeer producers (tid `Set.insert` consumers))
    fn (Just ColdPeer) =
        Just (HotPeer Set.empty (Set.singleton tid))
    fn (Just ps) =
        -- registerConsumer on a suspended peer
        assert False $ Just ps


-- | Unregister consumer from a 'PeerState'.
--
unregisterConsumer :: forall m addr.
                      ( Ord addr
                      , Ord (Async m ())
                      )
                   => addr
                   -> Async m ()
                   -> PeerStates m addr
                   -> PeerStates m addr
unregisterConsumer _addr _tid ps@ThrowException{} = ps
unregisterConsumer addr  tid (PeerStates peerStates) =
    PeerStates $ Map.alter fn addr peerStates
  where
    fn :: Maybe (PeerState m) -> Maybe (PeerState m)
    fn Nothing = Nothing
    fn (Just (HotPeer producers consumers)) =
      let consumers' = tid `Set.delete` consumers
      in if Set.null producers && Set.null consumers'
           then Nothing
           else Just (HotPeer producers consumers')
    fn (Just ColdPeer) = Nothing
    fn (Just ps) = Just ps


-- | Before connectin with a peer we make a decision to either connect to it or
-- not.
--
data ConnectDecision s
    = AllowConnection !s
    | DisallowConnection !s
  deriving Functor

-- | Check state before connecting to a remote peer.  We will connect only if
-- it retuns 'True'.
--
type BeforeConnect m s addr = Time -> addr -> s -> STM m (ConnectDecision s)

-- | Run 'BeforeConnect' callback in a 'MonadTime' monad.
--
runBeforeConnect :: ( MonadSTM  m
                    , MonadTime m
                    )
                 => StrictTVar m s
                 -> BeforeConnect m s addr
                 -> addr
                 -> m Bool
runBeforeConnect sVar beforeConnect addr = do
    t <- getMonotonicTime
    atomically $ do
      d <- readTVar sVar >>= beforeConnect t addr
      case d of
        AllowConnection s    -> True  <$ writeTVar sVar s
        DisallowConnection s -> False <$ writeTVar sVar s


-- | 'BeforeConnect' callback: it updates peer state and return boolean value
-- wheather to connect to it or not.  If a peer hasn't been recorded in
-- 'PeerStates', we add it and try to connect to it.
--
beforeConnectTx
  :: forall m addr.
     ( MonadSTM m
     , Ord addr
     )
  => BeforeConnect m
      (PeerStates m addr)
      addr

beforeConnectTx _t _addr ps@ThrowException{} = pure $ DisallowConnection ps

beforeConnectTx  t  addr (PeerStates s) =
    case alterAndLookup fn addr s of
      (s', mbd) -> case mbd of
        Nothing -> pure $ DisallowConnection (PeerStates s')
        Just d  -> pure (PeerStates s' <$ d)
  where
    fn :: Maybe (PeerState m)
       -> ( ConnectDecision ()
          , Maybe (PeerState m)
          )

    -- we see the peer for the first time; consider it as a good peer and
    -- try to connect to it.
    fn Nothing = ( AllowConnection ()
                 , Just ColdPeer
                 )

    fn (Just p@ColdPeer{}) = ( AllowConnection ()
                             , Just p
                             )

    fn (Just p@(HotPeer producers consumers))
      = if Set.null producers && Set.null consumers
        -- the peer has no registered producers nor consumers, thus it should
        -- be marked as a 'ColdPeer'
        then ( AllowConnection ()
             , Just ColdPeer
             )
        else ( AllowConnection ()
             , Just p
             )

    fn (Just p@(SuspendedConsumer producers consT)) =
      if consT < t
        then if Set.null producers
               -- the consumer is not suspended any longer, and it has no
               -- producers; thus it's a 'ColdPeer'.
               then (AllowConnection (), Just ColdPeer)
               else (AllowConnection (), Just (HotPeer producers Set.empty))
        else (DisallowConnection (), Just p)

    fn (Just p@(SuspendedPeer prodT consT)) =
      if t < prodT `max` consT
          then if t < prodT `min` consT
                 then (DisallowConnection (), Just p)
                 else if prodT < consT
                   then -- prodT ≤ t < consT
                        -- allow the remote peer to connect to us, but we're
                        -- still not allowed to connect to it.
                        (DisallowConnection (), Just $ SuspendedConsumer Set.empty consT)
                   else -- consT ≤ t < prodT
                        -- the local consumer is suspended shorter than local
                        -- producer; In this case we suspend both until `prodT`.
                        -- This means we effectively make local consumer
                        -- errors more sevier than the ones which come from
                        -- a local producer.
                        (DisallowConnection (), Just $ SuspendedPeer prodT prodT)

          -- the peer is not suspended any longer
          else (AllowConnection (), Just ColdPeer)
