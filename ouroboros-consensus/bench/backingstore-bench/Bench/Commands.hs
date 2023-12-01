{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Bench.Commands (
    -- * Command types
    Cmd (..)
  , VHID
    -- * Aux types
  , BackingStoreInitialiser
    -- * Running commands in a concrete monad
  , run
  ) where

import           Cardano.Slotting.Slot (SlotNo, WithOrigin)
import           Control.DeepSeq
import           Control.Monad (void)
import           Control.Monad.Class.MonadThrow (MonadThrow)
import           Control.Monad.Reader (MonadReader (ask), MonadTrans (..),
                     ReaderT (..))
import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                     gets, modify)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore
                     (BackingStore, BackingStoreValueHandle, InitFrom (..),
                     RangeQuery)
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as BS
import           System.FS.API (SomeHasFS)
import           System.FS.API.Types (FsPath)

{-------------------------------------------------------------------------------
  Command types
-------------------------------------------------------------------------------}

data Cmd ks vs d =
    BSInitFromValues !(WithOrigin SlotNo) !vs
  | BSInitFromCopy   !FsPath
  | BSClose
  | BSCopy           !FsPath
  | BSValueHandle    !VHID
  | BSWrite          !SlotNo !d
  | BSVHClose        !VHID
  | BSVHRangeRead    !VHID !(RangeQuery ks)
  | BSVHRead         !VHID !ks
  | BSRead           !ks
  deriving Show

-- | Identifiers for value handles
type VHID = Int

instance NFData (Cmd ks vs d) where rnf = rwhnf

{-------------------------------------------------------------------------------
  Aux types
-------------------------------------------------------------------------------}

type BackingStoreInitialiser m ks vs d =
     SomeHasFS m
  -> InitFrom vs
  -> m (BackingStore m ks vs d)

{-------------------------------------------------------------------------------
  Running commands in a concrete monad
-------------------------------------------------------------------------------}

run ::
     forall m ks vs d. MonadThrow m
  => SomeHasFS m
  -> BackingStoreInitialiser m ks vs d
  -> [Cmd ks vs d] -> m ()
run shfs bsi cmds = evalStateT (runReaderT (runM m) initialEnv) initialState
  where
    m :: M ks vs d m ()
    m = runCmds cmds

    initialEnv = Env {
          envSomeHasFS               = shfs
        , envBackingStoreInitialiser = bsi
        }

    initialState = St {
        stLookUp       = mempty
      , stBackingStore = Nothing
      }

-- | Concrete monad 'M' to run commands in.
--
-- 'M' is a newtype because 'runCmds' and 'runCmd' require a single transformer
-- in its type: @t m ()@. Compare this with @'ReaderT' r ('StateT' s m) a@,
-- which has two transfomers on top of @m@, while @M@ itself is just a single
-- transformer.
newtype M ks vs d m a = M {
    runM :: ReaderT (Env m ks vs d) (StateT (St m ks vs d) m) a
  }
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader (Env m ks vs d), MonadState (St m ks vs d))

instance MonadTrans (M ks vs d) where
  lift :: Monad m => m a -> M ks vs d m a
  lift = M . lift . lift

{-------------------------------------------------------------------------------
  Running commands
-------------------------------------------------------------------------------}

-- | State to keep track of while running commands.
data St m ks vs d = St {
    -- | Backing stores have no built-in notion of value handle management, so
    -- we have to keep track of them somewhere. Running a command that
    -- references a value handle by their 'VHID' should use this mapping to look
    -- up the corresponding value handle.
    stLookUp       :: !(Map VHID (BackingStoreValueHandle m ks vs))
    -- | The backing store that is currently in use.
    --
    -- This is a 'Maybe', because when starting to run a list of commands, there
    -- is initially no backing store. After an initialisation command like
    -- 'BSInitFromValues' and 'BSInitFromCopy', this field should never be
    -- 'Nothing'.
  , stBackingStore :: !(Maybe (BackingStore m ks vs d))
  }

-- | Reader environment to pass around while running commands.
data Env m ks vs d = Env {
    -- | Access to the file system (simulated or real) is required for
    -- initialising backing store, and making copies of a backing store.
    envSomeHasFS               :: !(SomeHasFS m)
    -- | A way to initialise a new backing store. A new backing store can be
    -- initialised even when one already exists.
  , envBackingStoreInitialiser :: !(BackingStoreInitialiser m ks vs d)
  }

runCmds ::
     forall m t ks vs d. (
       MonadReader (Env m ks vs d) (t m)
     , MonadState (St m ks vs d) (t m)
     , MonadTrans t
     , MonadThrow m
     )
  => [Cmd ks vs d]
  -> t m ()
runCmds = mapM_ runCmd

runCmd ::
     ( MonadReader (Env m ks vs d) (t m)
     , MonadState (St m ks vs d) (t m)
     , MonadTrans t
     , MonadThrow m
     )
  => Cmd ks vs d
  -> t m ()
runCmd = \case
    BSInitFromValues sl vs -> bsInitFromValues sl vs
    BSInitFromCopy bsp     -> bsInitFromCopy bsp
    BSClose                -> bsClose
    BSCopy bsp             -> bsCopy bsp
    BSValueHandle i        -> bsValueHandle i
    BSWrite sl d           -> bsWrite sl d
    BSVHClose i            -> bsvhClose i
    BSVHRangeRead i rq     -> bsvhRangeRead i rq
    BSVHRead i ks          -> bsvhRead i ks
    BSRead ks              -> bsRead ks
  where
    bsInitFromValues sl vs = do
        Env shfs bsi <- ask
        bs' <- lift $ bsi shfs (InitFromValues sl vs)
        modify (\st -> st {
            stBackingStore = Just bs'
          })

    bsInitFromCopy bsp = do
        Env shfs bsi <- ask
        bs' <- lift $ bsi shfs (InitFromCopy bsp)
        modify (\st -> st {
            stBackingStore = Just bs'
          })

    bsClose = do
        bs <- fromJust <$> gets stBackingStore
        lift $ BS.bsClose bs

    bsCopy bsp = do
        bs <- fromJust <$> gets stBackingStore
        lift $ BS.bsCopy bs bsp

    bsValueHandle i = do
        bs <- fromJust <$> gets stBackingStore
        vh <- lift $ BS.bsValueHandle bs
        let f vhMay = case vhMay of
                        Nothing -> Just vh
                        Just _  -> error "bsValueHandle"
        modify (\st -> st {
            stLookUp = Map.alter f i $ stLookUp st
          })

    bsWrite sl d = do
        bs <- fromJust <$> gets stBackingStore
        lift $ BS.bsWrite bs sl d

    bsvhClose i = do
        vh <- gets (fromJust . Map.lookup i . stLookUp)
        lift $ BS.bsvhClose vh

    bsvhRangeRead i rq =  do
        vh <- gets (fromJust . Map.lookup i . stLookUp)
        void $ lift $ BS.bsvhRangeRead vh rq

    bsvhRead i ks = do
        vh <- gets (fromJust . Map.lookup i . stLookUp)
        void $ lift $ BS.bsvhRead vh ks

    bsRead ks = do
        bs <- fromJust <$> gets stBackingStore
        void $ lift $ BS.bsRead bs ks
