{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
-- TODO: remove ScopedTypeVariables
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Orphans.NoThunks () where


import           Control.Concurrent.Class.MonadMVar.Strict.NoThunks
import           Control.Monad.IOSim
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Unsafe (unsafeSTToIO)
import           Data.Proxy
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm

instance NoThunks a => NoThunks (StrictSVar (IOSim s) a) where
  showTypeOf _ = "StrictSVar IOSim"
  wNoThunks ctxt StrictSVar { tvar } = do
      a <- unsafeSTToIO $ lazyToStrictST $ inspectTVar (Proxy :: Proxy (IOSim s)) tvar
      noThunks ctxt a

-- TODO: we need to be able to inspect the value inside the mvar a la MonadInspectSTM.
instance NoThunks a => NoThunks (StrictMVar (IOSim s) a) where
  showTypeOf _ = "StrictMVar IOSim"
  wNoThunks ctxt _v = do
      a <- undefined :: IO a -- TODO
      noThunks ctxt a
