{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Orphans.NoThunks () where


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
