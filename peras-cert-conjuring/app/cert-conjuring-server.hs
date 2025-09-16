module Main (main) where

import CertConjuring (mockUpdateServerState, initServerState, runServer)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (void)

main :: IO ()
main = do
  st <- newMVar initServerState
  void $ forkIO (mockUpdateServerState st)
  runServer st
