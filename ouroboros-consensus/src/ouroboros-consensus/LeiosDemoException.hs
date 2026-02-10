module LeiosDemoException (LeiosDbException (..)) where

import Control.Monad.Class.MonadThrow (Exception)

data LeiosDbException = LeiosDbException
  { leiosDbErrorMessage :: String
  , leiosDbErrorCallStack :: String
  }
  deriving (Show)

instance Exception LeiosDbException
