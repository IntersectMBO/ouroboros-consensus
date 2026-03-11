{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LeiosDemoException (module LeiosDemoException) where

import Control.Monad.Class.MonadThrow (Exception)
import Data.Aeson (KeyValue, ToJSON (..), Value (..), pairs, (.=))

data LeiosDbException = LeiosDbException
  { errorMessage :: String
  , callStack :: String
  }
  deriving Show

instance Exception LeiosDbException

instance ToJSON LeiosDbException where
  toJSON = Object . jsonLeiosDbException
  toEncoding = pairs . jsonLeiosDbException

jsonLeiosDbException :: (KeyValue a kv, Monoid kv) => LeiosDbException -> kv
jsonLeiosDbException e =
  mconcat
    [ "kind" .= String "LeiosDbException"
    , "errorMessage" .= e.errorMessage
    , "callStack" .= e.callStack
    ]
