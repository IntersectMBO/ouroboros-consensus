{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.Basics
  ( FilePath (..)
  , (</>)
  , RelativeFilePath (..)
  , anchorRelativePath
  , DefaultGiven (..)
  , Override (..)
  , (.:=)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.String (IsString)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import qualified System.FilePath as F
import Prelude hiding (FilePath)
import qualified Prelude as P

newtype FilePath (s :: Symbol) = FilePath {getFilePath :: P.FilePath}
  deriving (Generic, Show)
  deriving newtype (FromJSON, IsString)

(</>) :: FilePath a -> FilePath b -> FilePath c
FilePath a </> FilePath b = FilePath (a F.</> b)

newtype RelativeFilePath (s :: Symbol)
  = RelativeFilePath {relativeFilePath :: FilePath s}
  deriving (Generic, Show)

anchorRelativePath :: FilePath a -> RelativeFilePath b -> FilePath b
anchorRelativePath fp1 (RelativeFilePath fp2) = fp1 </> fp2

class DefaultGiven given a where
  defGiven :: given -> a

data Override a = NoOverride | Override a deriving (Generic, Show, Eq)

instance Default (Override a) where
  def = NoOverride

instance FromJSON a => FromJSON (Override a) where
  parseJSON v =
    withText
      "Override"
      ( \case
          "NoOverride" -> pure NoOverride
          _ -> fail "Not text"
      )
      v
      <|> Override <$> parseJSON v

(.:=) :: (FromJSON a, Default a) => Object -> Key -> Parser a
a .:= b = a .:? b .!= def
