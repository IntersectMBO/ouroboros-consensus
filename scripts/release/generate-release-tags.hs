#!/usr/bin/env cabal
{- cabal:
  build-depends:
    base,
    text,
    toml-reader,
    turtle,
  default-language: GHC2021
  default-extensions: BlockArguments, LambdaCase, OverloadedStrings, RecordWildCards
  ghc-options: -Wall -Wextra
-}

module Main where

import Data.Version
import Turtle
import TOML
import Data.Text qualified as Text

main :: IO ()
main = sh do

  chapCheckoutPath <- options "generate-release-tags.hs" do
    argPath "chap-checkout-path" "Path to a cardano-haskell-packages checkout"

  metaTomlPath <- find (contains "meta.toml") chapCheckoutPath

  CHaPEntry{..} <- liftIO (TOML.decodeFile metaTomlPath) >>= \case
    Left tomlError -> die $ renderTOMLError tomlError
    Right res -> pure res

  guard $ entryGitHubOwnerRepo == "input-output-hk/ouroboros-consensus"

  (packageName, packageVersion) <-
    case match packageNameVersionPattern (Text.pack metaTomlPath) of
      [] -> die $ format ("Couldn't parse package name and version for "%w) metaTomlPath
      res : _ -> pure res

  let tagName = format (s%"-"%s) packageName (Text.pack (showVersion packageVersion))

  printf ("Calling git tag "%s%" "%s%"\n") tagName entryGitHubRevision


  procs "git" ["tag", tagName, entryGitHubRevision] mempty

data CHaPEntry =
  CHaPEntry { entryGitHubOwnerRepo :: Text
            , entryGitHubRevision :: Text
            , entrySubdir :: Maybe Text
            }
  deriving (Show)

instance DecodeTOML CHaPEntry where
  tomlDecoder =
    CHaPEntry <$> getFields ["github", "repo"]
              <*> getFields ["github", "rev"]
              <*> optional (getField "subdir")

packageNameVersionPattern :: Pattern (Text, Version)
packageNameVersionPattern = do
  (,) <$> (chars *> "_sources/" *> packageNamePattern <* "/")
      <*> (packageVersionPattern <* "/meta.toml" <* eof)
  where
    packageNamePattern = plus (alphaNum <|> char '-')
    packageVersionPattern = Version <$> decimal `sepBy1` "." <*> pure []
