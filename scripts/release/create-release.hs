#!/usr/bin/env cabal 
{- project:
  with-compiler: ghc-9.4
-}
{- cabal:
  build-depends:
    base,
    commonmark,
    filepath,
    foldl,
    text,
    turtle ^>=1.6.0,
  default-language: GHC2021
-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

import Commonmark
import Commonmark.Types
import Control.Monad
import System.FilePath
import Turtle
import Data.Semigroup
import qualified Data.Text.IO as Text
import qualified Control.Foldl as Foldl

main :: IO ()
main = sh $ do
  echo "alright, let's make some releases"
  packageName <- select packageNames
  liftIO $ print packageName
  maxMaybe <- reduce Foldl.maximum $ do
    fragment <- findChangelogFragments packageName
    guard $ takeExtension fragment == ".md"
    findChangeSeverity fragment
  liftIO $ print maxMaybe

packageNames :: [FilePath]
packageNames =
  [ "ouroboros-consensus"
  , "ouroboros-consensus-cardano"
  , "ouroboros-consensus-diffusion"
  , "ouroboros-consensus-protocol"
  ]

findChangelogFragments :: FilePath -> Shell FilePath
findChangelogFragments pkg = do
  ls $ pkg </> "changelog.d"

data ChangeSeverity
  = Patch
  | NonBreaking
  | Breaking
  deriving (Show, Eq, Ord, Bounded)

newtype HeadingText = HeadingText Text
  deriving (Show, Read, Eq, Ord, Semigroup, Monoid)

instance Rangeable HeadingText where
  ranged _ = id

instance HasAttributes HeadingText where
  addAttributes _ = id

instance IsInline HeadingText where
  lineBreak = HeadingText "\n"
  softBreak = HeadingText "\n"
  str = HeadingText
  entity = HeadingText
  escapedChar = mempty
  emph = id
  strong = id
  link _dest txt _desc = HeadingText txt
  image = mempty
  code = HeadingText
  rawInline = mempty

newtype Headings = Headings (Maybe (Max ChangeSeverity))
  deriving (Show, Eq, Ord, Semigroup, Monoid)

instance Rangeable Headings where
  ranged _ = id

instance HasAttributes Headings where
  addAttributes _ = id

instance IsBlock HeadingText Headings where
  heading _lvl (HeadingText txt) =
    case txt of
      "Patch" -> Headings (Just (Max Patch))
      "Non-Breaking" -> Headings (Just (Max NonBreaking))
      "Breaking" -> Headings (Just (Max Breaking))
      _ -> mempty
  paragraph = mempty
  plain = mempty
  thematicBreak = mempty
  blockQuote = mempty
  codeBlock = mempty
  rawBlock = mempty
  referenceLinkDefinition = mempty
  list = mempty

findChangeSeverity :: FilePath -> Shell ChangeSeverity
findChangeSeverity frag = do
  contents <- liftIO $ Text.readFile frag
  case commonmark frag contents of
    Left err -> error $ show err
    Right (Headings Nothing) ->
      error $ "Couldn't find any change severity headers in " <> frag <> ", exiting!"
    Right (Headings (Just (Max sev))) -> pure sev
