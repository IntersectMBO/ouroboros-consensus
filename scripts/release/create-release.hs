#!/usr/bin/env cabal
{- cabal:
  build-depends:
    base,
    ansi-wl-pprint ^>=1.0,
    commonmark,
    containers,
    filepath,
    foldl,
    prettyprinter,
    text,
    turtle ^>=1.6.0,
    with-utf8,
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Wextra #-}

module Main where

import           Commonmark
import qualified Control.Foldl as Foldl
import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Monoid (First (..))
import           Data.Semigroup (Max (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version
import           Main.Utf8 (withStdTerminalHandles)
import           Prettyprinter
import           System.FilePath
import           Turtle hiding (d, fp, l, o)

main :: IO ()
main = withStdTerminalHandles $ sh do

  (isDryRun, skipGit) <- options helpDescription $
    (,) <$> switch "dry-run" 'd' "Make no changes"
        <*> switch "skip-git" 's' "Skip creating a new release branch or commits for these changes"

  -- Determine how breaking the sum of the changes in each package is
  packageChangeSeverities <- reduce collectSeverities do

    (packageName, dependencies) <- select packages

    maxMaybe <- reduce Foldl.maximum do
      fragment <- findChangelogFragments packageName
      findChangeSeverity fragment

    pure (packageName, maxMaybe, dependencies)

  packageVersions <- reduce Foldl.list do
    (packageName, _) <- select packages

    case join $ Map.lookup packageName packageChangeSeverities of
      Nothing -> do
        liftIO do
          putStrLn $ "No changes need to be made for package " <> packageName <> "!"
        mzero
      Just severity -> do
        currentPackageVersion <- findCurrentPackageVersion packageName
        let nextPackageVersion = calculateNextPackageVersion severity currentPackageVersion

        pure (packageName, (currentPackageVersion, nextPackageVersion))

  liftIO $ forM_ packageVersions $ \(package, (current, next)) -> do
    putStrLn $ package <> ": " <> showVersion current <> " -> " <> showVersion next

  unless isDryRun do
    unless skipGit do
      createGitBranch (Map.fromList packageVersions)

  (packageName, (current, next)) <- select $ Map.toList (Map.fromList packageVersions)

  if isDryRun
    then
      liftIO do
        putStrLn $ "This is a dry run, so no changes will be made for " <> packageName
    else do
      runScrivCollect packageName next
      updateCabalFile packageName current next (Map.fromList packageVersions)
      unless skipGit do
        createGitCommit packageName next

-- | Pairs of packages with a list of their dependencies. We use the
--   dependencies to calculate which version changes should require a version
--   bump in a package's dependencies as well.
--
--   BEWARE! This list should always be ordered such that any given package's
--   dependencies are located BEFORE that package in the list!
packages :: [(FilePath, [FilePath])]
packages =
  [ ("sop-extras", [])
  , ("ouroboros-consensus", ["sop-extras"])
  , ("ouroboros-consensus-diffusion", ["sop-extras", "ouroboros-consensus"])
  , ("ouroboros-consensus-protocol", ["ouroboros-consensus"])
  , ("ouroboros-consensus-cardano", ["sop-extras", "ouroboros-consensus", "ouroboros-consensus-protocol"])
  ]

helpDescription :: Description
helpDescription = Description $ vcat
  [ "Create releases for ouroboros-consensus packages"
  , line
  , nest 2 do
    mconcat
      [ "This script does (broadly) six things:"
      , line
      , vcat
          [ "1. Parses any Markdown (.md) files in the changelog.d directory "
          , "for each of the packages to determine the severity of the changes "
          , "made to that package since the last release, and accordingly "
          , "calculate the new version number for that package;"
          , line
          , "2. Checks the new versions of each of the dependencies of each "
          , "package to determine whether those dependency version bumps "
          , "require a new version of the package (even if its changes are of "
          , "lower severity;"
          , line
          , "3. Creates a new git branch for the current release;"
          , line
          , "4. For each package, updates its .cabal package description to "
          , "incorporate both the new version and the new versions of each of "
          , "the dependencies;"
          , line
          , "5. Runs `scriv collect` for each of the packages, updating their "
          , "CHANGELOG.md files;"
          , line
          , "6. Creates a git commit for each of the package releases."
          ]
      ]
  ]

findChangelogFragments :: FilePath -> Shell FilePath
findChangelogFragments pkg = do
  changeLogEntry <- ls $ pkg </> "changelog.d"
  guard $ takeExtension changeLogEntry == ".md"
  pure changeLogEntry

-- | Find a package's version by parsing its cabal file (only by searching for
--   the "version:" field, not by invoking the cabal library itself)
findCurrentPackageVersion :: FilePath -> Shell Version
findCurrentPackageVersion packageName = do
  maybeFirstMatch <- reduce (Foldl.foldMap (First . Just) getFirst) do
    l <- input $ packageName </> packageName <.> "cabal"
    case match versionLinePattern (lineToText l) of
      []          -> mzero
      version : _ -> pure version
  case maybeFirstMatch of
    Nothing -> do
      liftIO $ putStrLn $
        "Couldn't parse a version number from package " <> packageName <> "!"
      mzero
    Just version -> pure version

calculateNextPackageVersion :: ChangeSeverity -> Version -> Version
calculateNextPackageVersion c (Version branch tags) = do
  let incrementIndex :: Int -> [Int] -> [Int]
      incrementIndex 0 (h : t) = succ h : (0 <$ t)
      incrementIndex n []      = 0 : incrementIndex (pred n) []
      incrementIndex n (h : t) = h : incrementIndex (pred n) t
      ix = case c of
        Breaking    -> 1
        NonBreaking -> 2
        Patch       -> 3
  Version (incrementIndex ix branch) tags

versionLinePattern :: Pattern Version
versionLinePattern =
  "version:" *> spaces *> (Version <$> decimal `sepBy1` "." <*> pure mempty)

data ChangeSeverity
  = Patch
  | NonBreaking
  | Breaking
  deriving (Show, Eq, Ord, Bounded)

findChangeSeverity :: FilePath -> Shell ChangeSeverity
findChangeSeverity frag = do
  contents <- liftIO $ Text.readFile frag
  case commonmark frag contents of
    Left markdownError -> do
      liftIO $ putStrLn $ "Failed to parse markdown file " <> frag <> ":"
      error $ show markdownError
    Right (Headings Nothing) -> pure Patch
    Right (Headings (Just (Max sev))) -> pure sev

collectSeverities :: Fold (FilePath, Maybe ChangeSeverity, [FilePath]) (Map FilePath (Maybe ChangeSeverity))
collectSeverities = Foldl.Fold insert mempty id
  where
    insert :: Map FilePath (Maybe ChangeSeverity)
           -> (FilePath, Maybe ChangeSeverity, [FilePath])
           -> Map FilePath (Maybe ChangeSeverity)
    insert m (pkg, sev, deps) = do
      -- If a package is unchanged, but its dependencies have breaking changes,
      -- we should consider that package to also have breaking changes
      let dependenciesChanges = map (\dep -> join (Map.lookup dep m)) deps
          maxChangeSeverity =
            fmap getMax $
              foldMap (fmap Max) (sev : dependenciesChanges)
      Map.insert pkg maxChangeSeverity m

createGitBranch :: Map FilePath (Version, Version) -> Shell ()
createGitBranch versions = do
  let branchName = Text.intercalate "/" ("release" : Map.foldMapWithKey (\p (_, v) -> pure (packageNameWithVersion p v)) versions)
  procs "git" ["branch", branchName] mempty

inDirectory :: MonadIO m => FilePath -> m a -> m a
inDirectory targetDir act = do
  restoreDir <- pwd
  cd targetDir
  res <- act
  cd restoreDir
  pure res

updateCabalFile :: FilePath
                -> Version -- ^ The current version of the package
                -> Version -- ^ The next version of the package
                -> Map FilePath (Version, Version)
                -> Shell ()
updateCabalFile package current next dependenciesVersions = do
  inplace (updateVersion <|> updateDependencies) (package </> package <.> "cabal")
  where
    updateVersion =
      replaceIfContains "version:" (consensusVersionTextForCabal current) (consensusVersionTextForCabal next)
    updateDependencies = do
      Map.foldlWithKey (\pat pkg (i, o) -> replaceIfContains (fromString pkg) (consensusVersionTextForCabal i) (consensusVersionTextForCabal o) <|> pat) empty dependenciesVersions

replaceIfContains :: Pattern Text -> Text -> Text -> Pattern Text
replaceIfContains t i o = do
  contains $
    t <> chars <> (text i *> pure o)

runScrivCollect :: MonadIO m => FilePath -> Version -> m ()
runScrivCollect fp v = do
  inDirectory fp do
    procs "scriv" ["collect", "--version", Text.pack (showVersion v)] mempty

createGitCommit :: FilePath -> Version -> Shell ()
createGitCommit package next = do
  let commitString =
        "release " <> packageNameWithVersion package next
  liftIO $ putStrLn $ "Creating git commit: " <> show commitString
  procs "git" ["commit", "-am", commitString] mempty

packageNameWithVersion :: FilePath -> Version -> Text
packageNameWithVersion package v = Text.pack $
  package <> "-" <> showVersion v

consensusVersionTextForCabal :: Version -> Text
consensusVersionTextForCabal Version{versionBranch = vb@[_zero, _major, _minor, _patch]} =
  Text.intercalate "." . mapMaybe emptyIfZero $ vb
  where emptyIfZero n = if n == 0 then Nothing else Just $ Text.pack $ show n
consensusVersionTextForCabal v = Text.pack $ showVersion v

-- The following newtypes and instances are only used to pick out the headings
-- in the parsed Markdown files and can be safely ignored unless you care about
-- the internals of `findChangeSeverity`

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
      "Patch"        -> Headings (Just (Max Patch))
      "Non-Breaking" -> Headings (Just (Max NonBreaking))
      "Breaking"     -> Headings (Just (Max Breaking))
      _              -> mempty
  paragraph = mempty
  plain = mempty
  thematicBreak = mempty
  blockQuote = mempty
  codeBlock = mempty
  rawBlock = mempty
  referenceLinkDefinition = mempty
  list = mempty
