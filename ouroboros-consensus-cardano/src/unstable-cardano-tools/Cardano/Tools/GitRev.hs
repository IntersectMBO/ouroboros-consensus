{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Tools.GitRev (gitRev) where

import qualified Cardano.Git.Rev
import           Data.Text (Text)
import qualified Data.Text as T
import           GitHash (giDirty, giHash, tGitInfoCwdTry)

-- | A string representing what ouroboros-consensus git commit this code was
-- built from. No particular format should be assumed.
gitRev :: Text
gitRev
  | T.all (== '0') rev = "unavailable (git info missing at build time)"
  | otherwise          = rev
  where
    rev =
        $$(let eGitInfo = $$tGitInfoCwdTry in
           -- Local binding only to avoid redundant pattern match warning
           case eGitInfo of
             Right gitInfo ->
               [||T.pack (giHash gitInfo) <> if giDirty gitInfo then "-dirty" else ""||]
             -- In case of failure, try cardano-git-rev (where the commit hash
             -- can be embedded later).
             Left _ -> [||otherRev||]
          )
    otherRev = $(Cardano.Git.Rev.gitRev)
