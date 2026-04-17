{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell fallback for obtaining the git revision at compile time.
-- Used when @__GIT_REV__@ is not defined (plain @cabal build@ in a git
-- checkout).  Separated from "Cardano.Tools.GitRev" so that fourmolu can
-- process both modules (CPP and typed TH splices confuse the parser when
-- combined in one file).
module Cardano.Tools.GitRev.FromGit (gitRevText) where

import Data.Text (Text)
import qualified Data.Text as T
import GitHash (giDirty, giHash, tGitInfoCwdTry)

-- | The git revision obtained via @githash@'s 'tGitInfoCwdTry'.  Returns
-- 'T.empty' when git info is unavailable.
gitRevText :: Text
gitRevText =
  $$( let eGitInfo = $$tGitInfoCwdTry
       in case eGitInfo of
            Right gitInfo ->
              [||T.pack (giHash gitInfo) <> if giDirty gitInfo then "-dirty" else ""||]
            Left _ -> [||T.empty||]
    )
