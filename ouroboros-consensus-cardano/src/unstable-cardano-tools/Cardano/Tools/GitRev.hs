{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Tools.GitRev (gitRev) where

import Data.Text (Text)
import qualified Data.Text as T

#ifndef __GIT_REV__
import GitHash (giDirty, giHash, tGitInfoCwdTry)
#endif

-- | A string representing what ouroboros-consensus git commit this code was
-- built from. No particular format should be assumed.
--
-- When @__GIT_REV__@ is defined (Nix builds via @configureFlags@), the
-- revision is embedded directly as a string literal with no Template Haskell
-- overhead. Otherwise (plain @cabal build@ in a git checkout), we fall back
-- to @githash@'s 'tGitInfoCwdTry' to obtain the revision via Template
-- Haskell.
gitRev :: Text
gitRev
  | T.null rev = "unavailable (git info missing at build time)"
  | otherwise = rev
 where
  rev =
#ifdef __GIT_REV__
    T.pack __GIT_REV__
#else
    $$( let eGitInfo = $$tGitInfoCwdTry
         in -- Local binding only to avoid redundant pattern match warning
            case eGitInfo of
              Right gitInfo ->
                [||T.pack (giHash gitInfo) <> if giDirty gitInfo then "-dirty" else ""||]
              Left _ -> [||T.empty||]
      )
#endif
