{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tools.GitRev (gitRev) where

import Data.Text (Text)
import qualified Data.Text as T

#ifndef __GIT_REV__
import Cardano.Tools.GitRev.FromGit (gitRevText)
#endif

-- | A string representing what ouroboros-consensus git commit this code was
-- built from. No particular format should be assumed.
--
-- When @__GIT_REV__@ is defined (Nix builds via @configureFlags@), the
-- revision is embedded directly as a string literal with no Template Haskell
-- overhead. Otherwise (plain @cabal build@ in a git checkout), we fall back
-- to @githash@'s 'tGitInfoCwdTry' via "Cardano.Tools.GitRev.FromGit".
gitRev :: Text
gitRev
  | T.null rev = "unavailable (git info missing at build time)"
  | otherwise = rev
 where
#ifdef __GIT_REV__
  rev = T.pack __GIT_REV__
#else
  rev = gitRevText
#endif
