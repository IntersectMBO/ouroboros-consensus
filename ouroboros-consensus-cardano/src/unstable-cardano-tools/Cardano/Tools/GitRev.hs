{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tools.GitRev (gitRev) where

import Data.Text (Text)
import qualified Data.Text as T

-- | A string representing what ouroboros-consensus git commit this code was
-- built from. No particular format should be assumed.
--
-- The git revision is embedded at compile time via a @__GIT_REV__@ CPP flag
-- passed through @configureFlags@ in the Nix build. This replaces the old
-- @set-git-rev@ binary patching approach which broke Mach-O code signatures
-- on aarch64-darwin.
gitRev :: Text
gitRev
  | T.null rev = "unavailable (git info missing at build time)"
  | otherwise = rev
 where
  rev = T.pack gitRevStr

gitRevStr :: String
#ifdef __GIT_REV__
gitRevStr = __GIT_REV__
#else
gitRevStr = ""
#endif
