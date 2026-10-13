{-# LANGUAGE OverloadedStrings #-}

-- | A file system confined to one directory of another.
module Test.Util.FS.Subdirectory (subdirectoryHasFS) where

import System.FS.API (HasFS (..))
import System.FS.API.Types (FsPath, fsPathToList, (</>))

-- | The given file system, with every path taken relative to the given
-- directory in it, as 'System.FS.IO.ioHasFS' takes every path relative to its
-- 'System.FS.API.Types.MountPoint'.
--
-- Neither the directory nor any path may have a @..@ component, since that
-- could escape the directory.
subdirectoryHasFS :: FsPath -> HasFS m h -> HasFS m h
subdirectoryHasFS dir fs =
  fs
    { hOpen = \path mode -> hOpen fs (under path) mode
    , createDirectory = \path -> createDirectory fs (under path)
    , createDirectoryIfMissing = \parents path -> createDirectoryIfMissing fs parents (under path)
    , listDirectory = \path -> listDirectory fs (under path)
    , doesDirectoryExist = \path -> doesDirectoryExist fs (under path)
    , doesFileExist = \path -> doesFileExist fs (under path)
    , removeDirectoryRecursive = \path -> removeDirectoryRecursive fs (under path)
    , removeFile = \path -> removeFile fs (under path)
    , renameFile = \from to -> renameFile fs (under from) (under to)
    , mkFsErrorPath = \path -> mkFsErrorPath fs (under path)
    , unsafeToFilePath = \path -> unsafeToFilePath fs (under path)
    }
 where
  under path = noParent dir </> noParent path

noParent :: FsPath -> FsPath
noParent path
  | ".." `elem` fsPathToList path =
      error $ "subdirectoryHasFS: a path has a .. component: " <> show path
  | otherwise = path
