{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Ingress (buildDirTree) where

--------------------------------------------
import qualified Data.HashMap.Strict as Map
import           Data.IORef
import qualified System.Directory as Dir
--------------------------------------------
import Types
--------------------------------------------

-- | actual dir recursive traversal
-- ignores permissions in diffs and coerces file contents into unicode #YOLO
buildDirTree :: GlobalStore -> FilePath -> FilePath -> IO (HashTerm (NamedEntity Tree))
buildDirTree store path fn = do
  let fullpath = path ++ "/" ++ fn
  -- putStrLn $ "run buildDirTree on: " ++ fullpath
  isFile <- Dir.doesFileExist fullpath
  if isFile
    then do
      fc <- readFile fullpath
      let (pointer, entity) = lift $ NamedEntity fn $ Leaf fc
      modifyIORef' store (Map.insert pointer entity)
      pure $ Direct pointer entity
    else do
      isDir <- Dir.doesDirectoryExist fullpath
      if isDir
        then do
          contents <- filter (/= ".") . filter (/= "..") <$> Dir.getDirectoryContents fullpath
          entities <- traverse (buildDirTree store fullpath) contents
          let (pointer, entity) = lift $ NamedEntity fn $ Node $ fmap htPointer entities
          modifyIORef' store (Map.insert pointer entity)
          pure $ Direct pointer entity
      else error $ "invalid file path " ++ fullpath ++ "? wtf yo no symlinks or sockets allowed"
