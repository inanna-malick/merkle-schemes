{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Ingress (buildDirTree) where

--------------------------------------------
import qualified Data.Hashable as Hash
import qualified Data.HashMap.Strict as Map
import           Data.IORef
import qualified System.Directory as Dir
--------------------------------------------
import           Util.RecursionSchemes (Term(..))
import           Merkle.Tree.Types
import           Merkle.Types (mtPointer, HashIdentifiedEntity(..), Pointer(..))
--------------------------------------------

-- | actual dir recursive traversal
-- ignores permissions in diffs and coerces file contents into unicode #YOLO
buildDirTree :: GlobalStore -> FilePath -> FilePath -> IO MerkleTree
buildDirTree store path fn = do
  let fullpath = path ++ "/" ++ fn
  -- putStrLn $ "run buildDirTree on: " ++ fullpath
  isFile <- Dir.doesFileExist fullpath
  entity <- if isFile
    then do
      fc <- readFile fullpath
      pure $ NamedEntity fn $ Leaf fc
    else do
      isDir <- Dir.doesDirectoryExist fullpath
      if isDir
        then do
          contents <- filter (/= ".") . filter (/= "..") <$> Dir.getDirectoryContents fullpath
          entities <- traverse (buildDirTree store fullpath) contents
          pure $ NamedEntity fn $ Node $ fmap mtPointer entities
      else error $ "invalid file path " ++ fullpath ++ "? wtf yo no symlinks or sockets allowed"

  let pointer = Pointer . Hash.hash $ entity
      entity' = makeConcrete entity
  modifyIORef' store (Map.insert pointer entity')
  pure $ In $ Direct pointer entity'
