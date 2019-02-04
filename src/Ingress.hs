{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- todo rename to more generic filestore IO
module Ingress (buildDirTree, outputDirTree) where

import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Except
--------------------------------------------
import qualified Data.List as List
import qualified Data.Hashable as Hash
import qualified Data.HashMap.Strict as Map
import           Data.IORef
import qualified System.Directory as Dir
--------------------------------------------
import           Deref
import           Errors
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types (HashIdentifiedEntity(..), Pointer(..))
--------------------------------------------

-- | write tree to file path
outputDirTree :: GlobalStore -> FilePath -> MerkleTree -> ExceptT MerkleTreeCompareError IO ()
outputDirTree store outdir tree = do
  derefed <- deAnnotateM (derefOneLayer store) tree
  liftIO $ evalStateT (cata alg derefed) [outdir]

  where
    alg :: Algebra (NamedEntity Tree) (StateT [FilePath] IO ())
    alg (NamedEntity name (Leaf body))     = do
      path <- List.intercalate "/" . reverse . (name:) <$> get
      liftIO $ writeFile path body
    alg (NamedEntity name (Node children)) = do
      path <- List.intercalate "/" . reverse . (name:) <$> get
      liftIO $ Dir.createDirectory path
      modify (push name)
      _ <- traverse id children
      modify pop

    push x xs = x:xs
    pop (_:xs)  = xs
    pop []    = []


-- | actual dir recursive traversal
-- ignores permissions in diffs (all file permissions are, idk, that of running process?)
-- and coerces file contents into unicode #YOLO. Reads directory structure into memory
-- and annotates nodes with their hashes
buildDirTree :: GlobalStore -> FilePath -> FilePath -> ExceptT FileReadError IO MerkleTree
buildDirTree store path filename = buildDirTree' path filename >>= (liftIO . addDirTreeToStore store)

-- | annotate tree nodes with hash, adding them to some global store during this traversal
addDirTreeToStore
  :: GlobalStore
  -> Term (NamedEntity Tree)
  -> IO MerkleTree
addDirTreeToStore store = annotateWithLayer alg
  where
    alg :: MonadicAnnotaterAlg IO (NamedEntity Tree) HashIdentifiedEntity
    alg entity = do
      let pointer = Pointer . Hash.hash $ makeShallow entity
      modifyIORef' store (Map.insert pointer entity)
      pure $ In $ Direct pointer entity

buildDirTree'
  :: FilePath
  -> FilePath
  -- tree structure _without_ pointer annotation
  -- type-level guarantee that there is no hash identified
  -- entity indirection allowed here
  -> ExceptT FileReadError IO (Term (NamedEntity Tree))
buildDirTree' = curry (anaButInM alg)
  where
    alg :: MonadicCoAlgebra (ExceptT FileReadError IO) (NamedEntity Tree) (FilePath, FilePath)
    alg (path, filename) = do
      -- todo: validation of input file path, ideally some 'probefile :: FilePath -> IO FileType' widget
      let fullpath = path ++ "/" ++ filename
      -- putStrLn $ "run buildDirTree on: " ++ fullpath
      isFile <- liftIO $ Dir.doesFileExist fullpath
      if isFile
        then do
          fc <- liftIO $ readFile fullpath
          pure $ NamedEntity filename $ Leaf fc
        else do
          isDir <- liftIO $ Dir.doesDirectoryExist fullpath
          if isDir
            then fmap ( NamedEntity filename
                      . Node
                      . fmap (fullpath,)
                      . filter (/= ".")
                      . filter (/= "..")
                      )
               . liftIO
               $ Dir.getDirectoryContents fullpath
            else throwError (FileReadError fullpath)


data FileReadError = FileReadError FilePath -- tried to read this path but failed (todo better errors? idk lol)
  deriving (Show)
