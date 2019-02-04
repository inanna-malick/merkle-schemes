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

--------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy
import qualified Data.List as List
import           Data.Functor.Compose (Compose(..))
import qualified System.Directory as Dir
--------------------------------------------
import           Deref
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types (HashIdentifiedEntity(..))
import           Store
--------------------------------------------

-- | write tree to file path
outputDirTree
  :: MonadIO m
  => Store m
  -> FilePath
  -> MerkleTree
  -> m ()
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
buildDirTree
  :: MonadIO m
  => Store m
  -> FilePath
  -> FilePath
  -> m MerkleTree
buildDirTree store path filename
    = addDirTreeToStore store $ buildDirTree' path filename

-- | annotate tree nodes with hash, adding them to some global store during this traversal
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
addDirTreeToStore
  :: forall m
   . Monad m
  => Store m
  -> Term (Compose m (NamedEntity Tree))
  -> m MerkleTree
addDirTreeToStore store = cata alg
  where
    alg :: Algebra (Compose m (NamedEntity Tree)) (m MerkleTree)
    alg getEntity = do
      (NamedEntity name entity') <- getCompose $ getEntity
      entity <- NamedEntity name <$> case entity' of
        Leaf body -> pure $ Leaf body
        Node children -> do
          children' <- traverse id children
          pure $ Node children'
      pointer <- uploadShallow store $ makeShallow entity
      pure $ In $ Direct pointer entity

buildDirTree'
  :: forall m
   . MonadIO m
  => FilePath
  -> FilePath
  -- tree structure _without_ pointer annotation
  -- type-level guarantee that there is no hash identified
  -- entity indirection allowed here
  -> Term (Compose m (NamedEntity Tree))
buildDirTree' = curry (ana alg)
  where
    alg :: CoAlgebra (Compose m (NamedEntity Tree)) (FilePath, FilePath)
    alg (path, filename) = Compose $ do
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
            else fail ("file read error: unexpected type at " ++ fullpath)
