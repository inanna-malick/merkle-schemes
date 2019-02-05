{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- todo rename to indicate this does more generic filestore IO
module Ingress (buildDirTree, outputDirTree) where

--------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy
import qualified Data.List as List
import qualified System.Directory as Dir
--------------------------------------------
import           Deref
import           Util.MyCompose
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types (Pointer, HashIdentifiedEntity(..))
import           Store
--------------------------------------------

-- | write tree to file path
outputDirTree
  :: MonadIO m
  => Store m
  -> FilePath
  -> Pointer
  -> m ()
outputDirTree store outdir pointer = do
  derefed <- strictDeref store pointer
  liftIO $ evalStateT (cata alg derefed) [outdir]

  where
    alg :: Algebra ((,) Pointer :+ NamedEntity :+ Tree) (StateT [FilePath] IO ())
    alg (C (_p, (C (NamedEntity name (Leaf body)))))     = do
      path <- List.intercalate "/" . reverse . (name:) <$> get
      liftIO $ writeFile path body
    alg (C (_p, (C (NamedEntity name (Node children))))) = do
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
  -> m MerkleTree
buildDirTree store = addDirTreeToStore store . buildDirTree'

-- | annotate tree nodes with hash, adding them to some global store during this traversal
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
addDirTreeToStore
  :: forall m
   . Monad m
  => Store m
  -> Term (m :+ NamedEntity :+ Tree)
  -> m MerkleTree
addDirTreeToStore store = cata alg
  where
    alg :: Algebra (m :+ NamedEntity :+ Tree) (m MerkleTree)
    alg (C getEntity) = do
      (C (NamedEntity name entity')) <- getEntity
      entity <- NamedEntity name <$> case entity' of
        Leaf body -> pure $ Leaf body
        Node children -> do
          children' <- traverse id children
          pure $ Node children'
      pointer <- uploadShallow store $ makeShallow $ C entity
      pure $ In $ C $ Direct pointer $ C entity

buildDirTree'
  :: forall m
   . MonadIO m
  => FilePath
  -- tree structure _without_ pointer annotation
  -- type-level guarantee that there is no hash identified
  -- entity indirection allowed here
  -> Term (m :+ NamedEntity :+ Tree)
buildDirTree' = ana alg
  where
    alg :: CoAlgebra (m :+ NamedEntity :+ Tree) FilePath
    alg path = C $ do
      -- todo: validation of input file path, ideally some 'probefile :: FilePath -> IO FileType' widget
      isFile <- liftIO $ Dir.doesFileExist path
      if isFile
        then do
          fc <- liftIO $ readFile path
          pure . C . NamedEntity (justTheName path) $ Leaf fc
        else do
          isDir <- liftIO $ Dir.doesDirectoryExist path
          if isDir
            then fmap ( C
                      . NamedEntity (justTheName path)
                      . Node
                      . fmap (\x -> path ++ "/" ++ x)
                      . filter (/= ".")
                      . filter (/= "..")
                      )
               . liftIO
               $ Dir.getDirectoryContents path
            else fail ("file read error: unexpected type at " ++ path)

    justTheName :: String -> String -- hacky hax but it works - take just the name given a file path
    justTheName = reverse . takeWhile (/= '/') . reverse
