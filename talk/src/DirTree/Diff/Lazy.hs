module DirTree.Diff.Lazy where


import           Control.Monad (join)
import           Data.Functor.Compose
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import           DirTree.Types
import           DirTree.Diff.Types
import           Control.RecursionSchemes



-- | Diff two directory trees
diffDirTreesLazy
  :: forall m
   . Monad m
  => LazyDirTree m
  -> LazyDirTree m
  -> m [(FilePath, Diff)]
diffDirTreesLazy (Fix (Compose (hash1, Compose eff1)))
             (Fix (Compose (hash2, Compose eff2)))
  | hash1 == hash2 = pure []
  | otherwise = do
      DirLayer entities1 <- eff1
      DirLayer entities2 <- eff2
      mergeRes <- Map.mergeA (Map.mapMissing onRemoved)
                             (Map.mapMissing onAdded)
                             (Map.zipWithAMatched onConflict)
                             (Map.fromList entities1)
                             (Map.fromList entities2)
      pure . join . Map.elems $ mergeRes


onRemoved, onAdded :: FilePath -> FileTreeEntity x -> [(FilePath, Diff)]
onRemoved path _ = [(path, EntityDeleted)]
onAdded   path _ = [(path, EntityCreated)]


onConflict :: Monad m
           => FilePath
           -> FileTreeEntity (LazyDirTree m)
           -> FileTreeEntity (LazyDirTree m)
           -> m [(FilePath, Diff)]

-- named directory exists in both DirTrees: recurse
onConflict path (DirEntity d1) (DirEntity d2) = do
  let updatePath (p,x) = (path ++ "/" ++ p, x)
  res <- diffDirTreesLazy d1 d2
  pure $ fmap updatePath res

-- named file exists in both DirTrees: compare
onConflict path (FileEntity f1) (FileEntity f2)
  | f1 /= f2  = pure [(path, FileModified)]
  | otherwise = pure []

-- dir replaced with file
onConflict path (DirEntity _) (FileEntity _) = pure [(path, DirReplacedWithFile)]

-- file replaced with dir
onConflict path (FileEntity _) (DirEntity _) = pure [(path, FileReplacedWithDir)]

