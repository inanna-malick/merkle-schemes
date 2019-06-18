module DirTree.Diff.Simple where


import           Control.Monad (join)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import           DirTree.Types
import           DirTree.Diff.Types
import           Control.RecursionSchemes



-- | Diff two directory trees
diffDirTrees :: DirTree -> DirTree -> [(FilePath, Diff)]
diffDirTrees (Fix (DirLayer entities1))
             (Fix (DirLayer entities2)) = join $ Map.elems mergeRes

  where
    mergeRes :: Map FilePath [(FilePath, Diff)]
    mergeRes = Map.merge (Map.mapMissing onRemoved)
                         (Map.mapMissing onAdded)
                         (Map.zipWithMatched onConflict)
                         (Map.fromList entities1)
                         (Map.fromList entities2)


onRemoved, onAdded :: FilePath -> FileTreeEntity DirTree -> [(FilePath, Diff)]
onRemoved path _ = [(path, EntityDeleted)]
onAdded   path _ = [(path, EntityCreated)]


onConflict :: FilePath
           -> FileTreeEntity DirTree
           -> FileTreeEntity DirTree
           -> [(FilePath, Diff)]

-- named directory exists in both DirTrees: recurse
onConflict path (DirEntity d1) (DirEntity d2) =
  let updatePath (p,x) = (path ++ "/" ++ p, x)
   in updatePath <$> diffDirTrees d1 d2

-- named file exists in both DirTrees: compare
onConflict path (FileEntity f1) (FileEntity f2)
  | f1 /= f2  = [(path, FileModified)]
  | otherwise = []

-- dir replaced with file
onConflict path (DirEntity _) (FileEntity _) = [(path, DirReplacedWithFile)]

-- file replaced with dir
onConflict path (FileEntity _) (DirEntity _) = [(path, FileReplacedWithDir)]

