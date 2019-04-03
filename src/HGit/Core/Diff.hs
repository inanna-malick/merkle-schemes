-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed htPointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module HGit.Core.Diff where

--------------------------------------------
import           Control.Monad (join)
import           Data.Functor.Compose
import qualified Data.Map.Strict as Map
--------------------------------------------
import           HGit.Core.Types
import           Merkle.Functors
import           Util.These (These(..), mapCompare)
import           Util.RecursionSchemes
--------------------------------------------


-- | Diff two merkle trees where the hash-identified nodes have been
--   converted to lazily-expanded effectful streams of values in some monadic stack 'm'
diffMerkleDirs
  :: forall m x
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . ( Monad m
     , Eq x
     )
  => Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)
  -> Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)
  -> m [([PartialFilePath], Diff)]
diffMerkleDirs = compareDir []
  where
    compareDir
      :: [PartialFilePath]
      -> Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)
      -> Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)
      -> m [([PartialFilePath], Diff)]
    compareDir h dir1 dir2 =
      if htPointer dir1 == htPointer dir2
          then pure []
          else do
            ns1' <- fmap dirEntries . getCompose $ htElem dir1
            ns2' <- fmap dirEntries . getCompose $ htElem dir2

            fmap join . traverse (resolveMapDiff h)
                      $ mapCompare (Map.fromList ns1') (Map.fromList ns2')


    resolveMapDiff h (n, This _) = pure [(h ++ [n], EntityDeleted)]
    resolveMapDiff h (n, That _) = pure [(h ++ [n], EntityCreated)]

    -- two files with the same path
    resolveMapDiff h (path, These (FileEntity x1) (FileEntity x2))
          -- they're identical, no diff
          | x1 == x2  = pure []
          -- file entities are not equal, file modified
          | otherwise = pure [(h ++ [path], FileModified)]

    -- two dirs with the same path
    resolveMapDiff h (path, These (DirEntity dir1) (DirEntity dir2))
          -- pointers match, they're identical, just stop here
          | htPointer dir1 == htPointer dir2 = pure []
          | otherwise = compareDir (h ++ [path]) dir1 dir2

    -- dir replaced with file
    resolveMapDiff h (path, These (DirEntity _) (FileEntity _))
      = pure [(h ++ [path], DirReplacedWithFile)]

    -- file replaced with dir
    resolveMapDiff h (path, These (FileEntity _) (DirEntity _))
      = pure [(h ++ [path], FileReplacedWithDir)]


data Diff = FileModified
          | FileReplacedWithDir
          | DirReplacedWithFile
          | EntityDeleted
          | EntityCreated
  deriving (Eq, Ord, Show)
