-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed htPointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module HGit.Core.Diff where

--------------------------------------------
import           Control.Monad (join)
import           Data.Functor.Compose
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
--------------------------------------------
import           HGit.Core.Types
import           Merkle.Functors
--------------------------------------------

-- | Diff two merkle trees where the hash-identified nodes have been
--   converted to lazily-expanded effectful streams of values in some monadic stack 'm'
diffMerkleDirs
  :: forall m x
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . ( Monad m, Eq x)
  => LazyMerkleDir m x
  -> LazyMerkleDir m x
  -> m [([PartialFilePath], Diff)]
diffMerkleDirs dir1 dir2 =
  if htPointer dir1 == htPointer dir2
      then pure []
      else do
        ns1 <- fmap dirEntries . getCompose $ htElem dir1
        ns2 <- fmap dirEntries . getCompose $ htElem dir2


        res <- Map.mergeA onRemoved onAdded (Map.zipWithMaybeAMatched onConflict)
                (Map.fromList ns1) (Map.fromList ns2)

        pure $ join $ fmap snd $ Map.toList res

  where
    onRemoved  = Map.traverseMissing $ \path _ -> pure [([path], EntityDeleted)]
    onAdded    = Map.traverseMissing $ \path _ -> pure [([path], EntityCreated)]
    -- zipWithMaybeAMatched :: (k -> x -> y -> f (Maybe z)) -> WhenMatched f k x y z

    -- two files with the same path
    onConflict :: PartialFilePath
               -> FileTreeEntity x (LazyMerkleDir m x)
               -> FileTreeEntity x (LazyMerkleDir m x)
               -> m (Maybe [([PartialFilePath], Diff)])
    onConflict path (FileEntity x1) (FileEntity x2)
          -- they're identical, no diff
          | x1 == x2  = pure Nothing
          -- file entities are not equal, file modified
          | otherwise = pure $ Just [([path], FileModified)]

    -- two dirs with the same path
    onConflict path (DirEntity dir1') (DirEntity dir2')
          -- pointers match, they're identical, just stop here
          | htPointer dir1' == htPointer dir2' = pure Nothing
          | otherwise = Just . (fmap (\(p,x) -> ([path] ++ p, x))) <$> diffMerkleDirs dir1' dir2'

    -- dir replaced with file
    onConflict path (DirEntity _) (FileEntity _) = pure $ Just [([path], DirReplacedWithFile)]

    -- file replaced with dir
    onConflict path (FileEntity _) (DirEntity _) = pure $ Just [([path], FileReplacedWithDir)]


data Diff = FileModified
          | FileReplacedWithDir
          | DirReplacedWithFile
          | EntityDeleted
          | EntityCreated
  deriving (Eq, Ord, Show)
