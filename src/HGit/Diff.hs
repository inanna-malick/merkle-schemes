-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed pointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module HGit.Diff (diffMerkleDirs) where

--------------------------------------------
import           Control.Monad (join)
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict as Map
--------------------------------------------
import           HGit.Diff.Types
import           HGit.Types
import           Merkle.Types
import           Util.These (These(..), mapCompare)
import           Util.HRecursionSchemes
--------------------------------------------


-- | Diff two merkle trees where the hash-identified nodes have been
--   converted to lazily-expanded effectful streams of values in some monadic stack 'm'
diffMerkleDirs
  :: forall m
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => MonadIO m
  => Term (LazyHashTagged m HGit) 'DirTag
  -> Term (LazyHashTagged m HGit) 'DirTag
  -> m [([PartialFilePath], Diff)]
diffMerkleDirs = compareDir []
  where
    compareDir
      :: [PartialFilePath]
      -> Term (LazyHashTagged m HGit) 'DirTag
      -> Term (LazyHashTagged m HGit) 'DirTag
      -- todo: writer w/ stack (?) so I can push/path segments to go with changes to tag diffs with loc...
      -> m [([PartialFilePath], Diff)]
    compareDir h dir1 dir2 =
      if pointer dir1 == pointer dir2
          then pure []
          else do
            ns1' <- dirEntries <$> derefLayer dir1
            ns2' <- dirEntries <$> derefLayer dir2

            fmap join . traverse (resolveMapDiff h)
                      $ mapCompare (Map.fromList ns1') (Map.fromList ns2')

    resolveMapDiff
      :: [PartialFilePath]
      -> ( PartialFilePath
         , These (FileTreeEntity (Term (LazyHashTagged m HGit)))
                 (FileTreeEntity (Term (LazyHashTagged m HGit)))
         )
      -> m [([PartialFilePath], Diff)]
    resolveMapDiff h
      (n, This _) = pure [(h ++ [n], EntityDeleted)]
    resolveMapDiff h
      (n, These e1 e2) = do
        -- liftIO $ putStrLn $ "compareDerefed" ++ n
        compareDerefed h n e1 e2
    resolveMapDiff h
      (n, That _) = pure [(h ++ [n], EntityCreated)]

    -- TODO: new name?
    compareDerefed
      :: [PartialFilePath]
      -> PartialFilePath
      -> FileTreeEntity (Term (LazyHashTagged m HGit))
      -> FileTreeEntity (Term (LazyHashTagged m HGit))
      -> m [([PartialFilePath], Diff)]
    compareDerefed h path (DirEntity _) (FileEntity _)
      = pure [(h ++ [path], DirReplacedWithFile)]
    compareDerefed h path (FileEntity _) (DirEntity _)
      = pure [(h ++ [path], FileReplacedWithDir)]
    compareDerefed h path (FileEntity fc1) (FileEntity fc2)
      | pointer fc1 /= pointer fc2 = pure [(h ++ [path], FileModified)]
      | otherwise = do
          -- liftIO $ putStrLn $ "compare FileEntity (file) with FileEntity (file) same pointers"
          pure []
    compareDerefed h path (DirEntity dir1) (DirEntity dir2)
      | pointer dir1 == pointer dir2 = pure []
      | otherwise = compareDir (h ++ [path]) dir1 dir2
