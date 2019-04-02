-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed htPointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module HGit.Diff (diffMerkleDirs) where

--------------------------------------------
import           Control.Monad (join)
import           Control.Monad.IO.Class
import           Data.Functor.Compose
import qualified Data.Map.Strict as Map
--------------------------------------------
import           HGit.Diff.Types
import           HGit.Types.HGit
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
   . Monad m
  => Eq x
  => MonadIO m
  => Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)
  -> Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)
  -> m [([PartialFilePath], Diff)]
diffMerkleDirs = compareDir []
  where
    compareDir
      :: [PartialFilePath]
      -> Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)
      -> Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)
      -- todo: writer w/ stack (?) so I can push/path segments to go with changes to tag diffs with loc...
      -> m [([PartialFilePath], Diff)]
    compareDir h dir1 dir2 =
      if htPointer dir1 == htPointer dir2
          then pure []
          else do
            ns1' <- fmap dirEntries . getCompose $ htElem dir1
            ns2' <- fmap dirEntries . getCompose $ htElem dir2

            fmap join . traverse (resolveMapDiff h)
                      $ mapCompare (Map.fromList ns1') (Map.fromList ns2')

    resolveMapDiff
      :: [PartialFilePath]
      -> ( PartialFilePath
         , These (FileTreeEntity x (Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)))
                 (FileTreeEntity x (Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)))
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
      -> FileTreeEntity x (Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x))
      -> FileTreeEntity x (Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x))
      -> m [([PartialFilePath], Diff)]
    compareDerefed h path (DirEntity _) (FileEntity _)
      = pure [(h ++ [path], DirReplacedWithFile)]
    compareDerefed h path (FileEntity _) (DirEntity _)
      = pure [(h ++ [path], FileReplacedWithDir)]
    compareDerefed h path (FileEntity x1) (FileEntity x2)
      | x1 /= x2 = pure [(h ++ [path], FileModified)]
      | otherwise = do
          -- liftIO $ putStrLn $ "compare FileEntity (file) with FileEntity (file) same htPointers"
          pure []
    compareDerefed h path (DirEntity dir1) (DirEntity dir2)
      | htPointer dir1 == htPointer dir2 = pure []
      | otherwise = compareDir (h ++ [path]) dir1 dir2
