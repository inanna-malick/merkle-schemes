module HGit.Merge (mergeMerkleDirs) where

--------------------------------------------
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict as Map
import           Data.Functor.Const
--------------------------------------------
import           HGit.Types
import           HGit.Store
import           HGit.Store.Deref (lazyDeref)
import           Util.These (These(..), mapCompare)
import           Util.MyCompose
import           Util.HRecursionSchemes
--------------------------------------------
import qualified Data.Functor.Compose as FC
import Control.Monad.Trans.Except


data MergeViolation = MergeViolation { mergeViolationPath :: [PartialFilePath] }
  deriving Show


mergeMerkleDirs
  :: forall m
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => MonadIO m
  => Store m HGit
  -> Const HashPointer 'DirTag
  -> Const HashPointer 'DirTag
  -> m $ Either MergeViolation $ Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
mergeMerkleDirs store p1 p2 =
  runExceptT $ mergeMerkleDirs' store (lazyDeref store p1) (lazyDeref store p2)


-- | Merge two merkle trees and either fail (if partial derefing detects a conflict)
--   or succeed (in the case that all differences are non-overlapping) - will upload
--   any new directories (only entity created here) via the provided store. May end
--   up uploading some directories then failing due to a non-resolvable merge conflict,
--   could be fixed via future optimization, exercise for the reader, etc etc
mergeMerkleDirs'
  :: forall m
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => MonadIO m
  => Store m HGit
  -> Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
  -> Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
  -> ExceptT MergeViolation m $ Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
mergeMerkleDirs' store = mergeDirs []
  where
    mergeDirs
      :: [PartialFilePath]
      -> Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
      -> Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
      -> ExceptT MergeViolation m $ Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
    mergeDirs h dir1 dir2 = do
      ns1' <- ExceptT $ Right . dirEntries <$> derefLayer dir1
      ns2' <- ExceptT $ Right . dirEntries <$> derefLayer dir2

      entries <- traverse (resolveMapDiff h)
                $ mapCompare (Map.fromList ns1') (Map.fromList ns2')

      let dir = Dir entries
          dir' = hfmap pointer' dir
      hash <- ExceptT $ Right <$> sUploadShallow store dir'
      pure $ Term $ HC $ FC.Compose $ C $ (getConst hash, pure dir)

    resolveMapDiff
      :: [PartialFilePath]
      -> ( PartialFilePath
         , These (FileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit)))
                 (FileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit)))
         )
      -> ExceptT MergeViolation m $ NamedFileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit))
    resolveMapDiff _
      (n, This x) = pure (n, x) -- non-conflicting change, keep
    resolveMapDiff h
      (n, These e1 e2) = -- changes in both branches, might be valid if both dirs & non-overlapping
        compareDerefed h n e1 e2
    resolveMapDiff _
      (n, That x) = pure (n, x) -- non-conflicting change, keep

    -- TODO: new name?
    compareDerefed
      :: [PartialFilePath]
      -> PartialFilePath
      -> FileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit))
      -> FileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit))
      -> ExceptT MergeViolation m $ NamedFileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit))
    compareDerefed h path (DirEntity _) (FileEntity _)
      = throwE . MergeViolation $ h ++ [path]
    compareDerefed h path (FileEntity _) (DirEntity _)
      = throwE . MergeViolation $ h ++ [path]
    compareDerefed h path (FileEntity fc1) (FileEntity fc2)
      | pointer fc1 /= pointer fc2
      = throwE . MergeViolation $ h ++ [path]
      | otherwise
      = pure (path, FileEntity fc1)
    compareDerefed h path (DirEntity dir1) (DirEntity dir2)
      | pointer dir1 == pointer dir2
      = pure (path, DirEntity dir1) -- they're identical, just stop here
      | otherwise
      = (path,) . DirEntity <$> mergeDirs (h ++ [path]) dir1 dir2



