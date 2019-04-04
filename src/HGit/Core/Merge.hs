module HGit.Core.Merge where

--------------------------------------------
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map
import           Data.Functor.Compose
--------------------------------------------
import           HGit.Core.Types
import           Merkle.Functors
import           Merkle.Types
import           Util.These (These(..), mapCompare)
import           Util.MyCompose
import           Util.RecursionSchemes
--------------------------------------------


-- | Merge two merkle trees and either fail (if partial derefing detects a conflict)
--   or succeed (in the case that all differences are non-overlapping) - will upload
--   any new directories (only entity created here) via the provided store. May end
--   up uploading some directories then failing due to a non-resolvable merge conflict,
--   could be fixed via future optimization, exercise for the reader, etc etc
mergeMerkleDirs
  :: forall m x
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . ( Monad m, Eq x, Hashable (Dir x))
  => LazyMerkleDir m x -> LazyMerkleDir m x
  -> m $ MergeViolation `Either`
           ( LazyMerkleDir m x -- result of the merge
           , [Dir x (Hash (Dir x))] -- new structure created during merge, to be uploaded
           )
mergeMerkleDirs dir1' dir2' = runExceptT $ runWriterT $ mergeDirs [] dir1' dir2'
  where
    mergeDirs
      :: [PartialFilePath]
      ->  LazyMerkleDir m x
      ->  LazyMerkleDir m x
      ->  WriterT [Dir x (Hash (Dir x))] (ExceptT MergeViolation m) (LazyMerkleDir m x)
    mergeDirs h dir1 dir2 = do
      if htPointer dir1 == htPointer dir2
          then pure dir1 -- if both htPointers == then they're identical, either is fine for merge res
          else do
            ns1' <- lift . lift . fmap dirEntries . getCompose $ htElem dir1
            ns2' <- lift . lift . fmap dirEntries . getCompose $ htElem dir2

            entries <- traverse (resolveMapDiff h)
                      $ mapCompare (Map.fromList ns1') (Map.fromList ns2')

            let dir = Dir $ canonicalOrdering entries
                dir' = fmap htPointer dir
            tell [dir']
            pure $ Fix . Compose . (hash dir',) . Compose $ pure dir

    resolveMapDiff _
      (path, This x) = pure (path, x) -- non-conflicting change, keep
    resolveMapDiff _
      (path, That x) = pure (path, x) -- non-conflicting change, keep

    -- two files with the same path
    resolveMapDiff h (path, These (FileEntity x1) (FileEntity x2))
          -- they're identical, continue merge
          | x1 == x2  = pure (path, FileEntity x1)
          -- file entities are not equal, merge cannot continue
          | otherwise = lift . throwE . MergeViolation $ h ++ [path]

    -- two dirs with the same path
    resolveMapDiff h (path, These (DirEntity dir1) (DirEntity dir2))
          -- pointers match, they're identical, just stop here
          | htPointer dir1 == htPointer dir2 = pure (path, DirEntity dir1)
          | otherwise = (path,) . DirEntity <$> mergeDirs (h ++ [path]) dir1 dir2

    -- neither a (file, file) change nor a (dir, dir) change. fail.
    resolveMapDiff h (path, These _ _) = lift . throwE . MergeViolation $ h ++ [path]

data MergeViolation = MergeViolation { mergeViolationPath :: [PartialFilePath] }
  deriving (Eq, Show)
