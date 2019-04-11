module HGit.Core.Merge where

--------------------------------------------
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import           Data.Functor.Compose
--------------------------------------------
import           HGit.Core.Types
import           Merkle.Functors
import           Merkle.Types
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
  => LazyMerkleDir m x
  -> LazyMerkleDir m x
  -> m ( MergeViolation `Either`
           ( LazyMerkleDir m x -- result of the merge
           , [Dir x (Hash (Dir x))] -- new structure created during merge, to be uploaded
           )
       )
mergeMerkleDirs = ((runExceptT . runWriterT) .) . mergeDirs []
  where
    mergeDirs
      :: [PartialFilePath]
      -> LazyMerkleDir m x
      -> LazyMerkleDir m x
      -> WriterT [Dir x (Hash (Dir x))] (ExceptT MergeViolation m) (LazyMerkleDir m x)
    mergeDirs h dir1 dir2 = do
      if htPointer dir1 == htPointer dir2
          then pure dir1 -- if both htPointers == then they're identical, either is fine for merge res
          else do
            ns1 <- lift . lift . fmap dirEntries . getCompose $ htElem dir1
            ns2 <- lift . lift . fmap dirEntries . getCompose $ htElem dir2

            -- merge, preserving non-conflicting changes.
            entries <- Map.mergeA Map.preserveMissing Map.preserveMissing
                                  (Map.zipWithAMatched $ onConflict h)
                                  (Map.fromList ns1) (Map.fromList ns2)

            let dir = Dir $ canonicalOrdering $ Map.toList entries
                dir' = fmap htPointer dir
            tell [dir']
            pure $ Fix . Compose . (hash dir',) . Compose $ pure dir

    -- two files with the same path
    onConflict h path (FileEntity x1) (FileEntity x2)
          -- pointers match, they're identical, take either
          | x1 == x2  = pure $ FileEntity x1
          -- file entities are not equal, merge cannot continue
          | otherwise = lift . throwE . MergeViolation $ h ++ [path]

    -- two dirs with the same path
    onConflict h path (DirEntity dir1) (DirEntity dir2)
          -- pointers match, they're identical, take either
          | htPointer dir1 == htPointer dir2 = pure $ DirEntity dir1
          | otherwise = DirEntity <$> mergeDirs (h ++ [path]) dir1 dir2

    -- dir replaced with file or vice versa
    onConflict h path _ _ = lift . throwE . MergeViolation $ h ++ [path]


data MergeViolation = MergeViolation { mergeViolationPath :: [PartialFilePath] }
  deriving (Eq, Show)
