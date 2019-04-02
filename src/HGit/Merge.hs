module HGit.Merge where

--------------------------------------------
import           Control.Exception.Safe (MonadThrow)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map
import           Data.Functor.Compose
--------------------------------------------
import           HGit.Types.HGit
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Store.Deref (lazyDeref')
import           Merkle.Types
import           Util.These (These(..), mapCompare)
import           Util.MyCompose
import           Util.RecursionSchemes
--------------------------------------------


data MergeViolation = MergeViolation { mergeViolationPath :: [PartialFilePath] }
  deriving (Eq, Show)


mergeMerkleDirs
  :: forall m x
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . ( Monad m
     , MonadIO m
     , MonadThrow m
     , Eq x
     )
  => Store m (Dir x)
  -> Hash (Dir x)
  -> Hash (Dir x)
  -> m $ MergeViolation `Either` Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)
mergeMerkleDirs store p1 p2 =
  runExceptT $ mergeMerkleDirs' store (lazyDeref' store p1) (lazyDeref' store p2)


-- | Merge two merkle trees and either fail (if partial derefing detects a conflict)
--   or succeed (in the case that all differences are non-overlapping) - will upload
--   any new directories (only entity created here) via the provided store. May end
--   up uploading some directories then failing due to a non-resolvable merge conflict,
--   could be fixed via future optimization, exercise for the reader, etc etc
mergeMerkleDirs'
  :: forall m x
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => MonadIO m
  => Eq x
  -- TODO: is this really needed? could upload in later phase, don't like upload even if partial failure
  => Store m (Dir x)
  -> Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)
  -> Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)
  -> ExceptT MergeViolation m $ Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)
mergeMerkleDirs' store = mergeDirs []
  where
    mergeDirs
      :: [PartialFilePath]
      -> Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)
      -> Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)
      -> ExceptT MergeViolation m $ Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)
    mergeDirs h dir1 dir2 = do
      if htPointer dir1 == htPointer dir2
          then pure dir1 -- if both htPointers == then they're identical, either is fine for merge res
          else do
            ns1' <- ExceptT . fmap (Right . dirEntries) . getCompose $ htElem dir1
            ns2' <- ExceptT . fmap (Right . dirEntries) . getCompose $ htElem dir2

            entries <- traverse (resolveMapDiff h)
                      $ mapCompare (Map.fromList ns1') (Map.fromList ns2')

            let dir = Dir $ canonicalOrdering entries
                dir' = fmap htPointer dir
            p <- ExceptT $ Right <$> sUploadShallow store dir'
            pure $ Fix . Compose . (p,) . Compose $ pure dir

    resolveMapDiff
      :: [PartialFilePath]
      -> ( PartialFilePath
         , These (FileTreeEntity x (Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)))
                 (FileTreeEntity x (Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)))
         )
      -> ExceptT MergeViolation m $ NamedFileTreeEntity x (Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x))
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
      -> FileTreeEntity x (Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x))
      -> FileTreeEntity x (Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x))
      -> ExceptT MergeViolation m $ NamedFileTreeEntity x
                                  (Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x))
    compareDerefed h path (DirEntity _) (FileEntity _)
      = throwE . MergeViolation $ h ++ [path]
    compareDerefed h path (FileEntity _) (DirEntity _)
      = throwE . MergeViolation $ h ++ [path]
    compareDerefed h path (FileEntity x1) (FileEntity x2)
      | x1 /= x2  = throwE . MergeViolation $ h ++ [path]
      | otherwise = pure (path, FileEntity x1)
    compareDerefed h path (DirEntity dir1) (DirEntity dir2)
      | htPointer dir1 == htPointer dir2
      = pure (path, DirEntity dir1) -- they're identical, just stop here
      | otherwise
      = (path,) . DirEntity <$> mergeDirs (h ++ [path]) dir1 dir2



