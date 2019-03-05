-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed pointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module HGit.Diff (diffMerkleDirs) where

--------------------------------------------
import           Control.Monad (join)
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict as Map
import           Data.Functor.Const
--------------------------------------------
import           HGit.Diff.Types
import           HGit.Types
import           HGit.Store (Store)
import           HGit.Store.Deref (lazyDeref)
import           Util.These (These(..), mapCompare)
import           Util.MyCompose
import           Util.HRecursionSchemes
--------------------------------------------
import qualified Data.Functor.Compose as FC

-- | Diff two merkle trees, producing diffs and a record of expansions/derefs performed
--   lazily fetches structure of the two trees such that only the parts required
--   to do this comparison are fetched from the global state store (via 'network call')
diffMerkleDirs
  :: forall m
  -- no knowledge about actual monad stack - just knows it's the same
  -- as used by the store, which lets us create a lazy effectful streaming structure
   . Monad m
  => MonadIO m
  => Store m HGit
  -> Const HashPointer 'DirTag -- top level interface is just pointers!
  -> Const HashPointer 'DirTag -- top level interface is just pointers!
  -> m [([PartialFilePath], Diff)]
diffMerkleDirs store mt1 mt2 =
  -- transform merkle trees (hash-addressed indirection) into lazily streaming data structures
  -- before passing to diffing alg
  diffMerkleDirs' (lazyDeref store mt1) (lazyDeref store mt2)


-- | Diff two merkle trees where the hash-identified nodes have been
--   converted to lazily-expanded effectful streams of values in some monadic stack 'm'
diffMerkleDirs'
  :: forall m
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => MonadIO m
  => Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
  -> Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
  -> m [([PartialFilePath], Diff)]
diffMerkleDirs' = compareDir []
  where
    compareDir
      :: [PartialFilePath]
      -> Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
      -> Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
      -- todo: writer w/ stack (?) so I can push/path segments to go with changes to tag diffs with loc...
      -> m [([PartialFilePath], Diff)]
    compareDir h dir1 dir2 = do
      ns1' <- dirEntries <$> derefLayer dir1
      ns2' <- dirEntries <$> derefLayer dir2

      fmap join . traverse (resolveMapDiff h)
                $ mapCompare (Map.fromList ns1') (Map.fromList ns2')

    resolveMapDiff
      :: [PartialFilePath]
      -> ( PartialFilePath
         , These (FileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit)))
                 (FileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit)))
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
      -> FileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit))
      -> FileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit))
      -> m [([PartialFilePath], Diff)]
    compareDerefed h path (Left _) (Right _)
      = pure [(h ++ [path], DirReplacedWithFile)]
    compareDerefed h path (Right _) (Left _)
      = pure [(h ++ [path], FileReplacedWithDir)]
    compareDerefed h path (Right fc1) (Right fc2)
      | pointer fc1 /= pointer fc2 = pure [(h ++ [path], FileModified)]
      | otherwise = do
          -- liftIO $ putStrLn $ "compare Right (file) with Right (file) same pointers"
          pure []
    compareDerefed h path (Left dir1) (Left dir2)
      | pointer dir1 == pointer dir2 = pure []
      | otherwise = compareDir (h ++ [path]) dir1 dir2



derefLayer
  :: forall f m
  . NatM m (Term (FC.Compose (LazyHashTagged m) :++ f))
            (f (Term (FC.Compose (LazyHashTagged m) :++ f)))
derefLayer (Term (HC (FC.Compose (C (_p, m))))) = m
