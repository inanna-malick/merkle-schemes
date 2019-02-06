-- | Functions for interacting with the filesystem to
-- create dir trees from merkle trees or vice versa
module FileIO (buildDirTree, outputDirTree) where

--------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy
import qualified Data.List as List
import qualified System.Directory as Dir
--------------------------------------------
import           Deref
import           Util.MyCompose
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types (Pointer)
import           Store
--------------------------------------------

-- | Write tree to file path
outputDirTree
  :: MonadIO m
  => Store m
  -> FilePath
  -> Pointer
  -> m ()
outputDirTree store outdir p = do
  derefed <- strictDeref store p
  liftIO $ evalStateT (cata alg derefed) [outdir]

  where
    alg :: Algebra (WithHash :+ NamedTreeLayer) (StateT [FilePath] IO ())
    alg (C (_p, (C (Named name (Leaf body)))))     = do
      path <- List.intercalate "/" . reverse . (name:) <$> get
      liftIO $ writeFile path body
    alg (C (_p, (C (Named name (Node children))))) = do
      path <- List.intercalate "/" . reverse . (name:) <$> get
      liftIO $ Dir.createDirectory path
      modify (push name)
      _ <- traverse id children
      modify pop

    push x xs = x:xs
    pop (_:xs)  = xs
    pop []    = []


-- | actual dir recursive traversal
-- ignores permissions in diffs (all file permissions are, idk, that of running process?)
-- and coerces file contents into unicode #YOLO. Reads directory structure into memory
-- and annotates nodes with their hashes
buildDirTree
  :: MonadIO m
  => Store m
  -> FilePath
  -> m MerkleTree
buildDirTree store = addDirTreeToStore store . buildDirTree'

-- | annotate tree nodes with hash, adding them to some global store during this traversal
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
addDirTreeToStore
  :: forall m
   . Monad m
  => Store m
  -> Fix $ m :+ NamedTreeLayer
  -> m MerkleTree
addDirTreeToStore store = cata alg
  where
    alg :: Algebra (m :+ NamedTreeLayer) (m MerkleTree)
    alg (C getEntity) = do
      (C (Named name entity')) <- getEntity
      entity <- Named name <$> case entity' of
        Leaf body -> pure $ Leaf body
        Node children -> do
          children' <- traverse id children
          pure $ Node children'
      p <- sUploadShallow store $ makeShallow $ C entity
      pure . Fix . C . (p,) . C . Just $ C entity

buildDirTree'
  :: forall m
   . MonadIO m
  => FilePath
  -- tree structure _without_ pointer annotation
  -- type-level guarantee that there is no hash identified
  -- entity indirection allowed here
  -> Fix (m :+ NamedTreeLayer)
buildDirTree' = ana alg
  where
    alg :: CoAlgebra (m :+ NamedTreeLayer) FilePath
    alg path = C $ do
      -- todo: validation of input file path, ideally some 'probefile :: FilePath -> IO FileType' widget
      isFile <- liftIO $ Dir.doesFileExist path
      if isFile
        then do
          fc <- liftIO $ readFile path
          pure . C . Named (justTheName path) $ Leaf fc
        else do
          isDir <- liftIO $ Dir.doesDirectoryExist path
          if isDir
            then fmap ( C
                      . Named (justTheName path)
                      . Node
                      . fmap (\x -> path ++ "/" ++ x)
                      . filter (/= ".")
                      . filter (/= "..")
                      )
               . liftIO
               $ Dir.getDirectoryContents path
            else fail ("file read error: unexpected type at " ++ path)

    justTheName :: String -> String -- hacky hax but it works - take just the name given a file path
    justTheName = reverse . takeWhile (/= '/') . reverse
