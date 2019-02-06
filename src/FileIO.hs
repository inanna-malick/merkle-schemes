-- | Functions for interacting with the filesystem to
-- create dir trees from merkle trees or vice versa
module FileIO (readTree, writeTree, writeTree') where

--------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy
import qualified Data.List as List
import qualified System.Directory as Dir
--------------------------------------------
import           Util.MyCompose
import           Util.RecursionSchemes
import           Merkle.Tree.Types
--------------------------------------------

-- | Write tree to file path (using strict tree)
writeTree'
  :: MonadIO m
  => FilePath
  -> Fix $ Named :+ Tree
  -> m ()
writeTree' outdir tree = do
  liftIO $ evalStateT (cata alg tree) [outdir]

  where
    alg :: Algebra (Named :+ Tree) (StateT [FilePath] IO ())
    alg (C (name, (Leaf body)))     = do
      path <- List.intercalate "/" . reverse . (name:) <$> get
      liftIO $ writeFile path body
    alg (C (name, (Node children))) = do
      path <- List.intercalate "/" . reverse . (name:) <$> get
      liftIO $ Dir.createDirectory path
      modify (push name)
      _ <- traverse id children
      modify pop

    push x xs = x:xs
    pop (_:xs)  = xs
    pop []    = []


-- | Write tree to file path (using strict tree)
writeTree
  :: MonadIO m
  => FilePath
  -> Fix $ Named :+ Tree
  -> m ()
writeTree outdir tree = do
  liftIO $ evalStateT (cata alg tree) [outdir]

  where
    alg :: Algebra (Named :+ Tree) (StateT [FilePath] IO ())
    alg (C (name, (Leaf body)))     = do
      path <- List.intercalate "/" . reverse . (name:) <$> get
      liftIO $ writeFile path body
    alg (C (name, (Node children))) = do
      path <- List.intercalate "/" . reverse . (name:) <$> get
      liftIO $ Dir.createDirectory path
      modify (push name)
      _ <- traverse id children
      modify pop

    push x xs = x:xs
    pop (_:xs)  = xs
    pop []    = []

-- | Lazily read some directory tree into memory
readTree
  :: forall m
   . MonadIO m
  => FilePath
  -- tree structure _without_ pointer annotation
  -- type-level guarantee that there is no hash identified
  -- entity indirection allowed here
  -> Fix (m :+ (Named :+ Tree))
readTree = ana alg
  where
    alg :: CoAlgebra (m :+ Named :+ Tree) FilePath
    alg path = C $ do
      -- todo: validation of input file path, ideally some 'probefile :: FilePath -> IO FileType' widget
      isFile <- liftIO $ Dir.doesFileExist path
      if isFile
        then do
          fc <- liftIO $ readFile path
          pure . C . (justTheName path,) $ Leaf fc
        else do
          isDir <- liftIO $ Dir.doesDirectoryExist path
          if isDir
            then fmap ( C
                      . (justTheName path,)
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
