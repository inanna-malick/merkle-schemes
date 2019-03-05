-- | Functions for interacting with the filesystem to
-- create dir trees from merkle trees or vice versa
module FileIO where -- (readTree, writeTree, writeTree') where

--------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy
import qualified Data.List as List
import           Data.Foldable (traverse_)
import qualified Data.Functor.Compose as FC
import           Data.Functor.Const
import           Data.Singletons
import qualified System.Directory as Dir
--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes
import           HGit.Store
import           HGit.Types
--------------------------------------------

-- | Write strict hgit dirtree to file path
writeTree
  :: MonadIO m
  => FilePath
  -> Term HGit 'DirTag
  -> m ()
writeTree outdir tree = do
  liftIO $ evalStateT (getConst $ sCata alg tree) [outdir]

  where
    alg :: SAlg HGit (Const $ StateT [FilePath] IO ())
    alg (Blob contents)    = Const $ do -- append - will open file handle multiple times, w/e, can cache via state later
      path <- List.intercalate "/" . reverse <$> get
      liftIO $ appendFile path contents

    alg (BlobTree children) = Const $ traverse_ getConst children

    alg (Dir children) = Const $ do -- mkdir
      let mkDir = do
            path <- List.intercalate "/" . reverse <$> get
            liftIO $ Dir.createDirectory path
          touch = do
            path <- List.intercalate "/" . reverse <$> get
            liftIO $ writeFile path "" -- touch file
          handle (pathChunk, e) = do
            modify (push pathChunk)
            either (\_ -> mkDir) (\_ -> touch) e
            either getConst getConst e
            modify pop
      traverse_ handle children

    -- unreachable
    alg _ = Const $ fail "*sarcastic air quotes* unreachable *end sarcastic air quotes*"

    push x xs = x:xs
    pop (_:xs)  = xs
    pop []    = []


readAndStore
  :: forall m
   . MonadIO m
  => Store m HGit
  -> FilePath
  -> m (Const HashPointer 'DirTag)
readAndStore store = fmap Const . getConst . cata alg . readTree
  where
    alg :: Alg (FC.Compose m :++ HGit) (Const (m HashPointer))
    alg (HC (FC.Compose eff)) = Const $ do
      e <- eff
      -- upload sub-trees and get hashes
      let f :: forall i . HGit (Const (m HashPointer)) i -> m (HGit (Const (HashPointer)) i)
          f = \case
                Blob x -> pure $ Blob x
                BlobTree xs -> do
                  xs' <- traverse (fmap Const . getConst) xs
                  pure $ BlobTree xs'
                Dir xs -> do
                  xs' <- traverse (\(n, et) ->
                                    (n,) <$> either (fmap (Left . Const) . getConst)
                                                    (fmap (Right . Const) . getConst)
                                                     et
                                  ) xs
                  pure $ Dir xs'
                Commit msg a b -> do
                  a' <- getConst a
                  b' <- getConst b
                  pure $ Commit msg (Const a') (Const b')
                NullCommit ->
                  pure $ NullCommit
      e' <- f e

      -- hax (todo chain through)
      fmap getConst $ sUploadShallow store e'

-- | Lazily read some directory tree into memory
-- NOTE: this needs to be a futu so file steps can read the full file into memory instead of recursing into filepointers (note 2: is this till true? idk lol)
readTree
  :: forall m
   . MonadIO m
  => FilePath
  -- tree structure _without_ pointer annotation
  -- type-level guarantee that there is no hash identified
  -- entity indirection allowed here
  -> Term (FC.Compose m :++ HGit) 'DirTag
readTree = sFutu alg . Const
  where
    alg :: SCVCoalg (FC.Compose m :++ HGit) (Const FilePath)
    alg (Const path) = readTree' sing path

readTree'
  :: forall x m . MonadIO m => Sing x -> FilePath
  -> (FC.Compose m :++ HGit) (Cxt Hole (FC.Compose m :++ HGit) (Const FilePath)) x
readTree' s path = HC $ FC.Compose $ case s of
      SFileChunkTag -> liftIO $ fmap Blob $ readFile path
      SDirTag -> do
          -- liftIO $ putStrLn $ "readTree' path: " ++ path
          dirContents  <- liftIO $ Dir.getDirectoryContents path
          let dirContents'
                = fmap (\x -> path ++ "/" ++ x)
                -- ignore all starting with '.' (eg ..,.,.hgit/)
                . filter (\fn -> take 1 fn /= ".")
                $ dirContents
          dirContents'' <- traverse categorize dirContents'
          pure $ Dir $ fmap (\(p,e) -> (justTheName p, e)) dirContents''

      -- unreachable
      _ -> fail ("quote 'unreachable' unquote")

  where
    categorize p = do
      isFile <- liftIO $ Dir.doesFileExist p
      if isFile
        then pure $ (justTheName p, Right $ Hole $ Const p)
        else do
          isDir <- liftIO $ Dir.doesDirectoryExist p
          if isDir
            then pure $ (justTheName p, Left $ Hole $ Const p)
            else fail ("file read error: unexpected type at " ++ p)


justTheName :: FilePath -> String -- hacky hax but it works - take just the name given a file path
justTheName = reverse . takeWhile (/= '/') . reverse
