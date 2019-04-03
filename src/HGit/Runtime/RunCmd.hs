module HGit.Runtime.RunCmd where

--------------------------------------------
import           Control.Exception.Safe (throw)
import           Control.Monad (void)
import           Control.Monad.Trans.Class
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Functor.Compose
import qualified Data.List as L
import           Data.List.NonEmpty
import qualified Data.Map as M
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Network.Wai.Handler.Warp         as Warp
import           Servant.Client (mkClientEnv, runClientM, Scheme(..), BaseUrl(..), ClientM)
import qualified System.Directory as Dir
--------------------------------------------
import           HGit.Core.Diff (diffMerkleDirs, Diff)
import           HGit.Core.Merge
import           HGit.Core.Types
import           HGit.Runtime.Capabilities
import           HGit.Runtime.Commands
import           HGit.Runtime.FileIO
import           HGit.Runtime.Network
import           HGit.Runtime.Types
import           Util.RecursionSchemes
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Store.Deref
import           Merkle.Types
--------------------------------------------

runServer :: Int -> HgitStore IO -> IO ()
runServer port = Warp.run port . hgitApp

initRepo :: IO ()
initRepo = do
  mkHgitDir -- mk .hgit and .hgit/store dirs
  writeState initialRepoState
  caps  <- mkCaps initialRepoState

  -- initial objects
  void $ sUploadShallow (_commitStore caps) NullCommit
  void $ sUploadShallow (_dirStore    caps) (Dir [])


checkoutBranch :: BranchName -> ReaderT (RepoCaps IO) IO (Maybe RepoState)
checkoutBranch bn = do
  dirStore <- asks (liftStore lift . _dirStore . rcStore)
  commitStore <- asks (liftStore lift . _commitStore . rcStore)

  targetCommit <- getBranch bn >>= sDeref' commitStore
  diffs        <- status

  if not (null diffs)
    then do
      liftIO $ putStrLn "directory modified, cannot checkout. Blocking changes:"
      _ <- traverse printDiff diffs
      pure Nothing
    else do
      currentCommit <- asks (currentBranch . rcState) >>= getBranch >>= sDeref' commitStore
      topLevelCurrentDir <- sDeref' dirStore $ commitRoot currentCommit

      setDirTo topLevelCurrentDir (commitRoot targetCommit)

      asks (Just . (\r -> r { currentBranch = bn
                            }
                    ) . rcState)

pushBranch :: BranchName -> ReaderT (RepoCaps IO) IO (Maybe RepoState)
pushBranch bn = do
  blobStore <- asks (liftStore lift . _blobStore . rcStore)
  dirStore <- asks (liftStore lift . _dirStore . rcStore)
  commitStore <- asks (liftStore lift . _commitStore . rcStore)
  r <- getRemote

  localHash <- getBranch bn

  remoteCaps <- lift $ netStore r

  -- from local store, presumably
  commits <- strictDeref (lazyDeref' commitStore localHash)

  -- upload branch fully (required because remote relationship is not bidirectional)
  -- commits with full deref of blobs and dirs (will be large, todo is incremental download and upload)
  let handleDirs dh = do
        dirs <- strictDeref $ lazyDeref' dirStore dh
        _ <- bitraverseFix handleBlobs $ stripTags dirs

        -- upload to remote store
        _ <- uploadDeep (liftStore lift $ _dirStore remoteCaps) $ stripTags dirs
        pure dh

      handleBlobs bh = do
        blobs <- strictDeref $ lazyDeref' blobStore bh

        -- upload to remote store
        _ <- uploadDeep (liftStore lift $ _blobStore remoteCaps) $ stripTags blobs
        pure bh

  _ <- bitraverseFix handleDirs $ stripTags commits
  _ <- uploadDeep (liftStore lift $ _commitStore remoteCaps) $ stripTags commits

  runC $ pushBranchHash bn localHash
  pure Nothing

-- TODO: pull _and_ checkout
pullBranch :: BranchName -> ReaderT (RepoCaps IO) IO (Maybe RepoState)
pullBranch bn = do
  -- no need to pull full branch structure now (eagerly) because we can do it as-required
  remoteHash <- runC $ pullBranchHash bn
  -- just overwrite without trying to determine ancestry. TODO: not that
  asks (Just . (\r -> r { branches = M.insert bn remoteHash $ branches r }
                ) . rcState)

mkBranch :: BranchName -> ReaderT (RepoCaps IO) IO (Maybe RepoState)
mkBranch bn = do
  current <- asks (currentBranch . rcState) >>= getBranch
  asks (Just . (\r -> r { branches = M.insert bn current $ branches r
                        , currentBranch = bn
                        }
                ) . rcState)

mkCommit :: String -> ReaderT (RepoCaps IO) IO (Maybe RepoState)
mkCommit msg = do
  blobStore <- asks (liftStore lift . _blobStore . rcStore)
  dirStore <- asks (liftStore lift . _dirStore . rcStore)
  commitStore <- asks (liftStore lift . _commitStore . rcStore)
  baseDir <- asks rcBaseDir

  currentCommitHash <- asks (currentBranch . rcState) >>= getBranch

  let uploadBlob  = uploadDeep blobStore
      uploadBlobs = bitraverseFix uploadBlob

  currentStateHash <- liftIO (readTree baseDir) >>= uploadBlobs >>= uploadDeep dirStore

  let commit = Commit msg currentStateHash (pure currentCommitHash)
  rootHash <- sUploadShallow commitStore commit

  asks (Just . (\r -> r { branches = M.insert (currentBranch r) rootHash $ branches r
                        }
                ) . rcState)

mkMergeCommit :: BranchName -> String -> ReaderT (RepoCaps IO) IO (Maybe RepoState)
mkMergeCommit targetBranch msg = do
  dirStore <- asks (liftStore lift . _dirStore . rcStore)
  commitStore <- asks (liftStore lift . _commitStore . rcStore)

  diffs <- status

  if not (null diffs)
    then do
      liftIO $ putStrLn "directory modified, cannot merge. Changes:"
      _ <- traverse printDiff diffs
      pure Nothing
    else do
      targetCommitHash  <- getBranch targetBranch
      currentCommitHash <- asks (currentBranch . rcState) >>= getBranch

      currentCommit <- sDeref' commitStore currentCommitHash
      targetCommit  <- sDeref' commitStore targetCommitHash

      mergeRes <- mergeMerkleDirs dirStore (lazyDeref' dirStore $ commitRoot currentCommit)
                                           (lazyDeref' dirStore $ commitRoot targetCommit)

      case mergeRes of
        Left err -> liftIO . fail $ "merge nonviable due to: " ++ show err
        Right root -> do
          let commit = Commit msg (htPointer root) $ currentCommitHash :| [targetCommitHash]
          rootHash <- sUploadShallow commitStore commit

          topLevelCurrentDir <- sDeref' dirStore $ commitRoot currentCommit

          setDirTo topLevelCurrentDir $ htPointer root

          asks (Just . (\r -> r { branches = M.insert (currentBranch r) rootHash $ branches r
                                }
                       ) . rcState)

getDiff :: BranchName -> BranchName -> ReaderT (RepoCaps IO) IO (Maybe RepoState)
getDiff bn1 bn2 = do
    commitStore <- asks (liftStore lift . _commitStore . rcStore)
    dirStore <- asks (liftStore lift . _dirStore . rcStore)

    commit1   <- getBranch bn1 >>= sDeref' commitStore
    commit2   <- getBranch bn2 >>= sDeref' commitStore
    diffs     <- diffMerkleDirs (lazyDeref' dirStore $ commitRoot commit1)
                                (lazyDeref' dirStore $ commitRoot commit2)
    _ <- traverse printDiff diffs
    pure Nothing

unsetRepo :: ReaderT (RepoCaps IO) IO (Maybe RepoState)
unsetRepo = asks (Just . (\r -> r { remote = Nothing }) . rcState)

setRepo :: String -> Int -> ReaderT (RepoCaps IO) IO (Maybe RepoState)
setRepo path port = asks (Just . (\r -> r { remote = Just (path, port) }) . rcState)


runCommand :: RepoCommand -> ReaderT (RepoCaps IO) IO (Maybe RepoState)
runCommand = \case
  CheckoutBranch bn -> checkoutBranch bn
  PushBranch bn -> pushBranch bn
  PullBranch bn -> pullBranch bn
  MkBranch bn -> mkBranch bn

  SetRemoteRepo path port -> setRepo path port
  UnsetRemoteRepo -> unsetRepo

  MkCommit msg -> mkCommit msg

  -- todo: n-way merge
  MkMergeCommit bn msg -> mkMergeCommit bn msg

  GetStatus -> do
    diffs  <- status

    asks (currentBranch . rcState) >>= liftIO . putStrLn . ("current branch: " ++)
    liftIO $ putStrLn $ "diffs:"
    _ <- traverse printDiff diffs
    pure Nothing

  GetDiff bn1 bn2 -> getDiff bn1 bn2


runC :: ClientM x -> ReaderT (RepoCaps IO) IO x
runC mx = do
  (path, port) <- getRemote
  manager <- liftIO $ newManager defaultManagerSettings
  let env = mkClientEnv manager (BaseUrl Http path port "")
  liftIO $ runClientM mx env >>= either throw pure

printDiff :: MonadIO m => ([FilePath], Diff) -> m ()
printDiff (fps, d) = liftIO . putStrLn $ "\t" ++ show d ++ " at " ++ (L.intercalate "/" fps)

status :: ReaderT (RepoCaps IO) IO [([PartialFilePath], Diff)]
status = do
  baseDir <- asks rcBaseDir
  dirStore <- asks (liftStore lift . _dirStore . rcStore)
  commitStore <- asks (liftStore lift . _commitStore . rcStore)
  currentCommit <- asks (currentBranch . rcState) >>= getBranch >>= sDeref' commitStore

  strictCurrentState  <- liftIO $ readTree baseDir

  -- transform the strict directory tree into a hash-tagged 'lazy' tree.
  -- TODO: better name!
  let currentState = cata (\(Dir xs) ->
                              let xs' = fmap (fmap (first (cata hash))) xs
                                  h   = hash . Dir $ fmap (fmap (fmap htPointer)) xs'
                              in Fix $ Compose (h, Compose $ pure $ Dir xs')
                          ) strictCurrentState

  diffMerkleDirs (lazyDeref' dirStore $ commitRoot currentCommit) currentState


-- x, y, don't matter..
setDirTo :: forall x y. Dir x y -> Hash HashableDir -> ReaderT (RepoCaps IO) IO ()
setDirTo topLevelCurrentDir targetDir = do
  blobStore <- asks (liftStore lift . _blobStore . rcStore)
  dirStore <- asks (liftStore lift . _dirStore . rcStore)
  baseDir <- asks rcBaseDir


  let toDelete = dirEntries topLevelCurrentDir

  -- NOTE: basically only use in a docker container for a bit, lol
  -- delete each top-level entity in the current commit's root dir
  -- we just confirmed that there are no diffs btween it and the current dir state
  let cleanup (p, DirEntity  _) = Dir.removeDirectoryRecursive p
      cleanup (p, FileEntity _) = Dir.removeFile p
  _ <- liftIO $ traverse cleanup toDelete

  -- TODO better names
  x <- fmap stripTags . strictDeref $ lazyDeref' dirStore targetDir
  x' <- bitraverseFix (fmap stripTags . strictDeref . lazyDeref' blobStore) x

  writeTree baseDir x'


commitRoot
  :: HashableCommit x
  -> Hash HashableDir
commitRoot (Commit _ x _) = x
commitRoot NullCommit     = emptyHash
