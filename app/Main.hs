module Main where

--------------------------------------------
import           Data.Functor.Compose
import qualified Data.List as L
import           Data.List.NonEmpty
import qualified Data.Map as M
import qualified System.Directory as Dir
--------------------------------------------
import           Commands
import           FileIO
import           HGit.Diff (diffMerkleDirs)
import           HGit.Repo
import           HGit.Types.HGit
import           HGit.Types.RepoState
import           HGit.Merge
import           HGit.Network
import           Util.RecursionSchemes
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Store.Deref
import           Merkle.Types
--------------------------------------------
import Data.Bifunctor
import qualified Network.Wai.Handler.Warp         as Warp
import           Control.Exception.Safe                (throw)
import           Servant.Client (mkClientEnv, runClientM, Scheme(..), BaseUrl(..))
import Network.HTTP.Client (newManager, defaultManagerSettings)


-- | TODO HERE
-- 1. fn for making/working with all relevant stores (put in RepoState)

main :: IO ()
main = do
  cmd       <- parse
  case cmd of
    (Left InitRepo) -> initRepo
    (Left (InitServer port)) -> do
      caps  <- mkLocalCaps
      Warp.run port $ hgitApp caps
    (Right repoCmd) -> do
      base  <- baseDir
      state <- readState
      caps  <- mkCaps state
      mNextState <- runCommand base caps state repoCmd
      maybe (pure ()) writeState mNextState


initRepo :: IO ()
initRepo = do
  mkHgitDir -- mk .hgit and .hgit/store dirs
  writeState initialRepoState

runCommand :: FilePath -> RepoCaps IO -> RepoState -> RepoCommand -> IO (Maybe RepoState)
runCommand base caps repostate = \case
  CheckoutBranch branch -> do
    targetCommit <- getBranch branch repostate >>= sDeref' (_commitStore caps)
    diffs        <- status

    if not (null diffs)
      then do
        putStrLn "directory modified, cannot checkout. Blocking changes:"
        _ <- traverse renderDiff diffs
        pure Nothing
      else do
        currentCommit <- getBranch (currentBranch repostate) repostate >>= sDeref' (_commitStore caps)
        topLevelCurrentDir <- sDeref' (_dirStore caps) $ commitRoot currentCommit

        setDirTo topLevelCurrentDir (commitRoot targetCommit)
        pure $ Just repostate { currentBranch = branch }


  SetRemoteRepo path port -> pure $ Just repostate { remote = Just (path, port)}
  UnsetRemoteRepo         -> pure $ Just repostate { remote = Nothing}

  PullBranch bn -> do
    r <- getRemote repostate
    -- just overwrite without trying to determine ancestry. TODO: not that
    runC r $ do
      -- no need to pull full branch structure now (eagerly) because we can do it as-required
      remoteHash <- pullBranchHash bn
      pure $ Just repostate { branches = M.insert bn remoteHash $ branches repostate }

  PushBranch bn -> do
    r <- getRemote repostate
    localHash <- getBranch bn repostate

    remoteCaps <- netStore r

    -- from local store, presumably
    commits <- strictDeref (lazyDeref' (_commitStore caps) localHash)

    -- upload branch fully (required because remote relationship is not bidirectional)
    -- commits with full deref of blobs and dirs (will be large, todo is incremental download and upload)
    let handleDirs dh = do
          dirs <- strictDeref $ lazyDeref' (_dirStore caps) dh
          _ <- bitraverseFix handleBlobs $ stripTags dirs

          -- upload to remote store
          _ <- uploadDeep (_dirStore remoteCaps) $ stripTags dirs
          pure dh

        handleBlobs bh = do
          blobs <- strictDeref $ lazyDeref' (_blobStore caps) bh

          -- upload to remote store
          _ <- uploadDeep (_blobStore remoteCaps) $ stripTags blobs
          pure bh

    _ <- bitraverseFix handleDirs $ stripTags commits
    _ <- uploadDeep (_commitStore remoteCaps) $ stripTags commits

    runC r $ pushBranchHash bn localHash
    pure Nothing

  MkBranch branch -> do
    current   <- getBranch (currentBranch repostate) repostate
    pure $ Just $ repostate
         { branches      = M.insert branch current $ branches repostate
         , currentBranch = branch
         }

  MkCommit msg -> do
    currentCommitHash <- getBranch (currentBranch repostate) repostate

    let uploadBlob  = uploadDeep $ _blobStore caps
        uploadBlobs = bitraverseFix uploadBlob

    currentStateHash <- readTree base >>= uploadBlobs >>= uploadDeep (_dirStore caps)

    let commit = Commit msg currentStateHash (pure currentCommitHash)
    rootHash <- sUploadShallow (_commitStore caps) commit
    pure $ Just $ repostate
         { branches = M.insert (currentBranch repostate) rootHash $ branches repostate
         }

  -- todo: n-way branch merge once I figure out UX
  MkMergeCommit targetBranch msg -> do
    diffs <- status

    if not (null diffs)
      then do
        putStrLn "directory modified, cannot merge. Changes:"
        _ <- traverse renderDiff diffs
        fail "womp womp"
      else do
        targetCommitHash  <- getBranch targetBranch repostate
        currentCommitHash <- getBranch (currentBranch repostate) repostate

        currentCommit <- sDeref' (_commitStore caps) currentCommitHash
        targetCommit <- sDeref' (_commitStore caps) targetCommitHash

        mergeRes <- mergeMerkleDirs (_dirStore caps) (commitRoot currentCommit) (commitRoot targetCommit)

        case mergeRes of
          Left err -> fail $ "merge nonviable due to: " ++ show err
          Right root -> do
            let commit = Commit msg (htPointer root) $ currentCommitHash :| [targetCommitHash]
            rootHash <- sUploadShallow (_commitStore caps) commit

            topLevelCurrentDir <- sDeref' (_dirStore caps) $ commitRoot currentCommit
            setDirTo topLevelCurrentDir $ htPointer root

            pure $ Just $ repostate
                       { branches = M.insert (currentBranch repostate) rootHash
                                  $ branches repostate
                       }

  GetStatus -> do
    diffs  <- status

    putStrLn $ "current branch: " ++ currentBranch repostate
    putStrLn $ "diffs:"
    _ <- traverse renderDiff diffs
    pure Nothing

  GetDiff branch1 branch2 -> do
    commit1   <- getBranch branch1 repostate >>= sDeref' (_commitStore caps)
    commit2   <- getBranch branch2 repostate >>= sDeref' (_commitStore caps)
    diffs     <- diffMerkleDirs (lazyDeref' (_dirStore caps) $ commitRoot commit1)
                                (lazyDeref' (_dirStore caps) $ commitRoot commit2)
    _ <- traverse renderDiff diffs
    pure Nothing

  where
    runC (path, port) mx = do
      manager <- newManager defaultManagerSettings
      let env = mkClientEnv manager (BaseUrl Http path port "")
      runClientM mx env >>= either throw pure

    renderDiff (fps, d) = putStrLn $ "\t" ++ show d ++ " at " ++ (L.intercalate "/" fps)

    status = do
      currentCommit <- getBranch (currentBranch repostate) repostate >>= sDeref' (_commitStore caps)
      strictCurrentState  <- readTree base

      -- transform the strict directory tree into a hash-tagged 'lazy' tree.
      -- TODO: better name!
      let currentState :: Fix (HashAnnotated (Dir (Hash Blob)) `Compose` IO `Compose` Dir (Hash Blob))
          currentState = cata (\(Dir xs) ->
                                  let xs' = fmap (fmap (first (cata hash))) xs
                                      h   = hash . Dir $ fmap (fmap (fmap htPointer)) xs'
                                  in Fix $ Compose (h, Compose $ pure $ Dir xs')
                              ) strictCurrentState

      diffMerkleDirs (lazyDeref' (_dirStore caps) $ commitRoot currentCommit) currentState


    -- x, y, don't matter..
    setDirTo :: forall x y. Dir x y -> Hash HashableDir -> IO ()
    setDirTo topLevelCurrentDir targetDir = do
      let toDelete = dirEntries topLevelCurrentDir

      -- NOTE: basically only use in a docker container for a bit, lol
      -- delete each top-level entity in the current commit's root dir
      -- we just confirmed that there are no diffs btween it and the current dir state
      let cleanup (p, DirEntity  _) = Dir.removeDirectoryRecursive p
          cleanup (p, FileEntity _) = Dir.removeFile p
      _ <- traverse cleanup toDelete

      x <- fmap stripTags . strictDeref $ lazyDeref' (_dirStore caps) targetDir

      let f = fmap stripTags . strictDeref . lazyDeref' (_blobStore caps )

      x' <-  bitraverseFix f x

      writeTree base x'


commitRoot
  :: HashableCommit x
  -> Hash HashableDir
commitRoot (Commit _ x _) = x
commitRoot NullCommit     = emptyHash
