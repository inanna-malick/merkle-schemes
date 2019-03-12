module Main where

--------------------------------------------
import qualified Data.List as L
import           Data.List.NonEmpty
import qualified Data.Map as M
import qualified System.Directory as Dir
--------------------------------------------
import           Commands
import           FileIO
import           HGit.Diff (diffMerkleDirs)
import           HGit.Repo
import           HGit.Types
import           HGit.Merge
import           Util.HRecursionSchemes
import           HGit.Serialization
import           Merkle.Store
import           Merkle.Store.Deref
import           Merkle.Types
--------------------------------------------

main :: IO ()
main = parse >>= \case
  InitRepo -> do
    mkHgitDir -- mk .hgit and .hgit/store dirs
    writeState initialRepoState

  CheckoutBranch branch _ -> do
    store        <- mkStore
    base         <- baseDir
    repostate    <- readState

    targetCommit <- getBranch branch repostate >>= sDeref store

    diffs        <- status base repostate store
    if not (null diffs)
      then do
        putStrLn "directory modified, cannot checkout. Changes:"
        _ <- traverse renderDiff diffs
        fail "womp womp"
      else do
        currentCommit <- getBranch (currentBranch repostate) repostate >>= sDeref store
        topLevelCurrentDir <- sDeref store $ commitRoot currentCommit

        setDirTo store base topLevelCurrentDir (commitRoot targetCommit)
        writeState $ repostate
                  { currentBranch = branch
                  }

  MkBranch branch -> do
    repostate <- readState
    current   <- getBranch (currentBranch repostate) repostate
    writeState $ repostate
               { branches      = M.insert branch current $ branches repostate
               , currentBranch = branch
               }

  MkCommit msg -> do
    store             <- mkStore
    repostate         <- readState
    base              <- baseDir
    currentCommitHash <- getBranch (currentBranch repostate) repostate

    currentStateHash <- readTreeStrict base >>= uploadDeep store

    let commit = Commit msg currentStateHash (pure currentCommitHash)
    hash <- sUploadShallow store commit
    writeState $ repostate
               { branches = M.insert (currentBranch repostate) hash $ branches repostate
               }

  -- todo: n-way branch merge once I figure out UX
  MkMergeCommit targetBranch msg -> do
    store             <- mkStore
    repostate         <- readState
    base              <- baseDir

    diffs        <- status base repostate store
    if not (null diffs)
      then do
        putStrLn "directory modified, cannot merge. Changes:"
        _ <- traverse renderDiff diffs
        fail "womp womp"
      else do

        targetCommitHash  <- getBranch targetBranch repostate
        currentCommitHash <- getBranch (currentBranch repostate) repostate

        currentCommit <- sDeref store currentCommitHash
        targetCommit <- sDeref store targetCommitHash

        mergeRes <- mergeMerkleDirs store (commitRoot currentCommit) (commitRoot targetCommit)

        case mergeRes of
          Left err -> fail $ "merge nonviable due to: " ++ show err
          Right root -> do
            let commit = Commit msg (pointer root) $ currentCommitHash :| [targetCommitHash]
            hash <- sUploadShallow store commit
            writeState $ repostate
                       { branches = M.insert (currentBranch repostate) hash $ branches repostate
                       }


            topLevelCurrentDir <- sDeref store $ commitRoot currentCommit
            setDirTo store base topLevelCurrentDir $ pointer root

  GetStatus -> do
    store     <- mkStore
    repostate <- readState
    base      <- baseDir
    diffs     <- status base repostate store

    putStrLn $ "current branch: " ++ currentBranch repostate
    putStrLn $ "diffs:"
    _ <- traverse renderDiff diffs
    pure ()

  GetDiff branch1 branch2 -> do
    store     <- mkStore
    repostate <- readState
    commit1   <- getBranch branch1 repostate >>= sDeref store
    commit2   <- getBranch branch2 repostate >>= sDeref store
    diffs     <- diffMerkleDirs (lazyDeref store $ commitRoot commit1)
                                (lazyDeref store $ commitRoot commit2)
    _ <- traverse renderDiff diffs
    pure ()

  where
    renderDiff (fps, d) = putStrLn $ "\t" ++ show d ++ " at " ++ (L.intercalate "/" fps)

    status base repostate store = do
      currentCommit <- getBranch (currentBranch repostate) repostate >>= sDeref store
      strictCurrentState  <- readTreeStrict base
      let currentState = makeLazy $ hashTag structuralHash strictCurrentState
      diffMerkleDirs (lazyDeref store $ commitRoot currentCommit) currentState



    setDirTo store base topLevelCurrentDir targetDir = do
      let toDelete = dirEntries topLevelCurrentDir

      -- NOTE: basically only use in a docker container for a bit, lol
      -- delete each top-level entity in the current commit's root dir
      -- we just confirmed that there are no diffs btween it and the current dir state
      let cleanup (p, DirEntity  _) = Dir.removeDirectoryRecursive p
          cleanup (p, FileEntity _) = Dir.removeFile p
      _ <- traverse cleanup toDelete

      x <- makeStrict $ lazyDeref store targetDir
      writeTree base $ stripTags x


-- IDEA: use 'Pair (Const HashPointer) f' instead of (,) HashPointer :+ f
commitRoot
  :: forall x
   . HGit (Term (HashTagged x)) 'CommitTag
  -> Const HashPointer 'DirTag
commitRoot (Commit _ (Term (Pair p _)) _) = p
commitRoot NullCommit        = emptyDirHash
