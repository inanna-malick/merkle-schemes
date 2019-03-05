module HGit.Repo where -- (writeState, readState, mkStore, getBranch) where



--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as B
import           Data.Functor.Const
import qualified Data.Map as M
import qualified System.Directory as Dir
--------------------------------------------
import           Errors
import           HGit.Types
import           HGit.Store
import           HGit.Store.FileSystem (fsStore)
--------------------------------------------


hgitDir, hgitStateFile, hgitStoreDir :: PartialFilePath
hgitDir = ".hgit"
hgitStateFile = "state"
hgitStoreDir = "store"

-- TODO: dir tree traversal to allow for running app in non-root repo dir? mb
hgitDir', hgitState', hgitStore', baseDir :: MonadIO m => m FilePath
baseDir    = liftIO Dir.getCurrentDirectory
hgitDir'   = (++ "/" ++ hgitDir)       <$> baseDir
hgitState' = (++ "/" ++ hgitStateFile) <$> hgitDir'
hgitStore' = (++ "/" ++ hgitStoreDir)  <$> hgitDir'

mkHgitDir :: MonadIO m => m ()
mkHgitDir = do
  path1 <- hgitDir'
  path2 <- hgitStore'
  liftIO $ Dir.createDirectory path1
  liftIO $ Dir.createDirectory path2

mkStore
  :: MonadIO m
  => MonadThrow m
  => m (Store m HGit)
mkStore = fsStore <$> hgitStore'


-- | get branch from state, fail if not found
getBranch
  :: MonadThrow m
  => BranchName
  -> RepoState
  -> m (Const HashPointer 'CommitTag)
getBranch b
  = maybe (throw $ BranchNotFound b) (pure . Const) . M.lookup b . branches

-- | Filesystem backed store using the provided dir
readState
  :: MonadIO m
  => MonadThrow m
  => m RepoState
readState = do
  path <- hgitState'
  contents <- liftIO $ B.readFile path
  case (AE.eitherDecode contents) of
    Left e  -> throw $ DecodeError e
    Right x -> pure x

writeState
  :: MonadIO m
  => MonadThrow m
  => RepoState
  -> m ()
writeState rs = do
  path <- hgitState'
  liftIO . B.writeFile path $ AE.encode rs
