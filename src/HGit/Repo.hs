module HGit.Repo where


--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified System.Directory as Dir
--------------------------------------------
import           Errors
import           HGit.Types.HGit
import           HGit.Types.RepoState
import           Merkle.Types
import           Merkle.Store
import           Merkle.Store.FileSystem (fsStore)
--------------------------------------------


hgitDir, hgitStateFile, hgitStoreDir :: PartialFilePath
hgitDir = ".hgit"
hgitStateFile = "state.json"
hgitStoreDir = "store"

-- TODO: dir tree traversal to allow for running app in non-root repo dir? mb
hgitDir', hgitState', hgitStore', baseDir :: MonadIO m => m FilePath
baseDir    = liftIO Dir.getCurrentDirectory
hgitDir'   = (++ "/" ++ hgitDir)       <$> baseDir
hgitState' = (++ "/" ++ hgitStateFile) <$> hgitDir'
hgitStore' = (++ "/" ++ hgitStoreDir)  <$> hgitDir'


mkHgitDir :: MonadIO m => m ()
mkHgitDir = do
  hgitDir' >>= liftIO . Dir.createDirectory
  storeDir <- hgitStore'
  liftIO $ Dir.createDirectory storeDir
  void $ traverse (liftIO . Dir.createDirectory . (\x -> storeDir ++ "/" ++ x)) ["dir", "blob", "commit"]

-- | get branch from state, fail if not found
getBranch
  :: MonadThrow m
  => BranchName
  -> RepoState
  -> m (Hash HashableCommit)
getBranch b
  = maybe (throw $ BranchNotFound b) pure . M.lookup b . branches

-- | get remote addr from state, fail if not found
getRemote
  :: MonadThrow m
  => RepoState
  -> m (String, Int)
getRemote = maybe (throw RemoteNotFound) pure . remote


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

data RepoCaps m
  = RepoCaps
  { _blobStore   :: Store m Blob
  , _dirStore    :: Store m HashableDir
  , _commitStore :: Store m HashableCommit
  }

withFallbackRC :: Monad m => RepoCaps m -> RepoCaps m -> RepoCaps m
withFallbackRC main fallback
  = RepoCaps
  { _blobStore   = withFallback (_blobStore main)   (_blobStore fallback)
  , _dirStore    = withFallback (_dirStore main)    (_dirStore fallback)
  , _commitStore = withFallback (_commitStore main) (_commitStore fallback)
  }

-- TODO: create dir structure?
mkLocalCaps :: IO (RepoCaps IO)
mkLocalCaps = RepoCaps <$> mkStore "blob" <*> mkStore "dir" <*> mkStore "commit"
  where mkStore prefix = fsStore . (++ "/" ++ prefix) <$> hgitStore'
