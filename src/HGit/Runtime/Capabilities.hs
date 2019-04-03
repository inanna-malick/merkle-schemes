module HGit.Runtime.Capabilities where


--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throwString)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified System.Directory as Dir
--------------------------------------------
import           HGit.Core.Types
import           HGit.Runtime.Types
import           Merkle.Types
import           Merkle.Store
import           Merkle.Store.FileSystem (fsStore)
--------------------------------------------
import           Control.Monad.Reader


hgitDir, hgitStateFile, hgitStoreDir :: PartialFilePath
hgitDir = ".hgit"
hgitStateFile = "state.json"
hgitStoreDir = "store"

-- TODO: dir tree traversal to allow for running app in non-root repo dir? mb
hgitDir', hgitState', hgitStore', hgitBaseDir :: MonadIO m => m FilePath
hgitBaseDir    = liftIO Dir.getCurrentDirectory
hgitDir'   = (++ "/" ++ hgitDir)       <$> hgitBaseDir
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
  :: MonadThrow m  => MonadReader (RepoCaps m') m
  => BranchName -> m (Hash HashableCommit)
getBranch bn
  = asks rcState >>= getBranch' bn

-- | get branch from state, fail if not found
getBranch'
  :: MonadThrow m => BranchName -> RepoState -> m (Hash HashableCommit)
getBranch' bn
  = maybe (throwString $ "branch not found: " ++ bn) pure . M.lookup bn . branches

-- | get remote addr from state, fail if not found
getRemote
  :: MonadThrow m
  => MonadReader (RepoCaps m') m
  => m (String, Int)
getRemote = asks (remote . rcState) >>= maybe (throwString "remote node addr not foudn") pure

-- | Filesystem backed store using the provided dir
readState
  :: MonadIO m
  => MonadThrow m
  => m RepoState
readState = do
  path <- hgitState'
  liftIO $ AE.eitherDecodeFileStrict path >>= \case
    Left e  -> throwString e
    Right x -> pure x

writeState
  :: MonadIO m
  => MonadThrow m
  => RepoState
  -> m ()
writeState rs = do
  path <- hgitState'
  liftIO . B.writeFile path $ AE.encode rs

data HgitStore m
  = HgitStore
  { _blobStore   :: Store m Blob
  , _dirStore    :: Store m HashableDir
  , _commitStore :: Store m HashableCommit
  }

withFallbackRC :: Monad m => HgitStore m -> HgitStore m -> HgitStore m
withFallbackRC main fallback
  = HgitStore
  { _blobStore   = withFallback (_blobStore main)   (_blobStore fallback)
  , _dirStore    = withFallback (_dirStore main)    (_dirStore fallback)
  , _commitStore = withFallback (_commitStore main) (_commitStore fallback)
  }

-- TODO: create dir structure?
mkLocalCaps :: IO (HgitStore IO)
mkLocalCaps = HgitStore <$> mkStore "blob" <*> mkStore "dir" <*> mkStore "commit"
  where mkStore prefix = fsStore . (++ "/" ++ prefix) <$> hgitStore'


data RepoCaps m
  = RepoCaps
  { rcStore :: HgitStore m
  , rcState :: RepoState
  , rcBaseDir :: FilePath
  }
