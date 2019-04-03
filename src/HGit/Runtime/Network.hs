{-# LANGUAGE IncoherentInstances #-}

module HGit.Runtime.Network where

--------------------------------------------
import           Control.Exception.Safe (throw)
import qualified Data.Map as M
import           Data.Proxy
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant
import           Servant.Client
--------------------------------------------
import           HGit.Core.Types
import           HGit.Runtime.Capabilities
import           HGit.Runtime.Types
import           Merkle.Store
import           Merkle.Store.FileSystem (fsStore)
import qualified Merkle.Store.Network as MN
import           Merkle.Types (Hash)
--------------------------------------------




type BranchAPI
     = "branches" :> Get '[JSON] [BranchName]
  :<|> "branch"   :> Capture "branchname" BranchName :> Get '[JSON] (Hash HashableCommit)
  :<|> "branch"   :> Capture "branchname" BranchName :> ReqBody '[JSON] (Hash HashableCommit)
                                                     :> Post '[JSON] ()


type HGitStoreAPI
     = "blob"     :> MN.StoreAPI Blob
  :<|> "dir"      :> MN.StoreAPI HashableDir
  :<|> "commit"   :> MN.StoreAPI HashableCommit


type HGitAPI
     = BranchAPI
  :<|> HGitStoreAPI


hgitServer :: HgitStore IO -> Server HGitAPI
hgitServer caps = hgitBranchServer :<|> hgitStoreServer caps

hgitStoreServer :: HgitStore IO -> Server HGitStoreAPI
hgitStoreServer caps
     = MN.server "blob"   (_blobStore   caps)
  :<|> MN.server "dir"    (_dirStore    caps)
  :<|> MN.server "commit" (_commitStore caps)


hgitBranchServer :: Server BranchAPI
hgitBranchServer = listBranches' :<|> getBranchHash :<|> setBranchHash
  where
    listBranches' = fmap (M.keys . branches) readState
    getBranchHash b = readState >>= getBranch' b
    setBranchHash b h = readState >>= (\s -> pure $ s {branches = M.insert b h (branches s)})
                                  >>= writeState

hgitApp :: HgitStore IO -> Application
hgitApp = serve (Proxy :: Proxy HGitAPI) . hgitServer

listBranches :: ClientM [BranchName]
pullBranchHash :: [Char] -> ClientM (Hash HashableCommit)
pushBranchHash :: [Char] -> Hash HashableCommit -> ClientM ()
listBranches :<|> pullBranchHash :<|> pushBranchHash = client  (Proxy :: Proxy BranchAPI)

netStore :: (String, Int) -> IO (HgitStore IO)
netStore (path, port) = do
  m <- newManager defaultManagerSettings
  let env = mkClientEnv m (BaseUrl Http path port "")
      runC :: forall x . ClientM x -> IO x
      runC mx = runClientM mx env >>= either throw pure
  pure $ HgitStore
       { _blobStore   = liftStore runC $ _blobStore netStore'
       , _dirStore    = liftStore runC $ _dirStore netStore'
       , _commitStore = liftStore runC $ _commitStore netStore'
       }

netStore' :: HgitStore ClientM
netStore' = HgitStore
          { _blobStore   = Store dB uB
          , _dirStore    = Store dD uD
          , _commitStore = Store dC uC
          }
  where
    (dB :<|> uB) :<|> (dD :<|> uD) :<|> (dC :<|> uC) = client (Proxy :: Proxy HGitStoreAPI)

mkCaps :: RepoState -> IO (HgitStore IO)
mkCaps state = do
  localCaps <- HgitStore <$> mkStore "blob" <*> mkStore "dir" <*> mkStore "commit"
  case remote state of
    Nothing -> pure localCaps
    Just r  -> do
      netCaps <- netStore r
      pure $ withFallbackRC localCaps netCaps

  where
    mkStore prefix = fsStore . (++ "/" ++ prefix) <$> hgitStore'
