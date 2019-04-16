{-# LANGUAGE TemplateHaskell #-}

module Merkle.Store.NetStoreSpec where

--------------------------------------------
import qualified Control.Concurrent               as C
import           Control.Exception.Safe (throw, bracket)
import           Control.Monad.IO.Class
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Network.Wai.Handler.Warp         as Warp
import           Servant.Client (mkClientEnv, runClientM, Scheme(..), BaseUrl(..))
import           System.IO.Temp
--------------------------------------------
import           Hedgehog
import           HaskellWorks.Hspec.Hedgehog
import           Test.Hspec
--------------------------------------------
import           Data.Aeson.Orphans ()
import           Merkle.Store
import           Merkle.Store.FileSystem
import           Merkle.Store.Network
import           Merkle.Store.TestUtils
--------------------------------------------


spec :: Spec
spec = describe "network store" $ do
  it "round trips recursive dir tree structures with max depth 5" $ do

    -- use one shared hash-addressed store for all tests
    -- if hash == , then content ==, so safe. Merkle!
    withSystemTempDirectory "proptest" $ \ fspath ->
          requireProperty $ storeTestDeep (genMockDirTree 5) $ fsStore fspath
    withSystemTempDirectory "proptest" $ \ netpath -> do
      let port = 8081 -- todo random?
      manager <- newManager defaultManagerSettings
      let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
      let netStore' :: Store (PropertyT IO) MockDirectoryTree
          netStore' = liftStore (\mx -> liftIO (runClientM mx env) >>= either throw pure) netStore
      bracket (liftIO . C.forkIO . Warp.run port $ app (fsStore netpath :: Store IO MockDirectoryTree))
        C.killThread $ \_ -> do
          -- all this should work fine in parallel
          -- store is hash addressed, so no collisions should matter
          requireProperty $ storeTestDeep (genMockDirTree 5) netStore'
