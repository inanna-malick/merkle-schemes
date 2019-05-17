{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Merkle.Store.NetStoreSpec where

--------------------------------------------
import qualified Control.Concurrent               as C
import           Control.Exception.Safe (throw, bracket)
import           Control.Monad.IO.Class
import qualified Data.Aeson as AE
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
import           Merkle.Types
import           Merkle.Types.BlakeHash
import           Util.RecursionSchemes
--------------------------------------------


spec :: Spec
spec = describe "network store served over REST, backed by a file system store" $ do
  let testHarness :: forall f
                   . ( AE.ToJSON1 f
                     , AE.FromJSON1 f
                     , Show1 f
                     , Hashable RawBlakeHash f
                     , Eq1 f
                     , Traversable f
                     )
                  => Gen (Fix f)
                  -> IO ()
      testHarness g = do
        -- use one shared hash-addressed store for all tests
        -- if hash == , then content ==, so safe. Merkle!
        withSystemTempDirectory "proptest" $ \ netpath -> do
          let port = 8081 -- todo random?
          manager <- newManager defaultManagerSettings
          let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
          let netStore' = liftStore (\mx -> liftIO (runClientM mx env) >>= either throw pure) netStore
          bracket (liftIO . C.forkIO . Warp.run port $ app (liftShallowStore $ fsStore netpath :: Store IO RawBlakeHash f))
            C.killThread $ \_ -> do
              -- all this should work fine in parallel
              -- store is hash addressed, so no collisions should matter
              requireProperty $ storeTestDeep g netStore'

  it "round trips recursive dir tree structures with max depth 5" $
    testHarness $ genMockDirTree 5

  it "round trips recursive mock blockchain structures with constant depth 25" $
    testHarness $ genMockBlockchain 25
