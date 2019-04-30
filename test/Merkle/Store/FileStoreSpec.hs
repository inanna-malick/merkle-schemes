{-# LANGUAGE TemplateHaskell #-}

module Merkle.Store.FileStoreSpec where

--------------------------------------------
import           System.IO.Temp
--------------------------------------------
import           HaskellWorks.Hspec.Hedgehog
import           Test.Hspec
import           Merkle.Store.TestUtils
--------------------------------------------
import           Data.Aeson.Orphans ()
import           Merkle.Store
import           Merkle.Store.FileSystem
--------------------------------------------

spec :: Spec
spec = describe "file system backed store" $ do
  let testHarness g = do
        -- use one shared hash-addressed store for all tests
        -- if hash == , then content ==, so safe. Merkle!
        withSystemTempDirectory "proptest" $ \ fspath ->
              requireProperty $ storeTestDeep g $ liftShallowStore $ fsStore fspath

  it "round trips recursive dir tree structures with max depth 5" $
    testHarness $ genMockDirTree 5

  it "round trips recursive mock blockchain structures with constant depth 25" $
    testHarness $ genMockBlockchain 25
