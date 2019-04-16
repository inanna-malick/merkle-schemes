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
import           Merkle.Store.FileSystem
--------------------------------------------

spec :: Spec
spec = describe "file system store" $ do
  it "round trips recursive dir tree structures with max depth 5" $ do

    -- use one shared hash-addressed store for all tests
    -- if hash == , then content ==, so safe. Merkle!
    withSystemTempDirectory "proptest" $ \ fspath ->
          requireProperty $ storeTestDeep (genMockDirTree 5) $ fsStore fspath
