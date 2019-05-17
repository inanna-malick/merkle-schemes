{-# LANGUAGE TemplateHaskell #-}

module Merkle.Store.InMemoryStoreSpec where

--------------------------------------------
import           Data.IORef
import qualified Data.Map.Strict as M
--------------------------------------------
import           HaskellWorks.Hspec.Hedgehog
import           Test.Hspec
import           Merkle.Store.TestUtils
--------------------------------------------
import           Merkle.Store.InMemory
--------------------------------------------

spec :: Spec
spec = describe "in-memory store (IORef backed)" $ do
  let testHarness g = do
        -- use one shared hash-addressed store for all tests
        -- if hash == , then content ==, so safe. Merkle!
        ior <- newIORef M.empty
        requireProperty $ storeTestDeep g $ inMemoryStore ior

  it "round trips recursive dir tree structures with max depth 5" $
    testHarness $ genMockDirTree 5


  it "round trips recursive mock blockchain structures with constant depth 25" $
    testHarness $ genMockBlockchain 25
