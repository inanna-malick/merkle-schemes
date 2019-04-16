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
spec = describe "pure in-memory store" $ do
  it "round trips recursive dir tree structures with max depth 5" $ do

    -- use one shared hash-addressed store for all tests
    -- if hash == , then content ==, so safe. Merkle!
    ior <- newIORef M.empty
    requireProperty $ storeTestDeep (genMockDirTree 5) $ inMemoryStore ior
