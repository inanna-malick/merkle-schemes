{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where


--------------------------------------------
import qualified Data.HashMap.Strict as Map
import           Data.IORef
--------------------------------------------
import           Compare (compareMerkleTrees)
import           Ingress (buildDirTree)
--------------------------------------------

main :: IO ()
main = do
  globalStateStore <- newIORef Map.empty
  before <- buildDirTree globalStateStore "examples/before" "node1"
  after  <- buildDirTree globalStateStore "examples/after3" "node2"
  res <- compareMerkleTrees globalStateStore before after
  print $ res

