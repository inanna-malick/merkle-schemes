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
  after1 <- buildDirTree globalStateStore "examples/after1" "node1"
  after2 <- buildDirTree globalStateStore "examples/after2" "node1"
  after3 <- buildDirTree globalStateStore "examples/after3" "node2"

  putStrLn "comparing before to after1"
  compareMerkleTrees globalStateStore before after1 >>= print

  putStrLn "comparing before to after2"
  compareMerkleTrees globalStateStore before after2 >>= print

  putStrLn "comparing before to after3"
  compareMerkleTrees globalStateStore before after3 >>= print
