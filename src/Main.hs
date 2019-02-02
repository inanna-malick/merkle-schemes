{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where


--------------------------------------------
import           Control.Monad.Except (runExceptT, liftIO)
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

  res <- runExceptT $ do
    liftIO $ putStrLn "comparing before to after1"
    compareMerkleTrees globalStateStore before after1 >>= liftIO . print

    liftIO $ putStrLn "comparing before to after2"
    compareMerkleTrees globalStateStore before after2 >>= liftIO . print

    liftIO $ putStrLn "comparing before to after3"
    compareMerkleTrees globalStateStore before after3 >>= liftIO . print

  print res
