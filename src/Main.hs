{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

--------------------------------------------
import           Control.Monad.Except (runExceptT, liftIO)
--------------------------------------------
import           Commands
import           Compare (compareMerkleTrees)
import           Errors
import           Ingress -- (buildDirTree, outputDirTree)
import           Merkle.Types (mtPointer)
import           Util.Util (mapErrUtil)
import           Store
--------------------------------------------

main :: IO ()
main = run =<< parse

run :: MerkleDiffOpts -> IO ()
run (MerkleDiffOpts storeDir (Diff before after)) = do
  let store = fsStore storeDir
  res <- runExceptT $ compareMerkleTrees store before after
  print res
run (MerkleDiffOpts storeDir (Get p mfp)) = do
  let store = fsStore storeDir
  fp <- maybe (createTmpDir "merkle_get") pure mfp
  _res <- runExceptT $ outputDirTree store fp p
  putStrLn "done getting!"
  --print res
run (MerkleDiffOpts storeDir (Put fp)) = do
  let store = fsStore storeDir
  _res <- runExceptT $ buildDirTree store fp
  putStrLn "done putting!"
  --print res
run (MerkleDiffOpts storeDir Demo) = do -- run the old main method used for testing
  res' <- runExceptT $ do
    let store = fsStore storeDir

    -- forget structure of merkle trees and retain only a pointer to the top level
    let forgetStructure = mtPointer

    -- read some merkle trees into memory (and into the store) and then forget all but the top pointer
    before <- mapErrUtil InputError $ forgetStructure <$> buildDirTree store "examples/before/node1"
    after1 <- mapErrUtil InputError $ forgetStructure <$> buildDirTree store "examples/after1/node1"
    after2 <- mapErrUtil InputError $ forgetStructure <$> buildDirTree store "examples/after2/node1"
    after3 <- mapErrUtil InputError $ forgetStructure <$> buildDirTree store "examples/after3/node2"

    mapErrUtil InputError $ do
      liftIO $ putStrLn "comparing before to after1"
      compareMerkleTrees store before after1 >>= liftIO . print

      liftIO $ putStrLn "comparing before to after2"
      compareMerkleTrees store before after2 >>= liftIO . print

      liftIO $ putStrLn "comparing before to after3"
      compareMerkleTrees store before after3 >>= liftIO . print

    mapErrUtil FileWriteError $ liftIO (createTmpDir "output-example") >>= flip (outputDirTree store) after3

  print res'
