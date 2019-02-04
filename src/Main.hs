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
import           Errors
import           Ingress (buildDirTree, outputDirTree)
import           Merkle.Types (mtPointer, HashIdentifiedEntity(Indirect))
import           Util.RecursionSchemes (Term(In))
import           Util.Util (mapErrUtil)
import           Store
--------------------------------------------

main :: IO ()
main = do
  res' <- runExceptT $ do
    store <- liftIO $ iorefStore <$> newIORef Map.empty
    -- forget structure of merkle trees and retain only a pointer to the top level
    let forgetStructure = In . Indirect . mtPointer

    before <- mapErrUtil InputError $ forgetStructure <$> buildDirTree store "examples/before" "node1"
    after1 <- mapErrUtil InputError $ forgetStructure <$> buildDirTree store "examples/after1" "node1"
    after2 <- mapErrUtil InputError $ forgetStructure <$> buildDirTree store "examples/after2" "node1"
    after3 <- mapErrUtil InputError $ forgetStructure <$> buildDirTree store "examples/after3" "node2"

    _ <- mapErrUtil CompareError $ do
      liftIO $ putStrLn "comparing before to after1"
      compareMerkleTrees store before after1 >>= liftIO . print

      liftIO $ putStrLn "comparing before to after2"
      compareMerkleTrees store before after2 >>= liftIO . print

      liftIO $ putStrLn "comparing before to after3"
      compareMerkleTrees store before after3 >>= liftIO . print

    mapErrUtil FileWriteError $ outputDirTree store "tmp" after3

  print res'
