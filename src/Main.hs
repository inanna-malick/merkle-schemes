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
import           Ingress (buildDirTree, outputDirTree)
import           Merkle.Types (mtPointer, HashIdentifiedEntity(Indirect))
import           Util.RecursionSchemes (Term(In))
--------------------------------------------

main :: IO ()
main = do
  globalStateStore <- newIORef Map.empty
  res' <- runExceptT $ do
    -- forget structure of merkle trees and retain only a pointer to the top level
    let forgetStructure = In . Indirect . mtPointer

    before <- forgetStructure <$> buildDirTree globalStateStore "examples/before" "node1"
    after1 <- forgetStructure <$> buildDirTree globalStateStore "examples/after1" "node1"
    after2 <- forgetStructure <$> buildDirTree globalStateStore "examples/after2" "node1"
    after3 <- forgetStructure <$> buildDirTree globalStateStore "examples/after3" "node2"

    res <- liftIO . runExceptT $ do
      liftIO $ putStrLn "comparing before to after1"
      compareMerkleTrees globalStateStore before after1 >>= liftIO . print

      liftIO $ putStrLn "comparing before to after2"
      compareMerkleTrees globalStateStore before after2 >>= liftIO . print

      liftIO $ putStrLn "comparing before to after3"
      compareMerkleTrees globalStateStore before after3 >>= liftIO . print

      outputDirTree globalStateStore "tmp" after3

    liftIO $ print res
  print res'
