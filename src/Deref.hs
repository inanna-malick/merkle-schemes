{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}


module Deref where

--------------------------------------------
import           Control.Monad.Except
import qualified Data.HashMap.Strict as Map
import           Data.IORef
--------------------------------------------
import           Errors
import           Util.RecursionSchemes (Term(..))
import           Merkle.Tree.Types
import           Merkle.Types
--------------------------------------------

-- | fetch a value from the global store. Pretend this involves a network call.
-- NOTE: could also encode at the type level that this only returns one layer via return type..
deref
  :: GlobalStore
  -> Pointer
  -> ExceptT MerkleTreeCompareError IO ConcreteMerkleTreeLayer
deref store p = do
  globalStateStore <- liftIO $ readIORef store
  liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via global state store"
  case Map.lookup p globalStateStore of
    Nothing -> throwError LookupError
    Just x  -> do
      -- putStrLn $ "returning deref res: " ++ showT x
      pure x

derefOneLayer
  :: GlobalStore
  -> MerkleTree
  -> ExceptT MerkleTreeCompareError IO ConcreteMerkleTreeLayer
derefOneLayer store ht = case out ht of
  Direct _ t -> pure t
  Indirect p -> do
    t <- deref store p
    pure t
