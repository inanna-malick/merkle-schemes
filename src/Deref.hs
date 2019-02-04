{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}


module Deref where

--------------------------------------------
import           Util.RecursionSchemes (Term(..))
import           Merkle.Tree.Types
import           Merkle.Types
import           Store
--------------------------------------------

derefOneLayer
  :: Monad m
  => Store m
  -> MerkleTree
  -> m ConcreteMerkleTreeLayer
derefOneLayer store ht = case out ht of
  Direct _ t -> pure t
  Indirect p -> do
    t <- deref store p
    pure t
