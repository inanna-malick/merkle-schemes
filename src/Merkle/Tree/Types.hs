{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

module Merkle.Tree.Types where

--------------------------------------------
import qualified Data.Hashable as Hash
import           Data.HashMap.Strict (HashMap)
import           Data.IORef
--------------------------------------------
import           Merkle.Types (Pointer(..), HashIdentifiedEntity(..), mtPointer)
import           Util.RecursionSchemes (Term(..))
--------------------------------------------

type Name = String

-- | Tree in which leaf nodes are specialized to String
data Tree (a :: *) = Node [a] | Leaf String deriving (Eq, Show, Functor)

-- | Named entity
data NamedEntity (f :: * -> *) a
  = NamedEntity
  { neName   :: Name
  , neEntity :: f a
  } deriving (Eq, Show, Functor)

-- | named tree node or leaf, parameterized over the type of any sub-nodes
type MerkleTreeLayer = NamedEntity Tree
-- | merkle tree that at any level (including the top) can either consist of
--   hash-addressed pointers to nodes or substantiated nodes
type MerkleTree = Term (HashIdentifiedEntity MerkleTreeLayer)
-- | merkle tree in which the top layer is known to be substantiated and
--   sub-nodes can be either hash addressed pointers or direct references
type ConcreteMerkleTreeLayer = MerkleTreeLayer MerkleTree

-- | merkle tree in which the top layer is known to be substantiated and
--   all sub-nodes are represented using hash addressed pointers
type ShallowMerkleTreeLayer = MerkleTreeLayer Pointer

instance Hash.Hashable ShallowMerkleTreeLayer where
  hashWithSalt s (NamedEntity n (Leaf contents))
    = s `Hash.hashWithSalt` Hash.hash n `Hash.hashWithSalt` Hash.hash contents
  hashWithSalt s (NamedEntity n (Node contents))
    = s `Hash.hashWithSalt` Hash.hash n `Hash.hashWithSalt` Hash.hash contents

makeShallow :: ConcreteMerkleTreeLayer -> ShallowMerkleTreeLayer
makeShallow = fmap mtPointer

makeConcrete :: ShallowMerkleTreeLayer -> ConcreteMerkleTreeLayer
makeConcrete = fmap (In . Indirect)

type GlobalStore = IORef (HashMap Pointer ConcreteMerkleTreeLayer)
