{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Merkle.Tree.Types where

--------------------------------------------
import           Data.Aeson
import           Data.Functor.Compose
import qualified Data.Hashable as Hash
import           Data.HashMap.Strict (HashMap)
import           Data.IORef
import           Data.Text
--------------------------------------------
import           Merkle.Types (Pointer(..), HashIdentifiedEntity(..), mtPointer)
import           Util.RecursionSchemes (Term(..))
--------------------------------------------

type Name = String

-- | Tree in which leaf nodes are specialized to String
data Tree a = Node [a] | Leaf String deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Named entity
data NamedEntity a
  = NamedEntity
  { neName   :: Name
  , neEntity :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

-- | named tree node or leaf, parameterized over the type of any sub-nodes
type MerkleTreeLayer = Compose NamedEntity Tree
-- | merkle tree that at any level (including the top) can either consist of
--   hash-addressed pointers to nodes or substantiated nodes
type MerkleTree = Term (Compose HashIdentifiedEntity MerkleTreeLayer)
-- | merkle tree in which the top layer is known to be substantiated and
--   sub-nodes can be either hash addressed pointers or direct references
type ConcreteMerkleTreeLayer = MerkleTreeLayer MerkleTree

-- | merkle tree in which the top layer is known to be substantiated and
--   all sub-nodes are represented using hash addressed pointers
-- newtype and not type alias so we can have typeclass instances
newtype ShallowMerkleTreeLayer = SMTL { unSMTL :: MerkleTreeLayer Pointer}

instance Hash.Hashable ShallowMerkleTreeLayer where
  hashWithSalt s (SMTL (Compose (NamedEntity n (Leaf contents))))
    = s `Hash.hashWithSalt` Hash.hash n `Hash.hashWithSalt` Hash.hash contents
  hashWithSalt s (SMTL (Compose (NamedEntity n (Node contents))))
    = s `Hash.hashWithSalt` Hash.hash n `Hash.hashWithSalt` Hash.hash contents

instance ToJSON ShallowMerkleTreeLayer where
    -- this generates a Value
    toJSON (SMTL (Compose (NamedEntity name (Leaf body)))) =
        object ["type" .= ("leaf" :: Text), "name" .= pack name, "body" .= pack body]
    toJSON (SMTL (Compose (NamedEntity name (Node pointers)))) =
        object ["type" .= ("node" :: Text), "name" .= pack name, "children" .= toJSON pointers]

instance FromJSON ShallowMerkleTreeLayer where
    parseJSON = withObject "ShallowMerkleTreeLayer" $ \v -> do
        name <- v .: "name"
        typ  <- v .: "type"
        case typ of
          "node" -> do
              children <- v .: "children"
              pure . SMTL . Compose $ NamedEntity name $ Node children
          "leaf" -> do
              body <- v .: "body"
              pure . SMTL . Compose $ NamedEntity name $ Leaf body
          x -> fail $ "unsupported node type " ++ x



-- | Forget information at the term level - drop any direct references
makeShallow :: ConcreteMerkleTreeLayer -> MerkleTreeLayer Pointer
makeShallow = fmap mtPointer

-- | Forget information at the type level
--   turn a value known to be shallow to a potentially-deep one
makeConcrete :: MerkleTreeLayer Pointer -> ConcreteMerkleTreeLayer
makeConcrete = fmap (In . Compose . Indirect)


type GlobalStore = IORef (HashMap Pointer ConcreteMerkleTreeLayer)
