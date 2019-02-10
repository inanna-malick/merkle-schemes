module Merkle.Tree.Types where

--------------------------------------------
import           Data.Functor.Foldable
--------------------------------------------
import           Merkle.Types (WithHash)
import           Util.MyCompose
--------------------------------------------

-- | Tree in which leaf nodes are specialized to String
data Tree a = Node [a] | Leaf String deriving (Eq, Show, Functor, Foldable, Traversable)


type Name = String

-- | Entity tagged with a name
type Named = (,) Name

-- | merkle tree that at any level (including the top) can either consist of
--   hash-addressed pointers to nodes or substantiated named tree nodes paired with their pointer
type LazyMerkleTree = Fix (WithHash :+ Maybe :+ Named :+ Tree)

-- | merkle tree that has all nodes fully substantiated
type StrictMerkleTree = Fix (WithHash :+ Named :+ Tree)
