module Merkle.Tree.Types where

--------------------------------------------
import           Data.Functor.Foldable
--------------------------------------------
import           Util.MyCompose
--------------------------------------------

-- | Tree in which leaf nodes are specialized to String
data Tree a = Node [a] | Leaf String deriving (Eq, Show, Functor, Foldable, Traversable)


type Name = String

-- | Entity tagged with a name
type Named = (,) Name

newtype Pointer = Pointer { unPointer :: Int } deriving (Eq, Ord, Show)

-- | Entity tagged with a hash that uniquely identifies it
type WithHash = (,) Pointer

-- | merkle tree that at any level (including the top) can either consist of
--   hash-addressed pointers to nodes or substantiated named tree nodes paired with their pointer
type LazyMerkleTree = Fix (WithHash :+ Maybe :+ Named :+ Tree)

-- | merkle tree that has all nodes fully substantiated
type StrictMerkleTree = Fix (WithHash :+ Named :+ Tree)


pointer :: forall f . Fix $ WithHash :+ f -> Pointer
pointer = fst . getCompose . unfix

-- | Forget information at the term level - drop any direct references
makeShallow :: Named :+ Tree $ Fix (WithHash :+ x) -> Named :+ Tree $ Pointer
makeShallow = fmap pointer

-- | Forget information at the type level
--   turn a value known to be shallow to a potentially-deep one
makeConcrete :: Named :+ Tree $ Pointer -> Named :+ Tree $ LazyMerkleTree
makeConcrete = fmap (Fix . C . (,C Nothing))
