{-# LANGUAGE TypeFamilies #-}

module Merkle.Tree.Types where

--------------------------------------------
import           Data.Aeson
import qualified Data.Hashable as Hash
import           Data.Text
--------------------------------------------
import           Merkle.Types
import           Util.MyCompose
import           Util.RecursionSchemes (Fix(..))
--------------------------------------------
import           Data.Functor.Foldable (Base, Recursive(..))
import qualified Data.Functor.Foldable as F

type Name = String

-- | Tree in which leaf nodes are specialized to String
data Tree a = Node [a] | Leaf String deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Named entity
type Named = (,) Name

-- | TODO DELETE?
type NamedTreeLayer = Named :+ Tree

data NamedTree = NamedNode Name [NamedTree] | NamedLeaf Name String
type instance Base NamedTree = Named :+ Tree
instance Recursive NamedTree where
  project (NamedNode n ns) = C (n, Node ns)
  project (NamedLeaf n b)  = C (n, Leaf b)

instance F.Corecursive NamedTree where
  embed (C (n, Node ns)) = (NamedNode n ns)
  embed (C (n, Leaf b)) = (NamedLeaf n b)

-- ok, so I can write the same functions over NamedTree as I write over NamedTreeLayer
testCata :: NamedTree -> String
testCata = F.cata (alg)
  where
    alg (C (n, Leaf b)) = n ++ b
    alg (C (n, Node ns)) = n ++ show ns


testAna :: String -> NamedTree
testAna = F.ana alg
  where
    alg "" = (C ("foo", Leaf "bar"))
    alg _  = (C ("bar", Node ["", ""]))

-- data HashedNamedTree = HashedNamedNode Pointer Name [HashedNamedTree] | HashedNamedLeaf Pointer Name String
-- type instance Base HashedNamedTree = WithHash :+ Named :+ Tree
-- instance Recursive HashedNamedTree where
--   project (HashedNamedNode p n ns) = C (p, C (n, Node ns))
--   project (HashedNamedLeaf p n b)  = C (p, C (n, Leaf b))

-- data LazyHashedNamedTree
--   = Indirection Pointer
--   | DirectHashedNamedNode Pointer Name [LazyHashedNamedTree]
--   | DirectHashedNamedLeaf Pointer Name String
-- type instance Base LazyHashedNamedTree = WithHash :+ Maybe :+ Named :+ Tree
-- instance Recursive LazyHashedNamedTree where
--   project (DirectHashedNamedNode p n ns) = C (p, C . Just $ C (n, Node ns))
--   project (DirectHashedNamedLeaf p n b)  = C (p, C . Just $ C (n, Leaf b))
--   project (Indirection p)                = C (p, C Nothing)


-- | merkle tree that at any level (including the top) can either consist of
--   hash-addressed pointers to nodes or substantiated named tree nodes paired with their pointer
type LazyMerkleTree = Fix (WithHash :+ Maybe :+ NamedTreeLayer)

-- | merkle tree that has all nodes fully substantiated
type StrictMerkleTree = Fix (WithHash :+ NamedTreeLayer)

-- | merkle tree in which the top layer is known to be substantiated and
--   all sub-nodes are represented using hash addressed pointers
--   newtype and not type alias so we can have clean typeclass instances
newtype ShallowMerkleTreeLayer = SMTL { unSMTL :: (Named :+ Tree) Pointer}

instance Hash.Hashable ShallowMerkleTreeLayer where
  hashWithSalt s (SMTL (C (n, (Leaf contents))))
    = s `Hash.hashWithSalt` Hash.hash n `Hash.hashWithSalt` Hash.hash contents
  hashWithSalt s (SMTL (C (n, (Node contents))))
    = s `Hash.hashWithSalt` Hash.hash n `Hash.hashWithSalt` Hash.hash contents

instance ToJSON ShallowMerkleTreeLayer where
    -- this generates a Value
    toJSON (SMTL (C (name, (Leaf body)))) =
        object ["type" .= ("leaf" :: Text), "name" .= pack name, "body" .= pack body]
    toJSON (SMTL (C (name, (Node pointers)))) =
        object ["type" .= ("node" :: Text), "name" .= pack name, "children" .= toJSON pointers]

instance FromJSON ShallowMerkleTreeLayer where
    parseJSON = withObject "ShallowMerkleTreeLayer" $ \v -> do
        name <- v .: "name"
        typ  <- v .: "type"
        case typ of
          "node" -> do
              children <- v .: "children"
              pure . SMTL $ C (name, Node children)
          "leaf" -> do
              body <- v .: "body"
              pure . SMTL $ C (name, Leaf body)
          x -> fail $ "unsupported node type " ++ x



-- | Forget information at the term level - drop any direct references
makeShallow :: Named :+ Tree $ Fix (WithHash :+ x) -> Named :+ Tree $ Pointer
makeShallow = fmap pointer

-- | Forget information at the type level
--   turn a value known to be shallow to a potentially-deep one
makeConcrete :: Named :+ Tree $ Pointer -> Named :+ Tree $ LazyMerkleTree
makeConcrete = fmap (Fix . C . (,C Nothing))
