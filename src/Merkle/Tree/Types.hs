module Merkle.Tree.Types where

--------------------------------------------
import           Data.Aeson
import qualified Data.Hashable as Hash
import           Data.HashMap.Strict (HashMap)
import           Data.IORef
import           Data.Text
--------------------------------------------
import           Merkle.Types (Pointer(..), HashIdentifiedEntity(..), mtPointer)
import           Util.MyCompose
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


-- | TODO DOX
type NamedTreeLayer = NamedEntity :+ Tree

-- | merkle tree that at any level (including the top) can either consist of
--   hash-addressed pointers to nodes or substantiated named tree nodes
type MerkleTree = Term (HashIdentifiedEntity :+ NamedEntity :+ Tree)

-- | merkle tree in which the top layer is known to be substantiated and
--   all sub-nodes are represented using hash addressed pointers
--   newtype and not type alias so we can have clean typeclass instances
newtype ShallowMerkleTreeLayer = SMTL { unSMTL :: (NamedEntity :+ Tree) Pointer}

instance Hash.Hashable ShallowMerkleTreeLayer where
  hashWithSalt s (SMTL (C (NamedEntity n (Leaf contents))))
    = s `Hash.hashWithSalt` Hash.hash n `Hash.hashWithSalt` Hash.hash contents
  hashWithSalt s (SMTL (C (NamedEntity n (Node contents))))
    = s `Hash.hashWithSalt` Hash.hash n `Hash.hashWithSalt` Hash.hash contents

instance ToJSON ShallowMerkleTreeLayer where
    -- this generates a Value
    toJSON (SMTL (C (NamedEntity name (Leaf body)))) =
        object ["type" .= ("leaf" :: Text), "name" .= pack name, "body" .= pack body]
    toJSON (SMTL (C (NamedEntity name (Node pointers)))) =
        object ["type" .= ("node" :: Text), "name" .= pack name, "children" .= toJSON pointers]

instance FromJSON ShallowMerkleTreeLayer where
    parseJSON = withObject "ShallowMerkleTreeLayer" $ \v -> do
        name <- v .: "name"
        typ  <- v .: "type"
        case typ of
          "node" -> do
              children <- v .: "children"
              pure . SMTL . C $ NamedEntity name $ Node children
          "leaf" -> do
              body <- v .: "body"
              pure . SMTL . C $ NamedEntity name $ Leaf body
          x -> fail $ "unsupported node type " ++ x



-- | Forget information at the term level - drop any direct references
makeShallow :: (NamedEntity :+ Tree) MerkleTree -> (NamedEntity :+ Tree) Pointer
makeShallow = fmap mtPointer

-- | Forget information at the type level
--   turn a value known to be shallow to a potentially-deep one
makeConcrete :: (NamedEntity :+ Tree) Pointer -> (NamedEntity :+ Tree) MerkleTree
makeConcrete = fmap (In . C . Indirect)


type GlobalStore = IORef (HashMap Pointer  ((NamedEntity :+ Tree) MerkleTree))
