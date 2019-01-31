{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

--------------------------------------------
import qualified Data.Hashable as Hash
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import           Data.IORef
import           GHC.Generics (Generic)
--------------------------------------------

type Hash = Int
type Name = String
type FileBody = String
type GlobalStore = IORef (HashMap Pointer (Term (NamedEntity Tree)))

newtype Pointer = Pointer { unPointer :: Hash }
  deriving (Eq, Ord, Show, Generic)
instance Hash.Hashable Pointer

data NamedEntity (f :: * -> *) a = NamedEntity Name (f a) deriving (Eq, Show, Functor)

-- note: Node spiritually has a set of children, not a list, but I want a Functor instance
data Tree (a :: *) = Node [a] | Leaf String deriving (Eq, Show, Functor)

data Term (f :: * -> *) = In { out :: f (HashTerm f) }

data HashTerm (f :: * -> *)
  = Direct   Pointer (Term f) -- node id is included in node metadata of direct ref (pointer)
  | Indirect Pointer          -- indirect ref is just a pointer in some hash-addressed store

htPointer :: HashTerm f -> Pointer
htPointer (Direct p _) = p
htPointer (Indirect p) = p

showHT :: HashTerm (NamedEntity Tree) -> String
showHT (Direct p t) = "Direct{ pointer: " ++ show p ++ ", term: " ++ showT t ++ "}"
showHT (Indirect p) = "Indirect{ pointer: " ++ show p ++ "}"

showT :: Term (NamedEntity Tree) -> String
showT (In t) = "Term{" ++ showTree t ++ "}"

showTree :: NamedEntity Tree (HashTerm (NamedEntity Tree)) -> String
showTree (NamedEntity n (Node ns)) = "Node(" ++ n ++ ")(" ++ show (fmap showHT ns) ++ ")"
showTree (NamedEntity n (Leaf bs)) = "Leaf(" ++ n ++ ")(" ++ show bs ++ ")"


-- | hash to get pointer, basically - builder fn that takes a flat single layer of a tree
--   and lifts it to produce a direct reference to a pointer-identified tree layer
lift :: NamedEntity Tree Pointer -> (Pointer, Term (NamedEntity Tree))
lift e = (Pointer $ hashTree e, (In $ fmap Indirect e))

-- hash a single merkle tree entry with all subentities represented as hash pointers
hashTree :: NamedEntity Tree Pointer -> Hash
hashTree (NamedEntity n (Leaf contents))
  = Hash.hash n `Hash.hashWithSalt` Hash.hash contents
hashTree (NamedEntity n (Node contents))
  = Hash.hash n `Hash.hashWithSalt` Hash.hash contents


data Diff = LeafModified  (Name, FileBody, FileBody)
          | FileReplacedWithDir Name
          | DirReplacedWithFile Name
          | EntityAddedToDir
          | EntityRemovedFromDir
          | EntityRenamed Name Name
          | EntityDeleted Name
          | EntityCreated Name
  deriving (Show)

mapCompare :: Eq k => Hash.Hashable k => HashMap k v -> HashMap k v -> [These (k,v)]
mapCompare h1 h2 = h1Only ++ h2Only ++ both
  where h1Only = fmap This . Map.toList $ Map.difference h1 h2
        h2Only = fmap That . Map.toList $ Map.difference h2 h1
        both   = Map.elems $ Map.intersectionWithKey (\k v1 v2 -> These (k,v1) (k,v2)) h1 h2

-- | specialized to 'a a' to make functor derive easy... could do bifunctor?
data These a = This a | These a a | That a deriving (Eq, Ord, Show, Functor)
