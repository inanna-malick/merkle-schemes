{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics (Generic)

import qualified Data.Hashable as Hash

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)

type Name = String


-- pointer to entity via said entity's hash
data Pointer = Pointer Int deriving (Eq, Show, Generic)
instance Hash.Hashable Pointer

-- ugh, adding names really complicates things and I don't really like it but it seems req'd for diffs
data NamedEntity (f :: * -> *) a = NamedEntity Name (f a) deriving (Eq, Show, Functor)

data Tree a = Node (HashMap Name a) | Leaf Int  deriving (Eq, Show, Functor)

data Term (f :: * -> *) = In { out :: f (HashTerm f) }

data HashTerm (f :: * -> *)
  = Direct   Pointer (Term f) -- node id is included in node metadata of direct ref (pointer)
  | Indirect Pointer          -- indirect ref is just a pointer

htPointer :: HashTerm f -> Pointer
htPointer (Direct p _) = p
htPointer (Indirect p) = p

-- | hash to get pointer, basically - builder fn
-- NOTE: should really put canonical hash fns somewhere more central/clearer
lift :: NamedEntity Tree Pointer -> HashTerm (NamedEntity Tree)
lift e = Direct hash (In $ fmap Indirect e)
  where
    hash = Pointer $ case e of
      NamedEntity n (Leaf contents) -> (Hash.hash n) `Hash.hashWithSalt` (Hash.hash contents)
      NamedEntity n (Node contents) -> (Hash.hash n) `Hash.hashWithSalt` (Hash.hash contents)

-- NOTE: could just remove name from map? handling collissions would be harder (I think due to case where there are duplicates of name in dir) but I can just fucking nuke those cases via error.. and also it's not like I can't drop them back in a map for comparison's sake

leaf1 :: HashTerm (NamedEntity Tree)
leaf1 = lift (NamedEntity "leaf1" $ Leaf 1)

leaf2 :: HashTerm (NamedEntity Tree)
leaf2 = lift (NamedEntity "leaf2" $ Leaf 2)

node1 :: HashTerm (NamedEntity Tree) -- demonstrates problem, can have name error in structure
node1 = lift (NamedEntity "node1" $ Node $ Map.fromList ["leaf2", htPointer leaf1)])


-- type-level guarantee that it pops a layer off the hash stack, nice
deref :: Pointer -> IO (Term f)
deref = undefined -- network call goes here (todo println to debug along w/ w/e lookup)

type FileBody = Int

 -- todo idk record syntax or w/e for branches, will quickly become unreadable..
data Diff = NoDiff
          | LeafModified  (Name, FileBody, FileBody)
          | FileReplacedWithDir -- (String, Pointer, Pointer)
          | DirReplacedWithFile -- (String, Pointer, Pointer)
          | EntityAddedToDir     -- (String, Pointer, Pointer)
          | EntityRemovedFromDir -- (String, Pointer, Pointer)
          | EntityRenamed (Name, Name)
          | EntityDeleted Name -- dir or file
          | EntityCreated Name -- dir or file

-- | yeet yueet
-- todo bake in failure modes (either return type to represent deref fail, can 'error' for now)
-- ok so it compiles? time to run some test cases...
merklecata :: HashTerm (NamedEntity Tree) -> HashTerm (NamedEntity Tree) -> IO [Diff]
merklecata ht1 ht2 = case (htid ht1 == htid ht2) of
  True  -> pure [NoDiff] -- no need to explore further here
  False -> do -- hash mismatch NOTE: need to deref to look at node names now, which tbh makes sense
    (NamedEntity name1 entity1) <- out <$> derefOneLayer ht1
    (NamedEntity name2 entity2) <- out <$> derefOneLayer ht2

    case name1 == name2 of
      False ->  -- do the lazy thing
        -- usually an error case, but can happen if given (eg) top level complete disjoint trees
        -- with different id, different name
        pure [EntityDeleted name1, EntityCreated name2]
      True -> -- no name mismatch, but known hash mismatch - must explore further
        case (entity1, entity2) of
          (Leaf fc1, Leaf fc2)
            | fc1 /= fc2   -> pure [LeafModified (name1, fc1, fc2)]
            | otherwise    -> error "file contents don't differ? but hash ids do... (error case)"
          (Node _, Leaf _) -> pure [DirReplacedWithFile]
          (Leaf _, Node _) -> pure [FileReplacedWithDir]
          (Node ns1, Node ns2) -> do
            let cmpRes = mapCompare ns1 ns2

            traverse resolveMapDiff cmpRes

  where
    -- todo NAME
    resolveMapDiff  (This (name,_)) = pure $ EntityDeleted name
    resolveMapDiff  (That (name,_)) = pure $ EntityCreated name
    resolveMappDiff (These a b)     = merklecata a b

    -- grab hash id of a node
    htid :: HashTerm f -> Pointer
    htid (Direct p _) = p
    htid (Indirect p) = p

    derefOneLayer :: HashTerm f -> IO (Term f)
    derefOneLayer ht = case ht of
      Direct _ t -> pure t
      Indirect p -> do
        t <- deref p
        pure t



mapCompare :: Eq k => Hash.Hashable k => HashMap k v -> HashMap k v -> [These (k,v) (k,v)]
mapCompare h1 h2 = h1Only ++ h2Only ++ both
  where h1Only = fmap This . Map.toList $ Map.difference h1 h2
        h2Only = fmap That . Map.toList $ Map.difference h1 h2
        both   = Map.elems $ Map.intersectionWithKey (\k v1 v2 -> These (k,v1) (k,v2)) h1 h2


data These a b = This a | These a b | That b


main = do
  putStrLn "Hello"
  putStrLn "World"

