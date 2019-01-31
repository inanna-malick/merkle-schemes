{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import GHC.Generics (Generic)

import Control.Monad (join)

import qualified Data.Hashable as Hash

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)

import qualified Data.Set as Set
import Data.Set (Set)

type Name = String

-- pointer to entity via said entity's hash
data Pointer = Pointer Int deriving (Eq, Show, Generic)
instance Hash.Hashable Pointer

data NamedEntity (f :: * -> *) a = NamedEntity Name (f a) deriving (Eq, Show, Functor)

data Tree (a :: *) = Node (Set a) | Leaf Int  deriving (Eq, Show)

data Term (f :: * -> *) = In { out :: f (HashTerm f) }

data HashTerm (f :: * -> *)
  = Direct   Pointer (Term f) -- node id is included in node metadata of direct ref (pointer)
  | Indirect Pointer          -- indirect ref is just a pointer


showHT :: HashTerm (NamedEntity Tree) -> String
showHT (Direct p t) = "Direct{ pointer: " ++ show p ++ ", term: " ++ showT t ++ "}"
showHT (Indirect p) = "Indirect{ pointer: " ++ show p ++ "}"

showT :: Term (NamedEntity Tree) -> String
showT (In t) = "Term{" ++ showTree t ++ "}"

showTree :: NamedEntity Tree (HashTerm (NamedEntity Tree)) -> String
showTree (NamedEntity n (Node ns)) = "Node(" ++ n ++ ")(" ++ show (fmap showHT ns) ++ ")" -- todo make better
showTree (NamedEntity n (Leaf bs)) = "Leaf(" ++ n ++ ")(" ++ show bs ++ ")"

htPointer :: HashTerm f -> Pointer
htPointer (Direct p _) = p
htPointer (Indirect p) = p


-- lawless instance YOLO (duplicates resulting from 'b' will be removed)
fmapSet :: (a -> b) -> Set a -> Set b
fmapSet f = Set.toList fmap f . Set.toList

-- | hash to get pointer, basically - builder fn
-- NOTE: should really put canonical hash fns somewhere more central/clearer
lift :: NamedEntity Tree Pointer -> (Pointer, Term (NamedEntity Tree))
lift e = (hash, (In $ fmap Indirect e))
  where
    hash = Pointer $ case e of
      NamedEntity n (Leaf contents) -> (Hash.hash n) `Hash.hashWithSalt` (Hash.hash contents)
      NamedEntity n (Node contents) -> (Hash.hash n) `Hash.hashWithSalt` (Hash.hash contents)

leaf1 :: (Pointer, Term (NamedEntity Tree))
leaf1 = lift . NamedEntity "leaf1" $ Leaf 1

leaf2 :: (Pointer, Term (NamedEntity Tree))
leaf2 = lift . NamedEntity "leaf2" $ Leaf 2

node1 :: (Pointer, Term (NamedEntity Tree))
node1 = lift . NamedEntity "node1" $ Node [fst leaf1, fst leaf2]

node1' :: HashTerm (NamedEntity Tree)
node1' = uncurry Direct node1

-- | node 1 with diff: leaf2 removed
node2 :: (Pointer, Term (NamedEntity Tree))
node2 = lift . NamedEntity "node1" $ Node [fst leaf1]

-- | immutable global state store (shh, pretend it's on the other end of a network call)
globalStateStore :: HashMap Pointer (Term (NamedEntity Tree))
globalStateStore = Map.fromList [leaf1, leaf2, node1, node2]

-- | type-level guarantee that it pops a layer off the hash stack, nice
deref :: Pointer -> IO (Term (NamedEntity Tree))
deref p = do
  putStrLn $ "attempt to deref " ++ show p ++ " via global state store"
  case Map.lookup p globalStateStore of
    Nothing -> error "YOLO420"
    Just x  -> do
      putStrLn $ "returning deref res: " ++ showT x
      pure x

type FileBody = Int

data Diff = LeafModified  (Name, FileBody, FileBody)
          | FileReplacedWithDir -- (String, Pointer, Pointer)
          | DirReplacedWithFile -- (String, Pointer, Pointer)
          | EntityAddedToDir     -- (String, Pointer, Pointer)
          | EntityRemovedFromDir -- (String, Pointer, Pointer)
          | EntityRenamed (Name, Name)
          | EntityDeleted Name -- dir or file
          | EntityCreated Name -- dir or file
  deriving (Show)

-- | yeet yueet
-- todo bake in failure modes (either return type to represent deref fail, can 'error' for now)
merklecata :: HashTerm (NamedEntity Tree) -> HashTerm (NamedEntity Tree) -> IO [Diff]
merklecata ht1 ht2 = do
  putStrLn $ "merklecata: " ++ showHT ht1 ++ ", " ++ showHT ht2
  case (htid ht1 == htid ht2) of
    True  -> pure [] -- no need to explore further here
    False -> do -- hash mismatch NOTE: need to deref to look at node names now, which tbh makes sense
      ne1 <- out <$> derefOneLayer ht1
      ne2 <- out <$> derefOneLayer ht2
      compareDerefed ne1 ne2

  where


    compareDerefed ne1 ne2 = do
      putStrLn $ "compareDerefed: " ++ showTree ne1 ++ ", " ++ showTree ne2
      compareDerefed' ne1 ne2

    compareDerefed' (NamedEntity name1 entity1) (NamedEntity name2 entity2)
      | name1 /= name2 = -- this case is weird and problematic?
          -- usually an error case, but can happen if given (eg) top level complete disjoint trees
          -- with different id, different name (could have diff cases for node -> leaf, leaf -> node)
          -- NOTE: hitting this case TOO EARLY, need to just continue traversal here..
          pure [EntityDeleted name1, EntityCreated name2]
      | otherwise = -- no name mismatch, but known hash mismatch - must explore further
          case (entity1, entity2) of
            (Leaf fc1, Leaf fc2)
              | fc1 /= fc2   -> pure [LeafModified (name1, fc1, fc2)]
              -- this case is weird because recursing directly via compareDerefed means we skip the
              -- top level hash comparison.. this should instead be nodiff.
              | otherwise    -> pure []
            (Node _, Leaf _) -> pure [DirReplacedWithFile]
            (Leaf _, Node _) -> pure [FileReplacedWithDir]
            (Node ns1, Node ns2) -> do
               -- need to deref to get names to build diffs -- potential optimization here..
              -- NOTE: need to ONLY run this on those sub-trees with non-matching hashes or else ERROR
              -- note: could probably optimize this bit
              let filteredNs1 = Set.filter (not . flip Set.member ns2) ns1
                  filteredNs2 = Set.filter (not . flip Set.member ns1) ns2

              derefedNs1 <- traverse derefOneLayer $ Set.toList filteredNs1
              derefedNs2 <- traverse derefOneLayer $ Set.toList filteredNs2

              -- NOTE: do this BEFORE DEREF
              -- filter out all elems w/ i
              -- let derefedNs1 = filter (not . elem derefedNs2') derefedNs1'
              --     derefedNs2 = filter (not . elem derefedNs2') derefedNs1'

              let mkByNameMap :: [Term (NamedEntity Tree)] -> HashMap Name (Term (NamedEntity Tree))
                  mkByNameMap ns = Map.fromList . fmap mkByNameMapEntry ns
                  mkByNameMapEntry t@(In (NamedEntity n _)) = (n, t)

              let cmpRes :: [These (Name, Term (NamedEntity Tree)) (Name, Term (NamedEntity Tree))]
                  cmpRes = mapCompare (mkByNameMap derefedNs1) (mkByNameMap derefedNs2)

              join <$> traverse resolveMapDiff cmpRes

    -- todo NAME
    resolveMapDiff :: These (Name, Term (NamedEntity Tree)) (Name, Term (NamedEntity Tree))
                   -> IO [Diff]
    resolveMapDiff (This (name,_))     = pure [EntityDeleted name]
    resolveMapDiff (That (name,_))     = pure [EntityCreated name]
    resolveMapDiff (These (_,a) (_,b)) = compareDerefed (out a) (out b)

    -- grab hash id of a node
    htid :: HashTerm f -> Pointer
    htid (Direct p _) = p
    htid (Indirect p) = p

    derefOneLayer :: HashTerm (NamedEntity Tree) -> IO (Term (NamedEntity Tree))
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
  res <- merklecata (Indirect $ fst node1) (Indirect $ fst node2) 
  print $ res

