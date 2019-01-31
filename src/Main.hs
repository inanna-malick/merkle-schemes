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

import qualified System.Directory as Dir

import Data.IORef

type Hash = Int
type Name = String

type GlobalStore = IORef (HashMap Pointer (Term (NamedEntity Tree)))

-- TODO LIST MAIN
-- have fn that derives tree from actual on-disk dir structure to cleanly allow diffs of actual structures
--   note: prohibit any symlinks or whatever

-- | actual dir recursive traversal
-- ignores permissions in diffs and coerces file contents into unicode #YOLO
buildDirTree :: GlobalStore -> FilePath -> FilePath -> IO (HashTerm (NamedEntity Tree))
buildDirTree store path fn = do
  let fullpath = path ++ "/" ++ fn
  putStrLn $ "run buildDirTree on: " ++ fullpath
  isFile <- Dir.doesFileExist fullpath
  if isFile
    then do
      fc <- readFile fullpath
      let x = lift $ NamedEntity fn $ Leaf fc
      modifyIORef' store (uncurry Map.insert x) -- todo dont uncurry, instead pattern match?
      pure $ uncurry Direct x
    else do
      isDir <- Dir.doesDirectoryExist fullpath
      if isDir
        then do
          contents <- filter (/= ".") . filter (/= "..") <$> Dir.getDirectoryContents fullpath
          putStrLn $ "recurse on contents: " ++ show contents
          -- actual tree structure thrown away, only pointers used.. need to store in global map
          -- for actual use later
          entities <- traverse (buildDirTree store fullpath) contents
          let x = lift $ NamedEntity fn $ Node $ fmap htPointer entities
          modifyIORef' store (uncurry Map.insert x) -- todo dont uncurry, instead pattern match?
          pure $ uncurry Direct x
      else error $ "invalid file path " ++ fullpath ++ "? wtf yo no symlinks or sockets allowed"


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


-- | hash to get pointer, basically - builder fn that takes a flat single layer of a tree
--   and lifts it to produce a direct reference to a pointer-identified tree layer
-- NOTE: should really put canonical hash fns somewhere more central/clearer
lift :: NamedEntity Tree Pointer -> (Pointer, Term (NamedEntity Tree))
lift e = (Pointer $ hashTree e, (In $ fmap Indirect e))

-- hash a single merkle tree entry with all subentities represented as hash pointers
hashTree :: NamedEntity Tree Pointer -> Hash
hashTree (NamedEntity n (Leaf contents))
  = Hash.hash n `Hash.hashWithSalt` Hash.hash contents
hashTree (NamedEntity n (Node contents))
  = Hash.hash n `Hash.hashWithSalt` Hash.hash contents

-- leaf1 :: (Pointer, Term (NamedEntity Tree))
-- leaf1 = lift . NamedEntity "leaf1" $ Leaf "one"

-- leaf2 :: (Pointer, Term (NamedEntity Tree))
-- leaf2 = lift . NamedEntity "leaf2" $ Leaf "two"

-- node1 :: (Pointer, Term (NamedEntity Tree))
-- node1 = lift . NamedEntity "node1" $ Node [fst leaf1, fst leaf2]

-- leaf2node :: (Pointer, Term (NamedEntity Tree))
-- leaf2node = lift . NamedEntity "leaf2" $ Node [fst leaf2]

-- -- | node 1 with diff: leaf2 replaced with node
-- node2 :: (Pointer, Term (NamedEntity Tree))
-- node2 = lift . NamedEntity "node1" $ Node [fst leaf1] -- , fst leaf2node]

-- | immutable global state store (shh, pretend it's on the other end of a network call)
-- globalStateStore :: HashMap Pointer (Term (NamedEntity Tree))
-- globalStateStore = Map.fromList [leaf1, leaf2, node1, node2, leaf2node]

-- | type-level guarantee that it pops a layer off the hash stack, nice
deref :: GlobalStore -> Pointer -> IO (Term (NamedEntity Tree))
deref store p = do
  globalStateStore <- readIORef store
  putStrLn $ "attempt to deref " ++ show p ++ " via global state store"
  case Map.lookup p globalStateStore of
    Nothing -> error "YOLO420"
    Just x  -> do
      putStrLn $ "returning deref res: " ++ showT x
      pure x

type FileBody = String

data Diff = LeafModified  (Name, FileBody, FileBody)
          | FileReplacedWithDir Name
          | DirReplacedWithFile Name
          | EntityAddedToDir
          | EntityRemovedFromDir
          | EntityRenamed (Name, Name)
          | EntityDeleted Name
          | EntityCreated Name
  deriving (Show)

-- TODO: hammer this into shape such that I can use bare-bones recursion schemes such as 'cata'
merklecata :: GlobalStore -> HashTerm (NamedEntity Tree) -> HashTerm (NamedEntity Tree) -> IO [Diff]
merklecata store ht1 ht2 = do
  -- putStrLn $ "merklecata: " ++ showHT ht1 ++ ", " ++ showHT ht2
  case (htPointer ht1 == htPointer ht2) of
    True  -> pure [] -- no need to explore further here
    False -> do -- hash mismatch - deref and explore further
      ne1 <- out <$> derefOneLayer ht1
      ne2 <- out <$> derefOneLayer ht2
      compareDerefed ne1 ne2

  where


    compareDerefed ne1 ne2 = do
      putStrLn $ "compareDerefed: " ++ showTree ne1 ++ ", " ++ showTree ne2
      compareDerefed' ne1 ne2

    compareDerefed' (NamedEntity name1 entity1) (NamedEntity name2 entity2)
      | name1 /= name2 =
          -- TODO: in this case compare entities and do the below if not ==
          -- if == aside from name have renamed case that should be handled
          pure [EntityDeleted name1, EntityCreated name2]
      | otherwise = -- no name mismatch, but known hash mismatch - must explore further
          case (entity1, entity2) of
            (Leaf fc1, Leaf fc2)
              | fc1 /= fc2   -> pure [LeafModified (name1, fc1, fc2)]
              -- ASSERTION: we can only get here if there's a hash diff, but
              --            if we have a hash diff then the file contents should differ!?!?
              | otherwise    -> error "wtf"
            (Node _, Leaf _) -> pure [DirReplacedWithFile name1]
            (Leaf _, Node _) -> pure [FileReplacedWithDir name1]
            (Node ns1, Node ns2) -> do
              let ns1Pointers = Set.fromList $ fmap htPointer ns1
                  ns2Pointers = Set.fromList $ fmap htPointer ns2

              -- DECISION: order of node children doesn't matter, so drop down to Set here
              let filteredNs1 = filter (not . flip Set.member (ns2Pointers) . htPointer) ns1
                  filteredNs2 = filter (not . flip Set.member (ns1Pointers) . htPointer) ns2

              derefedNs1 <- traverse derefOneLayer filteredNs1
              derefedNs2 <- traverse derefOneLayer filteredNs2

              let mkByNameMap :: [Term (NamedEntity Tree)] -> HashMap Name (Term (NamedEntity Tree))
                  mkByNameMap ns = Map.fromList $ fmap mkByNameMapEntry ns
                  mkByNameMapEntry t@(In (NamedEntity n _)) = (n, t)

                  byNameMap1 = mkByNameMap derefedNs1
                  byNameMap2 = mkByNameMap derefedNs2

              let cmpRes :: [These (Name, Term (NamedEntity Tree))]
                  cmpRes = mapCompare byNameMap1 byNameMap2

              -- putStrLn "by name maps 1/2, cmp res"
              -- print $ fmap showT byNameMap1
              -- print $ fmap showT byNameMap2
              -- print $ fmap (fmap (fmap showT)) cmpRes

              join <$> traverse resolveMapDiff cmpRes

    -- todo better name maybe?
    resolveMapDiff :: These (Name, Term (NamedEntity Tree))
                   -> IO [Diff]
    resolveMapDiff (This (name,_))     = pure [EntityDeleted name]
    resolveMapDiff (That (name,_))     = pure [EntityCreated name]
    resolveMapDiff (These (_,a) (_,b)) = compareDerefed (out a) (out b)

    derefOneLayer :: HashTerm (NamedEntity Tree) -> IO (Term (NamedEntity Tree))
    derefOneLayer ht = case ht of
      Direct _ t -> pure t
      Indirect p -> do
        t <- deref store p
        pure t



mapCompare :: Eq k => Hash.Hashable k => HashMap k v -> HashMap k v -> [These (k,v)]
mapCompare h1 h2 = h1Only ++ h2Only ++ both
  where h1Only = fmap This . Map.toList $ Map.difference h1 h2
        h2Only = fmap That . Map.toList $ Map.difference h2 h1
        both   = Map.elems $ Map.intersectionWithKey (\k v1 v2 -> These (k,v1) (k,v2)) h1 h2

-- | specialized to 'a a' to make functor derive easy... could do bifunctor?
data These a = This a | These a a | That a deriving (Eq, Ord, Show, Functor)

main :: IO ()
main = do
  globalStateStore <- newIORef Map.empty
  before <- buildDirTree globalStateStore "examples/before" "node1"
  after  <- buildDirTree globalStateStore "examples/after"  "node1"
  res <- merklecata globalStateStore before after
  print $ res

