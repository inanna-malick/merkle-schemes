{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import GHC.Generics (Generic)

import qualified Data.Hashable as Hash

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)


-- type EntityBody = String

-- -- hash store (todo: artificial IO wrapper around access to repr. network call)
-- -- note: having it like this allows for net svc to return multiple layers in one call as optimization
-- data Entity a
--   = Pointer HashId
--   | Entity HashId a
--   deriving (Eq, Show)


-- -- note: use special fn here - can't use recursive derived has for HashDag...
-- -- that's fine I guess - only need hashable for Tree -> HashTree step



-- -- TODO: should this have phantom type param to avoid using key from wrong thing?
-- newtype HashId
--   = HashId { unHashId :: Int }
--   deriving (Eq, Show)

-- data HashStore a
--    = HashStore { unHashStore :: HashMap HashId (Entity a)}
--    deriving (Eq, Show)

-- newtype NetworkStore a
--   = NetworkStore { unNetworkStore :: HashStore a }
--   deriving (Eq, Show)

-- -- note: make type param'd so I can recurse through and annotate with hashes (note: use RC pls lol)
-- -- note: tree for v1, dag for v2
-- data Tree     = Node [Tree] | Leaf String deriving (Eq, Show, Generic)

-- -- | tree annotated at each level with hashes
-- data HashTree = HashNode [Entity HashTree] | HashLeaf String deriving (Eq, Show, Generic)

-- -- instance Hash.Hashable HashTree
-- -- instance Generic a => Hash.Hashable (Entity a)
-- -- instance Hash.Hashable HashId

-- -- disobeys eq law type stuff so not using hashable instance (pointer and entity can have hash equality)
-- hashTreeHash :: HashTree -> HashId
-- hashTreeHash (HashNode es) = HashId $ Hash.hash $ fmap entityHash es
-- hashTreeHash (HashLeaf s)  = HashId $ Hash.hash $ s

-- entityHash :: Entity HashTree -> HashId
-- entityHash (Pointer hid)  = hid
-- entityHash (Entity hid _) = hid


-- -- | note: node hash is derived from.. what? needs to just be hash of entity, not hash of raw structure, right... nah, should be hash of hashtree.. natural structure is that of algebra over rec. schemes...
-- -- treeToHashTree :: Tree -> Entity HashTree
-- -- treeToHashTree n@(Node ts) =
-- --   let subtrees = fmap treeToHashTree ts
      
-- -- treeToHashTree l@(Leaf s)  = HashLeaf s

-- data OutOfContextError = BrokenLink | HashValidationFail deriving Show

-- -- given fully populated list populate hash store (usually network store for setting up test)

-- -- note: resulting tree should be fully realized
-- -- todo version of this that takes fn that allows for early traversal termination
-- -- todo: does this need to require that it start with a pointer (via hashId param)? or just any entity?
-- fullyLookup :: HashStore HashTree -> HashId -> Either OutOfContextError Tree
-- fullyLookup hs id = f $ Pointer id
--   where
--     f :: Entity HashTree -> Either OutOfContextError Tree
--     f (Pointer id') = case Map.lookup id' (unHashStore hs) of
--       Nothing  -> Left BrokenLink
--       Just res
--         | Hash.hash res == id -> g res
--         | otherwise           -> Left HashValidationFail
--     f (Entity id' tree) = g tree

--     g :: HashTree -> Either OutOfContextError Tree
--     g (HashNode es) = fmap Node $ traverse f es
--     g (HashLeaf v)  = Right $ Leaf v


-- -- networkHashStoreLookup
-- --   :: NetworkStore a -- remote store having hash-driven lookup
-- --   -> HashId         -- hash id to look up
-- --   -> HashStore a    -- local hash store state (invariant: shouldn't have HashId for lookup?)
-- --   -> IO (Either NetworkStoreError (HashStore, a))
-- -- networkHashStoreLookup ns hid ls =
-- --   case Map.lookup hid lhs of
-- --     Just localRes -> pure $ Right localRes
-- --     Nothing ->
-- --       -- network call ;)
-- --       case Map.lookup hid (unNetworkStore ns) of
-- --         Just remoteRes ->
-- --           let updatedLocalState = Map.insert hid $ Entity hid remoteRes
-- --            in pure $ Right (updatedLocalState, remoteRes)
-- --         Nothing -> pure $ Left NetworkStoreBrokenLink


data Pointer (f :: * -> *) = Pointer Int -- note: why does f require kind sig?
data Concept a = ConceptTree [a] | Leaf String

data Term (f :: * -> *) = In { out :: f (HashTerm f) }
data HashTerm (f :: * -> *)
  = Direct (Pointer f) (Term f) -- node id is included in node metadata of direct ref (pointer)
  | Indirect (Pointer f)        -- indirect ref is just a pointer

-- type-level guarantee that it pops a layer off the hash stack, nice
deref :: Pointer f -> IO (Term f)
deref = undefined -- network call goes here (todo println to debug along w/ w/e lookup)


type Algebra f a = f a -> a
type HaltDeterminer f a = f a -> Bool -- doesn't know nuthin about 'a', must determine solely on structure of thingy...
-- actually, looks like this won't work - need to compare 2x branch's hashterm structure, not tree structure..

-- | yeet yueet
-- todo bake in failure modes (either return type to represent deref fail, can 'error' for now)
merklecata :: (Traversable f, Functor f) => Algebra f a -> HashTerm f -> IO a
merklecata alg ht = case ht of
  Direct _ t -> -- could use node pointer to do checksum here.. don't tho
    fmap alg . traverse (merklecata alg) $ out t
  Indirect p -> do
    t <- deref p
    fmap alg . traverse (merklecata alg) $ out t


-- | just the tree
type SansHash = Term Concept
-- problem: this is good for single recursion but lazy stopping (since this is IO) needs
-- to be controlable via algebra - needs some way to terminate recursion
-- step one is to solve this.

-- problem: this also doesn't allow for zip-wise comparison




main = do
  putStrLn "Hello"
  putStrLn "World"
