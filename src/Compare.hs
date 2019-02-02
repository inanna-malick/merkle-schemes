{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Compare (compareMerkleTrees) where

--------------------------------------------
import           Control.Monad (join)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import           Data.IORef
import qualified Data.Set as Set
--------------------------------------------
import           Diff.Types
import           Util.RecursionSchemes (Term(..))
import           Util.These (These(..), mapCompare)
import           Merkle.Tree.Render
import           Merkle.Tree.Types
import           Merkle.Types
--------------------------------------------

-- | fetch a value from the global store. Pretend this involves a network call.
-- NOTE: could also encode at the type level that this only returns one layer via return type..
deref :: GlobalStore -> Pointer -> IO ConcreteMerkleTreeLayer
deref store p = do
  globalStateStore <- readIORef store
  putStrLn $ "attempt to deref " ++ show p ++ " via global state store"
  case Map.lookup p globalStateStore of
    Nothing -> error "YOLO420"
    Just x  -> do
      -- putStrLn $ "returning deref res: " ++ showT x
      pure x


-- | Compare two merkle trees for equality, producing diffs
--   lazily fetches structure of the two trees such that only the parts required
--   to do this comparison are fetched from the global state store (via 'network call')
-- TODO: hang on to full tree as fetched and log after?
compareMerkleTrees
  :: GlobalStore
  -> MerkleTree
  -> MerkleTree
  -> IO [Diff]
compareMerkleTrees store ht1 ht2 = do
  putStrLn $ "compareMerkleTrees: " ++ showMerkleTree ht1 ++ ", " ++ showMerkleTree ht2
  case (mtPointer ht1 == mtPointer ht2) of
    True  -> pure [] -- no need to explore further here
    False -> do -- hash mismatch - deref and explore further
      ne1 <- derefOneLayer ht1
      ne2 <- derefOneLayer ht2
      compareDerefed ne1 ne2

  where


    compareDerefed ne1 ne2 = do
      putStrLn $ "compareDerefed: " ++ showConcreteMerkleTreeLayer ne1 ++ ", " ++ showConcreteMerkleTreeLayer ne2
      compareDerefed' ne1 ne2

    compareDerefed' :: ConcreteMerkleTreeLayer -> ConcreteMerkleTreeLayer -> IO [Diff]
    compareDerefed' (NamedEntity name1 entity1) (NamedEntity name2 entity2)
      | name1 /= name2 =
          -- flatten out sub-entities to only contain pointers then check equality
          if (fmap mtPointer entity1 == fmap mtPointer entity2)
            then
              pure [EntityRenamed name1 name2]
            else
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
              let ns1Pointers = Set.fromList $ fmap mtPointer ns1
                  ns2Pointers = Set.fromList $ fmap mtPointer ns2

              -- DECISION: order of node children doesn't matter, so drop down to Set here
              let filteredNs1 = filter (not . flip Set.member (ns2Pointers) . mtPointer) ns1
                  filteredNs2 = filter (not . flip Set.member (ns1Pointers) . mtPointer) ns2

              derefedNs1 <- traverse derefOneLayer filteredNs1
              derefedNs2 <- traverse derefOneLayer filteredNs2

              let mkByNameMap :: [ConcreteMerkleTreeLayer] -> HashMap Name ConcreteMerkleTreeLayer
                  mkByNameMap ns = Map.fromList $ fmap (\e -> (neName e, e)) ns

                  cmpRes :: [These (Name, ConcreteMerkleTreeLayer) (Name, ConcreteMerkleTreeLayer)]
                  cmpRes = mapCompare (mkByNameMap derefedNs1) (mkByNameMap derefedNs2)

              join <$> traverse resolveMapDiff cmpRes

    resolveMapDiff :: These (Name, ConcreteMerkleTreeLayer) (Name, ConcreteMerkleTreeLayer)
                   -> IO [Diff]
    resolveMapDiff (This (name,_))     = pure [EntityDeleted name]
    resolveMapDiff (That (name,_))     = pure [EntityCreated name]
    resolveMapDiff (These (_,a) (_,b)) = compareDerefed a b

    derefOneLayer :: MerkleTree -> IO ConcreteMerkleTreeLayer
    derefOneLayer ht = case out ht of
      Direct _ t -> pure t
      Indirect p -> do
        t <- deref store p
        pure t
