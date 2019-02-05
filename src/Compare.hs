{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}


-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed pointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module Compare (compareMerkleTrees) where

--------------------------------------------
import           Control.Monad.Except
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import qualified Data.Set as Set
--------------------------------------------
import           Deref
import           Diff.Types
import           Util.These
import           Util.MyCompose
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types
import           Store
--------------------------------------------

-- compare two merkle trees where the hash-identified nodes have been
-- converted to lazily-expanded effectful streams of values in some monadic stack 'm'
compareMerkleTrees'
  :: forall m
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => Term (WithHash :+ m :+ NamedTreeLayer)
  -> Term (WithHash :+ m :+ NamedTreeLayer)
  -> m ( [Diff]
       , ( Term (WithHash :+ Maybe :+ NamedTreeLayer)
         , Term (WithHash :+ Maybe :+ NamedTreeLayer)
         )
       )
compareMerkleTrees' t1 t2
  | haesfPointer t1 == haesfPointer t2
      -- no diff, no need to explore further here
      = pure ([], (unexpanded $ haesfPointer t1, unexpanded $ haesfPointer t2))
  | otherwise
      = do deref1 <- haesfDeref t1
           deref2 <- haesfDeref t2
           (diffres, (de1, de2)) <- compareDerefed deref1 deref2
           pure (diffres, (de1 $ haesfPointer t1, de2 $ haesfPointer t2))

  where
    compareDerefed
      :: NamedTreeLayer $ Term (WithHash :+ m :+ NamedTreeLayer)
      -> NamedTreeLayer $ Term (WithHash :+ m :+ NamedTreeLayer)
      -> m ( [Diff]
           -- functions used to build up structure - in this fn we have no access to pointers (having already checked ==)
           , ( Pointer -> Term (WithHash :+ Maybe :+ NamedTreeLayer)
             , Pointer -> Term (WithHash :+ Maybe :+ NamedTreeLayer)
             )
           )
    compareDerefed ne1@(C (NamedEntity name1 entity1)) ne2@(C (NamedEntity name2 entity2))
      | name1 /= name2 = do
          -- flatten out sub-entities to only contain pointers then check equality
          if (fmap haesfPointer entity1 == fmap haesfPointer entity2)
            then
              pure ([EntityRenamed name1 name2], (expandedShallow ne1, expandedShallow ne2))
            else
              pure ([EntityDeleted name1, EntityCreated name2], (expandedShallow ne1, expandedShallow ne2))
      | otherwise = -- no name mismatch, but known hash mismatch - must explore further
          case (entity1, entity2) of
            (Leaf fc1, Leaf fc2)
              | fc1 /= fc2   -> pure ([LeafModified (name1, fc1, fc2)], (expandedShallow ne1, expandedShallow ne2))
              -- ASSERTION we can only get here if there's a hash diff, but
              --            if we have a hash diff then the file contents should differ!?!?
              -- NOTE: this indicates a situation where two hashes are /= but the hash-addressed
              --       file contents they describe are ==. fail silently (TODO: note error?)
              -- NOTE: making this function pure in 'm' is pretty nice, logging this error
              --       would require some additional error type that I don't want to think about
              --       right now
              | otherwise    -> pure ([], (unexpanded, unexpanded))
            (Node _, Leaf _) -> pure ([DirReplacedWithFile name1], (expandedShallow ne1, expandedShallow ne2))
            (Leaf _, Node _) -> pure ([FileReplacedWithDir name1], (expandedShallow ne2, expandedShallow ne1))
            (Node ns1, Node ns2) -> do
              let ns1Pointers = Set.fromList $ fmap haesfPointer ns1
                  ns2Pointers = Set.fromList $ fmap haesfPointer ns2

              -- DECISION: order of node children doesn't matter, so drop down to Set here
              let exploredNs1 = filter (not . flip Set.member (ns2Pointers) . haesfPointer) ns1
                  exploredNs2 = filter (not . flip Set.member (ns1Pointers) . haesfPointer) ns2
                  -- for construting 'unexpanded' branches
                  unexploredNs1 :: [Term (WithHash :+ Maybe :+ NamedTreeLayer)]
                  unexploredNs1  = fmap (unexpanded . haesfPointer) $ filter (flip Set.member (ns2Pointers) . haesfPointer) ns1
                  unexploredNs2 :: [Term (WithHash :+ Maybe :+ NamedTreeLayer)]
                  unexploredNs2  = fmap (unexpanded . haesfPointer) $ filter (flip Set.member (ns1Pointers) . haesfPointer) ns2

              derefedNs1 <- traverse (\x -> fmap C . fmap (haesfPointer x,) $ haesfDeref x) exploredNs1
              derefedNs2 <- traverse (\x -> fmap C . fmap (haesfPointer x,) $ haesfDeref x) exploredNs2

              -- MAIN PROBLEM: layer + term type is fucking ungainly, ugly
              let mkByNameMap :: [WithHash :+ NamedTreeLayer $ Term (WithHash :+ m :+ NamedTreeLayer)]
                              -> HashMap Name $ WithHash :+ NamedTreeLayer
                                              $ Term (WithHash :+ m :+ NamedTreeLayer)
                  mkByNameMap ns = Map.fromList $ fmap (\e@(C (_, C (NamedEntity n _))) -> (n, e)) ns
                  resolveMapDiff t = case t of -- todo lambdacase
                    This (n,_) -> pure ( [EntityDeleted n]
                                       , Nothing
                                       )

                    These (_, C (p1,a)) (_, C (p2,b)) -> do
                      (diffs, (de1,de2)) <- compareDerefed a b
                      pure (diffs, Just (de1 p1, de2 p2))

                    That (n,_) -> pure ( [EntityCreated n]
                                       , Nothing
                                       )


              recurseRes <-
                traverse resolveMapDiff $ mapCompare (mkByNameMap derefedNs1) (mkByNameMap derefedNs2)

              -- kinda gross - basically just taking all the results and hammering them into the right shape
              let diffs = join $ fmap fst recurseRes
                  rExpansions1 = recurseRes >>= (maybe [] (pure . fst) . snd)
                  rExpansions2 = recurseRes >>= (maybe [] (pure . snd) . snd)

                  expand name nodes = expanded $ C $ NamedEntity name $ Node nodes
                  expansions1 = expand name1 $ unexploredNs1 ++ rExpansions1
                  expansions2 = expand name2 $ unexploredNs2 ++ rExpansions2

              pure (diffs, (expansions1, expansions2))

-- | Compare two merkle trees for equality, producing diffs and a record of expansions/derefs performed
--   lazily fetches structure of the two trees such that only the parts required
--   to do this comparison are fetched from the global state store (via 'network call')
compareMerkleTrees
  :: forall m
  -- no knowledge about actual monad stack - just knows it's the same
  -- as used by the store, which lets us create a lazy effectful streaming structure
   . Monad m
  => Store m
  -> Pointer -- top level interface is just pointers!
  -> Pointer -- top level interface is just pointers!
  -> m ( [Diff] -- resulting diffs
       -- partially substantiated trees - can be used to track how far tree traversal went
       , ( Term (WithHash :+ Maybe :+ NamedTreeLayer)
         , Term (WithHash :+ Maybe :+ NamedTreeLayer)
         )
       )
compareMerkleTrees store mt1 mt2 =
  -- transform merkle trees (hash-addressed indirection) into lazily streaming data structures
  -- before passing to diffing alg
  compareMerkleTrees' (lazyDeref store mt1) (lazyDeref store mt2)
