-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed pointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module Compare (compareMerkleTrees) where

--------------------------------------------
import           Control.Monad (join)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import qualified Data.Set as Set
--------------------------------------------
import           Deref (lazyDeref)
import           Diff.Types
import           Util.These (These(..), mapCompare)
import           Util.MyCompose
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types
import           Store (Store)
--------------------------------------------

-- | Diff two merkle trees, producing diffs and a record of expansions/derefs performed
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
       , ( Fix $ WithHash :+ Maybe :+ NamedTreeLayer
         , Fix $ WithHash :+ Maybe :+ NamedTreeLayer
         )
       )
compareMerkleTrees store mt1 mt2 =
  -- transform merkle trees (hash-addressed indirection) into lazily streaming data structures
  -- before passing to diffing alg
  compareMerkleTrees' (lazyDeref store mt1) (lazyDeref store mt2)


-- | Diff two merkle trees where the hash-identified nodes have been
--   converted to lazily-expanded effectful streams of values in some monadic stack 'm'
compareMerkleTrees'
  :: forall m
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => Fix $ WithHash :+ m :+ NamedTreeLayer
  -> Fix $ WithHash :+ m :+ NamedTreeLayer
  -> m ( [Diff]
       , ( Fix $ WithHash :+ Maybe :+ NamedTreeLayer
         , Fix $ WithHash :+ Maybe :+ NamedTreeLayer
         )
       )
compareMerkleTrees' t1 t2
  | pointer t1 == pointer t2
      -- no diff, no need to explore further here
      = pure ([], (unexpanded $ pointer t1, unexpanded $ pointer t2))
  | otherwise
      = do deref1 <- derefLayer t1
           deref2 <- derefLayer t2
           (diffres, (de1, de2)) <- compareDerefed deref1 deref2
           pure (diffres, (de1 $ pointer t1, de2 $ pointer t2))

  where
    compareDerefed
      :: NamedTreeLayer $ Fix $ WithHash :+ m :+ NamedTreeLayer
      -> NamedTreeLayer $ Fix $ WithHash :+ m :+ NamedTreeLayer
      -> m ( [Diff]
           -- functions used to build up structure - in this fn we have no access to pointers (having already checked ==)
           , ( Pointer -> Fix $ WithHash :+ Maybe :+ NamedTreeLayer
             , Pointer -> Fix $ WithHash :+ Maybe :+ NamedTreeLayer
             )
           )
    compareDerefed ne1@(C (name1, entity1)) ne2@(C (name2, entity2))
      | name1 /= name2 = do
          -- expansion for the case in which neither node is derefed or explored
          let shallowExpansion = (expandedShallow ne1, expandedShallow ne2)
          -- flatten out sub-entities to only contain pointers then check equality
          if (fmap pointer entity1 == fmap pointer entity2)
            then
              pure ([EntityRenamed name1 name2], shallowExpansion)
            else
              pure ([EntityDeleted name1, EntityCreated name2], shallowExpansion)
      | otherwise = do -- no name mismatch, but known hash mismatch - must explore further
          -- expansion for the case in which neither node is derefed or explored
          let shallowExpansion = (expandedShallow ne1, expandedShallow ne2)
          case (entity1, entity2) of
            (Leaf fc1, Leaf fc2)
              | fc1 /= fc2   -> pure ([LeafModified (name1, fc1, fc2)], shallowExpansion)
              -- ASSERTION we can only get here if there's a hash diff, but
              --            if we have a hash diff then the file contents should differ.
              -- NOTE: this indicates a situation where two hashes are /= but the hash-addressed
              --       file contents they describe are ==. fail silently (TODO: note error?)
              -- NOTE: making this function pure in 'm' is pretty nice, logging this error
              --       would require some additional error type that I don't want to think about
              --       right now
              | otherwise    -> pure ([], (unexpanded, unexpanded))
            (Node _, Leaf _) -> pure ([DirReplacedWithFile name1], shallowExpansion)
            (Leaf _, Node _) -> pure ([FileReplacedWithDir name1], shallowExpansion)
            -- most of the complexity of this function is here - this
            -- is where the children of two nodes being diffed are compared.
            -- this requires derefing all nodes for which there is a hash
            -- mismatch and using the names of the resulting named entities to
            -- compare nodes with the same names
            (Node ns1, Node ns2) -> do
              let ns1Pointers = Set.fromList $ fmap pointer ns1
                  ns2Pointers = Set.fromList $ fmap pointer ns2

              -- DECISION: order of node children doesn't matter, so drop down to Set here
              let exploredNs1 = filter (not . flip Set.member (ns2Pointers) . pointer) ns1
                  exploredNs2 = filter (not . flip Set.member (ns1Pointers) . pointer) ns2
                  -- for construting 'unexpanded' branches
                  unexploredNs1 :: [Fix $ WithHash :+ Maybe :+ NamedTreeLayer]
                  unexploredNs1  = fmap (unexpanded . pointer) $ filter (flip Set.member (ns2Pointers) . pointer) ns1
                  unexploredNs2 :: [Fix $ WithHash :+ Maybe :+ NamedTreeLayer]
                  unexploredNs2  = fmap (unexpanded . pointer) $ filter (flip Set.member (ns1Pointers) . pointer) ns2

              derefedNs1 <- traverse (\x -> fmap C . fmap (pointer x,) $ derefLayer x) exploredNs1
              derefedNs2 <- traverse (\x -> fmap C . fmap (pointer x,) $ derefLayer x) exploredNs2

              let mkByNameMap :: [WithHash :+ NamedTreeLayer $ Fix $ WithHash :+ m :+ NamedTreeLayer]
                              -> HashMap Name $ WithHash :+ NamedTreeLayer
                                              $ Fix $ WithHash :+ m :+ NamedTreeLayer
                  mkByNameMap ns = Map.fromList $ fmap (\e@(C (_, C (n, _))) -> (n, e)) ns
                  resolveMapDiff
                    (This (n,_)) = pure ( [EntityDeleted n]
                                       , Nothing
                                       )

                  resolveMapDiff
                    (These (_, C (p1,a)) (_, C (p2,b))) = do
                      (diffs, (de1,de2)) <- compareDerefed a b
                      pure (diffs, Just (de1 p1, de2 p2))

                  resolveMapDiff
                    (That (n,_)) = pure ( [EntityCreated n]
                                      , Nothing
                                      )

              recurseRes <-
                traverse resolveMapDiff $ mapCompare (mkByNameMap derefedNs1) (mkByNameMap derefedNs2)

              -- kinda gross - basically just taking all the results and hammering them into the right shape
              let diffs = join $ fmap fst recurseRes
                  rExpansions1 = recurseRes >>= (maybe [] (pure . fst) . snd)
                  rExpansions2 = recurseRes >>= (maybe [] (pure . snd) . snd)

                  expand name nodes = expanded $ C (name, Node nodes)
                  expansions1 = expand name1 $ unexploredNs1 ++ rExpansions1
                  expansions2 = expand name2 $ unexploredNs2 ++ rExpansions2

              pure (diffs, (expansions1, expansions2))



unexpanded
  :: Pointer
  -> Fix $ WithHash :+ Maybe :+ NamedTreeLayer
unexpanded p = Fix $ C (p, C Nothing)

expanded
  :: NamedTreeLayer $ Fix $ WithHash :+ Maybe :+ NamedTreeLayer
  -> Pointer
  -> Fix $ WithHash :+ Maybe :+ NamedTreeLayer
expanded x p = Fix $ C (p, C $ Just x)

-- todo better name?
expandedShallow
  :: forall g
   . NamedTreeLayer $ (Fix (WithHash :+ g))
  -> Pointer
  -> Fix $ WithHash :+ Maybe :+ NamedTreeLayer
expandedShallow x = expanded (fmap (\(Fix (C (p,_))) -> Fix $ C (p, C Nothing)) x)


derefLayer :: Fix $ WithHash :+ m :+ NamedTreeLayer
           -> m $ NamedTreeLayer $ Fix $ WithHash :+ m :+ NamedTreeLayer
derefLayer   = getCompose . snd . getCompose . unFix
