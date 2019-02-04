{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}


-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed pointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module Compare (compareMerkleTrees) where

--------------------------------------------
import           Control.Monad.Except
import           Data.Functor.Compose
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import qualified Data.Set as Set
--------------------------------------------
import           Deref
import           Diff.Types
import           Errors
import           Util.These -- (These(..), mapCompare)
import           Util.Util (mapErrUtil)
import           Util.RecursionSchemes
import           Merkle.Tree.Render
import           Merkle.Tree.Types
import           Merkle.Types
import           Store
--------------------------------------------


-- compare two merkle trees where the hash-identified nodes have been
-- converted to lazily-expanded effectful streams of values in some monadic stack 'm'
compareMerkleTrees'
  :: forall m
   . MonadIO m
  => Term (HashAnnotatedEffectfulStreamF m)
  -> Term (HashAnnotatedEffectfulStreamF m)
  -> m [Diff]
compareMerkleTrees' t1 t2
  | haesfPointer t1 == haesfPointer t2
      = pure [] -- no need to explore further here
  | otherwise
      = do deref1 <- haesfDeref t1
           deref2 <- haesfDeref t2
           compareDerefed deref1 deref2

  where
    compareDerefed
      :: (NamedEntity Tree (Term (HashAnnotatedEffectfulStreamF m)))
      -> (NamedEntity Tree (Term (HashAnnotatedEffectfulStreamF m)))
      -> m [Diff]
    compareDerefed (NamedEntity name1 entity1) (NamedEntity name2 entity2)
      | name1 /= name2 = do
          -- flatten out sub-entities to only contain pointers then check equality
          if (fmap haesfPointer entity1 == fmap haesfPointer entity2)
            then
              pure [EntityRenamed name1 name2]
            else
              pure [EntityDeleted name1, EntityCreated name2]
      | otherwise = -- no name mismatch, but known hash mismatch - must explore further
          case (entity1, entity2) of
            (Leaf fc1, Leaf fc2)
              | fc1 /= fc2   -> pure [LeafModified (name1, fc1, fc2)]
              -- ASSERTION we can only get here if there's a hash diff, but
              --            if we have a hash diff then the file contents should differ!?!?
              -- NOTE: this indicates a situation where two hashes are /= but the hash-addressed
              --       file contents they describe are ==. fail silently (TODO: note error?)
              -- NOTE: making this function pure in 'm' is pretty nice, logging this error
              --       would require some additional error type that I don't want to think about
              --       right now
              | otherwise    -> pure []
            (Node _, Leaf _) -> pure [DirReplacedWithFile name1]
            (Leaf _, Node _) -> pure [FileReplacedWithDir name1]
            (Node ns1, Node ns2) -> do
              let ns1Pointers = Set.fromList $ fmap haesfPointer ns1
                  ns2Pointers = Set.fromList $ fmap haesfPointer ns2

              -- DECISION: order of node children doesn't matter, so drop down to Set here
              let filteredNs1 = filter (not . flip Set.member (ns2Pointers) . haesfPointer) ns1
                  filteredNs2 = filter (not . flip Set.member (ns1Pointers) . haesfPointer) ns2

              derefedNs1 <- traverse haesfDeref filteredNs1
              derefedNs2 <- traverse haesfDeref filteredNs2

              let mkByNameMap :: [HashAnnotatedEffectfulStreamLayer m]
                              -> HashMap Name (HashAnnotatedEffectfulStreamLayer m)
                  mkByNameMap ns = Map.fromList $ fmap (\e -> (neName e, e)) ns

              fmap join . traverse resolveMapDiff $ mapCompare (mkByNameMap derefedNs1) (mkByNameMap derefedNs2)

    haesfPointer :: forall f . Term (Compose ((,) Pointer) f) -> Pointer
    haesfPointer = fst . getCompose . out

    haesfDeref :: Term (HashAnnotatedEffectfulStreamF m)
               -> m (HashAnnotatedEffectfulStreamLayer m)
    haesfDeref   = getCompose . snd . getCompose . out

    resolveMapDiff :: These (Name, HashAnnotatedEffectfulStreamLayer m) (Name, HashAnnotatedEffectfulStreamLayer m)
                   -> m [Diff]
    resolveMapDiff = these
      (pure . pure . EntityDeleted . fst)
      (\(_, a) (_, b) -> compareDerefed a b)
      (pure . pure . EntityCreated . fst)

-- | Compare two merkle trees for equality, producing diffs
--   lazily fetches structure of the two trees such that only the parts required
--   to do this comparison are fetched from the global state store (via 'network call')
-- TODO: hang on to full tree as fetched and log after? would help in testing (asserting only some chunk was fetched..)
compareMerkleTrees
  :: forall m
   . Monad m
  => Store m
  -> MerkleTree
  -> MerkleTree
  -> m [Diff]
compareMerkleTrees store mt1 mt2 = undefined
  where
    lazyExpandTree
      :: CVCoAlgebra (HashAnnotatedEffectfulStreamF m) MerkleTree
    lazyExpandTree (In (Direct p e)) =
      let e' :: (HashAnnotatedEffectfulStreamF m) MerkleTree
          e' = Compose (p, Compose $ pure $ e)
          e'' :: (HashAnnotatedEffectfulStreamF m) (CoAttr (HashAnnotatedEffectfulStreamF m) MerkleTree)
          e'' = Automatic <$> e'
       in e''

    -- lazyExpandTree (In (Indirect p))
    --   =
    --   let d :: m ConcreteMerkleTreeLayer -- store can return multiple tree layers
    --       d  = deref store p
    --       d' :: m (CoAttr (HashAnnotatedEffectfulStreamF m) MerkleTree)
    --       d' = fmap (Manual . fmap (ana lazyExpandTree)) d
    --       -- d'' :: Term (HashAnnotatedEffectfulStreamF m)
    --       -- d''  = (In . Compose . (p,) . Compose) d'
    --    in Compose (p, d')

type HashAnnotatedEffectfulStreamF m
  = Compose ((,) Pointer) (Compose m (NamedEntity Tree))

type HashAnnotatedEffectfulStreamLayer m =
  NamedEntity Tree (Term (HashAnnotatedEffectfulStreamF m))
