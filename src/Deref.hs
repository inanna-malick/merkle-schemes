{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}


module Deref where

--------------------------------------------
import           Data.Functor.Compose
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types
import           Store
--------------------------------------------

strictDeref
  :: forall m
   . Monad m
  => Store m
  -> Pointer
  -> m (Term HashAnnotatedTree)
strictDeref store = cata alg . lazyDeref store
  where
    alg :: Algebra (HashAnnotatedEffectfulStreamF m) (m (Term HashAnnotatedTree))
    alg (Compose (p, e)) =
      do
        e' <- getCompose e
        e'' <- traverse id e'
        pure $ In $ Compose (p, e'')


-- construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall m
   . Monad m
  => Store m
  -> Pointer
  -> Term (HashAnnotatedEffectfulStreamF m)
lazyDeref store = futu alg
  where
    alg :: CVCoAlgebra (HashAnnotatedEffectfulStreamF m) Pointer
    alg p = Compose (p, Compose $ handleCMTL <$> deref store p)

    -- handleCMTL :: Term (Compose HashIdentifiedEntity MerkleTreeLayer)
    --           -> CoAttr (Compose ((,) Pointer) (Compose m (Compose NamedEntity Tree))) Pointer
    handleCMTL (Compose (NamedEntity name e))
      = Compose . NamedEntity name $ fmap (handleMTL) e


    handleMTL :: Term (Compose HashIdentifiedEntity MerkleTreeLayer)
              -> CoAttr (Compose ((,) Pointer) (Compose m (Compose NamedEntity Tree))) Pointer

    handleMTL (In (Compose (Direct p e))) = Manual $ Compose (p, Compose . pure $ handleCMTL e)
    handleMTL (In (Compose (Indirect p))) = Automatic p

-- type PartiallyExpandedHashAnnotatedTree
--   = Term PartiallyExpandedHashAnnotatedTreeF

-- type PartiallyExpandedHashAnnotatedTreeF
--   = Compose ((,) Pointer) (Compose Maybe (NamedEntity Tree))

-- unexpanded :: Pointer -> PartiallyExpandedHashAnnotatedTree
-- unexpanded p = Compose (p, Compose Nothing)

type HashAnnotatedTree
  = Compose ((,) Pointer) (Compose NamedEntity Tree)

type HashAnnotatedEffectfulStreamF m
  = Compose ((,) Pointer) (Compose m (Compose NamedEntity Tree))

type HashAnnotatedEffectfulStreamLayer m
  = Compose NamedEntity Tree (Term (HashAnnotatedEffectfulStreamF m))
