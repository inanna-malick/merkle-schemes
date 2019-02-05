{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Deref where

--------------------------------------------
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types
import           Store
--------------------------------------------
import           Util.MyCompose
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
    alg (C (p, C e)) =
      do
        e' <- e
        e'' <- traverse id e'
        pure $ In $ C (p, e'')


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
    alg :: CVCoAlgebra ((,) Pointer :+ m :+ NamedEntity :+ Tree) Pointer
    alg p = C (p, C $ handleCMTL <$> deref store p)

    handleCMTL (C (NamedEntity name e))
      = C . NamedEntity name $ fmap (handleMTL) e

    handleMTL (In (C (Direct p e))) = Manual $ C (p, C . pure $ handleCMTL e)
    handleMTL (In (C (Indirect p))) = Automatic p

type PartiallyExpandedHashAnnotatedTree
  = Term PartiallyExpandedHashAnnotatedTreeF

type PartiallyExpandedHashAnnotatedTreeF
  = (,) Pointer :+ Maybe :+ NamedEntity :+ Tree

unexpanded
  :: Pointer
  -> PartiallyExpandedHashAnnotatedTree
unexpanded p = In $ C (p, C Nothing)

expanded
  :: (NamedEntity :+ Tree) PartiallyExpandedHashAnnotatedTree
  -> Pointer
  -> PartiallyExpandedHashAnnotatedTree
expanded x p = In $ C (p, C $ Just x)

-- todo better name?
expandedShallow
  :: forall g
   . (NamedEntity :+ Tree) (Term ((,) Pointer :+ g))
  -> Pointer
  -> PartiallyExpandedHashAnnotatedTree
expandedShallow x = expanded (fmap (\(In (C (p,_))) -> In $ C (p, C Nothing)) x)

type HashAnnotatedTree
  = (,) Pointer :+ NamedEntity :+ Tree

type HashAnnotatedEffectfulStreamF m
  = (,) Pointer :+ m :+ NamedEntity :+ Tree

type HashAnnotatedEffectfulStreamLayer m
  = (NamedEntity :+ Tree) (Term (HashAnnotatedEffectfulStreamF m))
