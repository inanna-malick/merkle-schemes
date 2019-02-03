{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Util.RecursionSchemes where

-- | Fixed-point of type `f`
data Term (f :: * -> *) = In { out :: f (Term f) }

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata f = f . fmap (cata f) . out

-- | A coalgebra, but everything's in some arbitrary monad stack? wild.
type MonadicCoAlgebra m f a = a -> m (f a)

-- | An anamorphism, but everything's in some arbitrary monad stack? wild.
anaButInM :: (Traversable f, Monad m) => MonadicCoAlgebra m f a -> a -> m (Term f)
anaButInM alg = fmap In . (>>= traverse (anaButInM alg)) . alg

type MonadicAnnotaterAlg m f g
  = f (Term (g f)) -> m (Term (g f))

annotateWithLayer :: (Traversable f, Monad m) => MonadicAnnotaterAlg m f g -> Term f -> m (Term (g f))
annotateWithLayer alg = (>>= alg) . traverse (annotateWithLayer alg) . out
