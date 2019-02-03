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

type MonadicAlgebra m f a = f a -> m a

cataButInM :: (Monad m, Traversable f) => MonadicAlgebra m f a -> Term f -> m a
cataButInM alg = (>>= alg) . traverse (cataButInM alg) . out


-- | A coalgebra, but everything's in some arbitrary monad stack? wild.
type MonadicCoAlgebra m f a = a -> m (f a)

-- | An anamorphism, but everything's in some arbitrary monad stack? wild.
anaButInM :: (Traversable f, Monad m) => MonadicCoAlgebra m f a -> a -> m (Term f)
anaButInM alg = fmap In . (>>= traverse (anaButInM alg)) . alg

type MonadicAnnotaterAlg m f g
  = f (Term (g f)) -> m (Term (g f))

annotateWithLayer :: (Traversable f, Monad m) => MonadicAnnotaterAlg m f g -> Term f -> m (Term (g f))
annotateWithLayer alg = (>>= alg) . traverse (annotateWithLayer alg) . out

type DeAnotater f g
  = Term (g f) -> f (Term (g f))

deAnnotate :: (Functor f) => DeAnotater f g -> Term (g f) -> Term f
deAnnotate alg = In . fmap (deAnnotate alg) . alg

type MonadicDeAnotater m f g
  = Term (g f) -> m (f (Term (g f)))

deAnnotateM :: (Traversable f, Monad m) => MonadicDeAnotater m f g -> Term (g f) -> m (Term f)
deAnnotateM alg = fmap In . (>>= traverse (deAnnotateM alg)) . alg
