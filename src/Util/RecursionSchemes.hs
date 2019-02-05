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


type CoAlgebra f a = a -> f a

-- | An anamorphism, but everything's in some arbitrary monad stack? wild.
ana
  :: (Functor f)
  => CoAlgebra f a
  -> a
  -> Term f
ana alg = In . (fmap (ana alg)) . alg


-- | A coalgebra, but everything's in some arbitrary monad stack? wild.
type MonadicCoAlgebra m f a = a -> m (f a)

-- | An anamorphism, but everything's in some arbitrary monad stack? wild.
anaButInM
  :: (Traversable f, Monad m)
  => MonadicCoAlgebra m f a
  -> a
  -> m (Term f)
anaButInM alg = fmap In . (>>= traverse (anaButInM alg)) . alg

type MonadicAnnotaterAlg m f g
  = f (Term (g f)) -> m (Term (g f))

annotateWithLayer
  :: (Traversable f, Monad m)
  => MonadicAnnotaterAlg m f g
  -> Term f
  -> m (Term (g f))
annotateWithLayer alg = (>>= alg) . traverse (annotateWithLayer alg) . out

type DeAnotater f g
  = Term (g f) -> f (Term (g f))

deAnnotate
  :: (Functor f)
  => DeAnotater f g
  -> Term (g f)
  -> Term f
deAnnotate alg = In . fmap (deAnnotate alg) . alg

type MonadicDeAnotater m f g
  = Term (g f) -> m (f (Term (g f)))

deAnnotateM
  :: (Traversable f, Monad m)
  => MonadicDeAnotater m f g
  -> Term (g f)
  -> m (Term f)
deAnnotateM alg = fmap In . (>>= traverse (deAnnotateM alg)) . alg

data CoAttr f a
  = Automatic a
  | Manual (f (CoAttr f a))

type CVCoAlgebra f a = a -> f (CoAttr f a)

futu :: forall f a . Functor f => CVCoAlgebra f a -> a -> Term f
futu f = In . fmap worker . f
  where
    worker :: CoAttr f a -> Term f
    worker (Automatic a) = futu f a        -- continue through this level
    worker (Manual g) = In (fmap worker g) -- omit folding this level,
                                           -- delegating to the worker
                                           -- to perform any needed
                                           -- unfolds later on.
