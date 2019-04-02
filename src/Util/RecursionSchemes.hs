module Util.RecursionSchemes
  ( module Data.Functor.Foldable
  , module Control.Monad.Free
  , module Util.RecursionSchemes
  ) where

import Data.Functor.Compose
import Control.Monad.Free hiding (unfold)
import Data.Functor.Foldable

type Algebra f a = f a -> a
type AlgebraM m f a = f a -> m a

type CoAlgebra f a = a -> f a
type CoAlgebraM m f a = a -> m (f a)

type CVCoAlgebra f a = a -> f (Free f a)

annotate
  :: Functor f
  => Algebra f x
  -> Fix f -> Fix ((,) x `Compose` f)
annotate alg = cata alg'
  where
    alg' f = Fix . Compose $  (alg $ fmap (fst . getCompose . unfix) f, f)

anaM
  :: Monad m => Traversable f
  => CoAlgebraM m f a
  -> a -> m (Fix f)
anaM f = fmap Fix . (>>= traverse (anaM f)) . f

cataM
  :: Monad m => Traversable f
  => AlgebraM m f a
  -> Fix f -> m a
cataM f = (>>= f) . traverse (cataM f) . unfix


