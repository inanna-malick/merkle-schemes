-- | Algebra names missing from recursion-schemes
module Util.RecursionSchemes
  ( Algebra
  , CoAlgebra
  , CVCoAlgebra
  , module Data.Functor.Foldable
  ) where

import Control.Monad.Free (Free)
import Data.Functor.Foldable

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a
type CVCoAlgebra f a = a -> f (Free f a)
