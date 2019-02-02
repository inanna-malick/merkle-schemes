{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module RecursionSchemes where

-- | Fixed-point of type `f`
data Term (f :: * -> *) = In { out :: f (Term f) }

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata f = f . fmap (cata f) . out
