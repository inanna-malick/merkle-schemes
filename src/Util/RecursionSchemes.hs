module Util.RecursionSchemes where

-- | Fixed-point of type `f`
data Fix (f :: * -> *) = Fix { unFix :: f (Fix f) }

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . unFix

type CoAlgebra f a = a -> f a

ana
  :: (Functor f)
  => CoAlgebra f a
  -> a
  -> Fix f
ana alg = Fix . (fmap (ana alg)) . alg

data CoAttr f a
  = Automatic a
  | Manual (f (CoAttr f a))

type CVCoAlgebra f a = a -> f (CoAttr f a)

futu :: forall f a . Functor f => CVCoAlgebra f a -> a -> Fix f
futu f = Fix . fmap worker . f
  where
    worker :: CoAttr f a -> Fix f
    worker (Automatic a) = futu f a        -- continue through this level
    worker (Manual g) = Fix (fmap worker g) -- omit folding this level,
                                           -- delegating to the worker
                                           -- to perform any needed
                                           -- unfolds later on.
