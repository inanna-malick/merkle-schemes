module Util.RecursionSchemes where

-- | Fixed-point of type `f`
data Fix (f :: * -> *) = In { unFix :: f (Fix f) }

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . unFix

type MonadicAlgebra m f a = f a -> m a

cataButInM :: (Monad m, Traversable f) => MonadicAlgebra m f a -> Fix f -> m a
cataButInM alg = (>>= alg) . traverse (cataButInM alg) . unFix


type CoAlgebra f a = a -> f a

-- | An anamorphism, but everything's in some arbitrary monad stack? wild.
ana
  :: (Functor f)
  => CoAlgebra f a
  -> a
  -> Fix f
ana alg = In . (fmap (ana alg)) . alg


-- | A coalgebra, but everything's in some arbitrary monad stack? wild.
type MonadicCoAlgebra m f a = a -> m (f a)

-- | An anamorphism, but everything's in some arbitrary monad stack? wild.
anaButInM
  :: (Traversable f, Monad m)
  => MonadicCoAlgebra m f a
  -> a
  -> m (Fix f)
anaButInM alg = fmap In . (>>= traverse (anaButInM alg)) . alg

type MonadicAnnotaterAlg m f g
  = f (Fix (g f)) -> m (Fix (g f))

annotateWithLayer
  :: (Traversable f, Monad m)
  => MonadicAnnotaterAlg m f g
  -> Fix f
  -> m (Fix (g f))
annotateWithLayer alg = (>>= alg) . traverse (annotateWithLayer alg) . unFix

type DeAnotater f g
  = Fix (g f) -> f (Fix (g f))

deAnnotate
  :: (Functor f)
  => DeAnotater f g
  -> Fix (g f)
  -> Fix f
deAnnotate alg = In . fmap (deAnnotate alg) . alg

type MonadicDeAnotater m f g
  = Fix (g f) -> m (f (Fix (g f)))

deAnnotateM
  :: (Traversable f, Monad m)
  => MonadicDeAnotater m f g
  -> Fix (g f)
  -> m (Fix f)
deAnnotateM alg = fmap In . (>>= traverse (deAnnotateM alg)) . alg

data CoAttr f a
  = Automatic a
  | Manual (f (CoAttr f a))

type CVCoAlgebra f a = a -> f (CoAttr f a)

futu :: forall f a . Functor f => CVCoAlgebra f a -> a -> Fix f
futu f = In . fmap worker . f
  where
    worker :: CoAttr f a -> Fix f
    worker (Automatic a) = futu f a        -- continue through this level
    worker (Manual g) = In (fmap worker g) -- omit folding this level,
                                           -- delegating to the worker
                                           -- to perform any needed
                                           -- unfolds later on.
