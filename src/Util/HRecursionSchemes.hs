{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A BUNCH OF SHIT STOLEN FROM COMPDATA + POLYKINDS/SING stuff from me, TODO UPSTREAM PR
module Util.HRecursionSchemes where

import           Data.Singletons
import           Data.Functor.Compose
import           Data.Kind (Type)

type NatM m f g = forall i. SingI i => f i -> m (g i)

type f :-> g = forall (i :: k) . SingI i => f i -> g i
type f :=> a = forall (i :: k) . SingI i => f i -> a

class HFunctor (h :: (k -> Type) -> k -> Type) where
    hfmap :: (f :-> g) -> h f :-> h g

newtype K a h i = K { getK :: a}
  deriving (Eq, Ord, Show)

instance (Functor f) => HFunctor (Compose f) where
  hfmap f (Compose xs) = Compose (fmap f xs)

-- note: just the bit I need for cataM/anaM
class HTraversable t where
    hmapM :: (Monad m) => NatM m f g -> NatM m (t f) (t g)

instance (Traversable f) => HTraversable (Compose f) where
  hmapM nat (Compose xs) = Compose <$> traverse nat xs

data Cxt h f a i where
    Term ::  f (Cxt h f a) i -> Cxt h f a i
    Hole :: a i -> Cxt Hole f a i

unCxt :: f (Cxt h f a) :-> x
      -> a :-> x
      -> Cxt h f a :-> x
unCxt f _ (Term x) = f x
unCxt _ f (Hole x) = f x

data Hole
data NoHole

type Context = Cxt Hole

type Term f = Cxt NoHole f (K () ())

unTerm :: Term f t -> f (Term f) t
unTerm (Term t) = t

instance (HFunctor f) => HFunctor (Cxt h f) where
    hfmap f (Hole x) = Hole (f x)
    hfmap f (Term t) = Term (hfmap (hfmap f) t)

type Alg f e = f e :-> e

cata :: forall f a. HFunctor f => Alg f a -> Term f :-> a
cata f = f . hfmap (cata f) . unTerm

type AlgM m f e = NatM m (f e) e

-- | This is a monadic version of 'cata'.
cataM
  :: forall f m a
   . (HTraversable f, Monad m)
  => AlgM m f a
  -> NatM m (Term f) a
cataM f = (>>= f) . hmapM (cataM f) . unTerm

type Coalg f a = a :-> f a

ana :: forall f a. HFunctor f => Coalg f a -> (a :-> Term f)
ana f = Term . hfmap (ana f) . f

type CoalgM m f a = NatM m a (f a)

anaM
  :: forall a m f
   . (HTraversable f, Monad m)
  => CoalgM m f a
  -> NatM m a (Term f)
anaM f = fmap Term . (>>= hmapM (anaM f)) . f

type CVCoalg f a = a :-> f (Context f a)

futu :: forall f a . HFunctor f => CVCoalg f a -> a :-> Term f
futu coa = ana (unCxt id coa) . Hole

-- | ETC:

data Tagged f g i = Tagged { _tag :: (f i), _elem :: (g i) }

instance HFunctor (Tagged x) where
    hfmap f (Tagged x y) = Tagged x (f y)

instance HTraversable (Tagged x) where
  hmapM nat (Tagged x y) = Tagged x <$> nat y


data HCompose f g e t = HC { getHC :: (f (g e) t) }

-- getHC :: (f `HCompose` g) e t -> f (g e) t
-- getHC (HC x) = x

instance (HFunctor f, HFunctor g) => HFunctor (f `HCompose` g) where
  hfmap f (HC x) = HC $ hfmap (hfmap f) x

-- incomplete instance, yolo, etc
instance (HTraversable f, HTraversable g) => HTraversable (f `HCompose` g) where
    hmapM nat (HC x) = HC <$> hmapM (hmapM nat) x

annotate
  :: forall f x
   . HFunctor f
  => Alg f x
  -> Term f :-> Term (Tagged x `HCompose` f)
annotate alg = cata alg'
  where
    alg' :: Alg f (Term (Tagged x `HCompose` f))
    alg' f = Term . HC $ Tagged (alg $ hfmap (_tag . getHC . unTerm) f) f
