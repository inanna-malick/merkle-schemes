-- | in this module we commit the cardinal haskell sin...
--   ...defining our own type-level operators for convenience...
module Util.MyCompose where


import Util.HRecursionSchemes
-- taken from http://hackage.haskell.org/package/type-operators-0.1.0.4/docs/src/Control-Type-Operator.html#%24
type f $ a = f a
infixr 2 $


-- stolen from compdata w/ missing instances added 420 yolo

data (:++) f g e t = HC { getHC :: (f (g e) t) }
infixr 7 :++

-- getHC :: (f :++ g) e t -> f (g e) t
-- getHC (HC x) = x

instance (HFunctor f, HFunctor g) => HFunctor ((:++) f g) where
  hfmap f (HC x) = HC $ hfmap (hfmap f) x

-- incomplete instance, yolo, etc
instance (HTraversable f, HTraversable g) => HTraversable (f :++ g) where
    hmapM nat (HC x) = HC <$> hmapM (hmapM nat) x


annotate
  :: forall f x
   . HFunctor f
  => Alg f x
  -> Term f :-> Term (Tagged x :++ f)
annotate alg = cata alg'
  where
    alg' :: Alg f (Term (Tagged x :++ f))
    alg' f = Term . HC $ Tagged (alg $ hfmap (_tag . getHC . unTerm) f) f

(<:>) :: (HFunctor f, HFunctor g)
      => (f b :-> c)
      -> (g a :-> b)
      -> ((f :++ g) a :-> c)
(<:>) f g = f . hfmap g . getHC


(<!>) :: (HFunctor f, HFunctor g)
      => (a :-> f b)
      -> (b :-> g c)
      -> (a :-> (f :++ g) c)
(<!>) f g a =  HC . hfmap g $ f a
