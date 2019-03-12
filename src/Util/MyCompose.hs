-- | in this module we commit the cardinal haskell sin...
--   ...defining our own type-level operators for convenience...
module Util.MyCompose where


import qualified Util.HRecursionSchemes as C
-- taken from http://hackage.haskell.org/package/type-operators-0.1.0.4/docs/src/Control-Type-Operator.html#%24
type f $ a = f a
infixr 2 $


-- stolen from compdata w/ missing instances added 420 yolo

data (:++) f g e t = HC (f (g e) t)
infixr 7 :++

getHCompose :: (f :++ g) e t -> f (g e) t
getHCompose (HC x) = x

instance (C.HFunctor f, C.HFunctor g) => C.HFunctor ((:++) f g) where
  hfmap f (HC x) = HC $ C.hfmap (C.hfmap f) x

-- incomplete instance, yolo, etc
instance (C.HTraversable f, C.HTraversable g) => C.HTraversable (f :++ g) where
    hmapM nat (HC x) = HC <$> C.hmapM (C.hmapM nat) x

