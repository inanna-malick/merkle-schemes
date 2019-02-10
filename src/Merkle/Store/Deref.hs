module Merkle.Store.Deref where

--------------------------------------------
import           Control.Monad.Free (Free(..))
import           Util.MyCompose
import           Util.RecursionSchemes
import           Merkle.Types
import           Merkle.Store
--------------------------------------------


-- | Greedily deref a merkle tree
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
strictDeref
  :: forall m x
   . Monad m
  => Traversable x
  => Store m x
  -> Pointer
  -> m $ Fix $ WithHash :+ x
strictDeref store = cata alg . lazyDeref store
  where
    alg :: Algebra (WithHash :+ m :+ x)
                   (m $ Fix (WithHash :+ x))
    alg (C (p, C e)) = e >>= traverse id >>= pure . Fix . C . (p,)


-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall m x
   . Monad m
  => Functor x
  => Store m x
  -> Pointer
  -> Fix $ WithHash :+ m :+ x
lazyDeref store = futu alg
  where
    alg :: CVCoAlgebra (WithHash :+ m :+ x)
                       (Pointer)
    alg p = C (p, C $ handleCMTL <$> sDeref store p)

    handleCMTL x
      = fmap (handleMTL) x

    handleMTL (Fix (C (p, C (Just e)))) = Free $ C (p, C . pure $ handleCMTL e)
    handleMTL (Fix (C (p, C Nothing))) = Pure p

