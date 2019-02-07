module Deref where

--------------------------------------------
import           Control.Monad.Free (Free(..))
import           Util.MyCompose
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Store.Capability
--------------------------------------------


-- | Greedily deref a merkle tree
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
strictDeref
  :: forall m
   . Monad m
  => Store m
  -> Pointer
  -> m $ Fix $ WithHash :+ Named :+ Tree
strictDeref store = cata alg . lazyDeref store
  where
    alg :: Traversable f
        => Algebra (WithHash :+ m :+ f)
                   (m $ Fix (WithHash :+ f))
    alg (C (p, C e)) = e >>= traverse id >>= pure . Fix . C . (p,)


-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall m
   . Monad m
  => Store m
  -> Pointer
  -> Fix $ WithHash :+ m :+ Named :+ Tree
lazyDeref store = futu alg
  where
    alg :: CVCoAlgebra (WithHash :+ m :+ Named :+ Tree)
                       (Pointer)
    alg p = C (p, C $ handleCMTL <$> sDeref store p)

    handleCMTL (C (name, e))
      = C . (name,) $ fmap (handleMTL) e

    handleMTL (Fix (C (p, C (Just e)))) = Free $ C (p, C . pure $ handleCMTL e)
    handleMTL (Fix (C (p, C Nothing))) = Pure p

