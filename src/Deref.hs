module Deref where

--------------------------------------------
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types
import           Store
--------------------------------------------
import           Util.MyCompose
--------------------------------------------


-- | Greedily deref a merkle tree
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
strictDeref
  :: forall m
   . Monad m
  => Store m
  -> Pointer
  -> m $ Fix (WithHash :+ NamedTreeLayer)
strictDeref store = cata alg . lazyDeref store
  where
    alg :: Algebra (WithHash :+ m :+ NamedTreeLayer)
                   (m $ Fix (WithHash :+ NamedTreeLayer))
    alg (C (p, C e)) = e >>= traverse id >>= pure . Fix . C . (p,)


-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall m
   . Monad m
  => Store m
  -> Pointer
  -> Fix (WithHash :+ m :+ NamedTreeLayer)
lazyDeref store = futu alg
  where
    alg :: CVCoAlgebra (WithHash :+ m :+ NamedTreeLayer)
                       (Pointer)
    alg p = C (p, C $ handleCMTL <$> deref store p)

    handleCMTL (C (NamedEntity name e))
      = C . NamedEntity name $ fmap (handleMTL) e

    handleMTL (Fix (C (Direct p e))) = Manual $ C (p, C . pure $ handleCMTL e)
    handleMTL (Fix (C (Indirect p))) = Automatic p

-- todo: rename?
haesfPointer :: forall f . Fix (WithHash :+ f) -> Pointer
haesfPointer = fst . getCompose . unFix

-- compelling argument for type aliases right here
haesfDeref :: Fix (WithHash :+ m :+ NamedTreeLayer)
            -> m $ NamedTreeLayer (Fix (WithHash :+ m :+ NamedTreeLayer))
haesfDeref   = getCompose . snd . getCompose . unFix

type WithHash = (,) Pointer
