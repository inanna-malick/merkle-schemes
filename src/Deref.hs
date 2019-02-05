module Deref where

--------------------------------------------
import           Util.RecursionSchemes
import           Merkle.Tree.Types
import           Merkle.Types
import           Store
--------------------------------------------
import           Util.MyCompose
--------------------------------------------

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
    alg (C (p, C e)) =
      do
        e' <- e
        e'' <- traverse id e'
        pure $ In $ C (p, e'')


-- construct a potentially-infinite tree-shaped stream of further values constructed by
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

    handleMTL (In (C (Direct p e))) = Manual $ C (p, C . pure $ handleCMTL e)
    handleMTL (In (C (Indirect p))) = Automatic p

unexpanded
  :: Pointer
  -> Fix (WithHash :+ Maybe :+ NamedTreeLayer)
unexpanded p = In $ C (p, C Nothing)

expanded
  :: NamedTreeLayer $ Fix (WithHash :+ Maybe :+ NamedTreeLayer)
  -> Pointer
  -> Fix (WithHash :+ Maybe :+ NamedTreeLayer)
expanded x p = In $ C (p, C $ Just x)

-- todo better name?
expandedShallow
  :: forall g
   . NamedTreeLayer $ (Fix (WithHash :+ g))
  -> Pointer
  -> Fix (WithHash :+ Maybe :+ NamedTreeLayer)
expandedShallow x = expanded (fmap (\(In (C (p,_))) -> In $ C (p, C Nothing)) x)

-- todo: rename?
haesfPointer :: forall f . Fix (WithHash :+ f) -> Pointer
haesfPointer = fst . getCompose . unFix

-- compelling argument for type aliases right here
haesfDeref :: Fix (WithHash :+ m :+ NamedTreeLayer)
            -> m $ NamedTreeLayer (Fix (WithHash :+ m :+ NamedTreeLayer))
haesfDeref   = getCompose . snd . getCompose . unFix

type WithHash = (,) Pointer
