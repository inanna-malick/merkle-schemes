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
  -> m $ Term (WithHash :+ NamedTreeLayer)
strictDeref store = cata alg . lazyDeref store
  where
    alg :: Algebra (WithHash :+ m :+ NamedTreeLayer)
                   (m $ Term (WithHash :+ NamedTreeLayer))
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
  -> Term (WithHash :+ m :+ NamedTreeLayer)
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
  -> Term (WithHash :+ Maybe :+ NamedTreeLayer)
unexpanded p = In $ C (p, C Nothing)

expanded
  :: NamedTreeLayer $ Term (WithHash :+ Maybe :+ NamedTreeLayer)
  -> Pointer
  -> Term (WithHash :+ Maybe :+ NamedTreeLayer)
expanded x p = In $ C (p, C $ Just x)

-- todo better name?
expandedShallow
  :: forall g
   . NamedTreeLayer $ (Term (WithHash :+ g))
  -> Pointer
  -> Term (WithHash :+ Maybe :+ NamedTreeLayer)
expandedShallow x = expanded (fmap (\(In (C (p,_))) -> In $ C (p, C Nothing)) x)

-- todo: rename?
haesfPointer :: forall f . Term (WithHash :+ f) -> Pointer
haesfPointer = fst . getCompose . out

-- compelling argument for type aliases right here
haesfDeref :: Term (WithHash :+ m :+ NamedTreeLayer)
            -> m $ NamedTreeLayer (Term (WithHash :+ m :+ NamedTreeLayer))
haesfDeref   = getCompose . snd . getCompose . out

type WithHash = (,) Pointer
