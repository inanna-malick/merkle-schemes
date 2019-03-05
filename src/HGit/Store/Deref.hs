module HGit.Store.Deref where

--------------------------------------------
import qualified Data.Functor.Compose as FC
import           Data.Functor.Const
import           Data.Singletons
--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes
import           HGit.Store
import           HGit.Types
--------------------------------------------


-- | Greedily deref a merkle tree
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
strictDeref''
  :: forall i m p
   . HTraversable p
  => Monad m
  =>     Term (FC.Compose ((,) HashPointer :+ m) :++ p) i
  -> m $ Term p i
strictDeref'' = anaM alg
  where
    alg :: CoalgM m p (Term (FC.Compose ((,) a :+ m) :++ p))
    alg (Term (HC (FC.Compose (C (_, eff))))) = eff


-- | Greedily deref a merkle tree
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
strictDeref
  :: forall i m p
   . HTraversable p
  => Monad m
  =>     Term (FC.Compose ((,) HashPointer :+ m) :++ p) i
  -> m $ Term (FC.Compose ((,) HashPointer     ) :++ p) i
strictDeref = anaM alg
  where
    alg :: CoalgM m (FC.Compose ((,) a) :++ p) (Term (FC.Compose ((,) a :+ m) :++ p))
    alg (Term (HC (FC.Compose (C (p, e))))) = do
      e' <- e
      pure $ HC $ FC.Compose $ (p, e')


strictDeref'
  :: forall i m p
   . Monad m
  => SHFunctor p
  => HFunctor p
  => HTraversable p
  => SingI i
  => Store m p
  -> Const HashPointer i
  -> m $ Term (FC.Compose ((,) HashPointer     ) :++ p) i
strictDeref' store = strictDeref . lazyDeref store

-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
-- TODO: update dox for gadt way

lazyDeref
  :: forall i m p
   . Monad m
  => SHFunctor p
  => HFunctor p
  => SingI i
  => Store m p
  -> Const HashPointer i
  -> Term (FC.Compose (LazyHashTagged m) :++ p) i
lazyDeref store = sFutu alg
  where
    alg :: SCVCoalg
             (FC.Compose (LazyHashTagged m) :++ p)
             (Const HashPointer)
    alg p = HC $ FC.Compose $ C (getConst p, hfmap helper <$> sDeref store p)


    helper :: Term (FC.Compose HashIndirect :++ p)
                 :-> Context (FC.Compose (LazyHashTagged m) :++ p) (Const HashPointer)
    helper (Term (HC (FC.Compose (C (p, Nothing))))) = Hole $ Const p
    helper (Term (HC (FC.Compose (C (p, Just x))))) =
      Term $ HC (FC.Compose (C (p, pure $ hfmap helper x)))
