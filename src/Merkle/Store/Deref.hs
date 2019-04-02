module Merkle.Store.Deref where

--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.Trans.Maybe
import           Data.Functor.Compose
import           Data.Functor.Const
--------------------------------------------
import           Errors
import           Util.RecursionSchemes
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Types
--------------------------------------------


-- TODO: move?
strictDeref
  :: Traversable f
  => Monad m
  => Fix (HashAnnotated f `Compose` m `Compose` f)
  -> m (Fix (HashAnnotated f `Compose` f))
strictDeref = anaM alg
  where
    alg (Fix (Compose (p, Compose eff))) = do
      x <- eff
      pure $ Compose (p, x)


lazyDeref'
  :: forall m f
   . Monad m
  => MonadThrow m
  => Functor f
  => Store m f
  -> Hash f
  -> Fix (HashAnnotated f `Compose` m `Compose` f)
lazyDeref' store = cata alg . lazyDeref store
  where
    alg (Compose (h, Compose eff)) = Fix . Compose . (h,) . Compose $ do
      res <- runMaybeT eff
      maybe (throw $ LookupError (getConst h)) pure res

-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall m f
   . Monad m
  => Functor f
  => Store m f
  -> Hash f
  -> Fix (HashAnnotated f `Compose` (MaybeT m) `Compose` f)
lazyDeref store = futu alg
  where
    alg :: CVCoAlgebra (HashAnnotated f `Compose` (MaybeT m) `Compose` f) (Hash f)
    alg p = Compose . (p,) . Compose $ fmap (cata helper) <$> MaybeT (sDeref store p)

    helper :: Algebra (HashAnnotated f `Compose` Maybe `Compose` f)
                      (Free (HashAnnotated f `Compose` (MaybeT m) `Compose` f) (Hash f))
    helper (Compose (p, (Compose Nothing))) = Pure p
    helper (Compose (p, (Compose(Just x))))
      = Free . Compose $ (p, (Compose $ pure $ x))
