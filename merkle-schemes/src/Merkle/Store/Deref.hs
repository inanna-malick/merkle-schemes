module Merkle.Store.Deref where

--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throwString)
import           Control.Monad.Trans.Maybe
import           Data.Functor.Compose
--------------------------------------------
import           Util.RecursionSchemes
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Types
--------------------------------------------


-- TODO: move?
strictDeref
  :: Traversable f
  => Monad m
  => Fix (HashAnnotated h f `Compose` m `Compose` f)
  -> m (Fix (HashAnnotated h f `Compose` f))
strictDeref = anaM alg
  where
    alg (Fix (Compose (p, Compose eff))) = do
      x <- eff
      pure $ Compose (p, x)


lazyDeref'
  :: forall m h f
   . ( Monad m
     , MonadThrow m
     , Functor f
     , Show h
     )
  => GetCapability m h f
  -> Hash h f
  -> Fix (HashAnnotated h f `Compose` m `Compose` f)
lazyDeref' store = cata alg . lazyDeref store
  where
    alg (Compose (h, Compose eff)) = Fix . Compose . (h,) . Compose $ do
      res <- runMaybeT eff
      maybe (throwString $ "lookup error: " ++ show h) pure res

-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall m h f
   . Monad m
  => Functor f
  => GetCapability m h f
  -> Hash h f
  -> Fix (HashAnnotated h f `Compose` (MaybeT m) `Compose` f)
lazyDeref (GetCapability get) = futu alg
  where
    alg :: CVCoAlgebra (HashAnnotated h f `Compose` (MaybeT m) `Compose` f) (Hash h f)
    alg p = Compose . (p,) . Compose $ fmap (cata helper) <$> MaybeT (get p)

    helper :: Algebra (HashAnnotated h f `Compose` Maybe `Compose` f)
                      (Free (HashAnnotated h f `Compose` (MaybeT m) `Compose` f) (Hash h f))
    helper (Compose (p, (Compose Nothing))) = Pure p
    helper (Compose (p, (Compose(Just x))))
      = Free . Compose $ (p, (Compose $ pure $ x))


sDeref'
  :: (MonadThrow m, Show h)
  => GetCapability m h f
  -> Hash h f
  -> m (DerefRes h f)
sDeref' (GetCapability get) h = get h >>= maybe (throwString $ "lookup error: " ++ show h) pure
