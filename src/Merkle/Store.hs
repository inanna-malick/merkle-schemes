module Merkle.Store where

--------------------------------------------
import           Control.Applicative (Const(..))
import           Data.Functor.Compose
import           Data.Kind (Type)
--------------------------------------------
import           Merkle.Functors (HashAnnotated)
import           Merkle.Types (Hash, RawHash)
import           Util.RecursionSchemes
--------------------------------------------

type DerefRes f = f (Fix (HashAnnotated f `Compose` Maybe `Compose` f))

data Store m (f :: Type -> Type)
  = Store
  { sDeref :: Hash f -> m (Maybe (DerefRes f))
  , sUploadShallow :: f (Hash f) -> m (Hash f)
  }

data ShallowStore m (f :: Type -> Type)
  = ShallowStore
  { ssDeref :: Hash f -> m (Maybe (f (Hash f)))
  , ssUploadShallow :: f (Hash f) -> m (Hash f)
  }

liftShallowStore :: forall m f. Monad m => Functor f => ShallowStore m f -> Store m f
liftShallowStore (ShallowStore d u) = Store d' u
  where
    d' :: Hash f -> m (Maybe (DerefRes f))
    d' h =
        d h >>= \case
          Nothing -> pure Nothing
          Just x -> pure . Just $ fmap (\p' -> Fix $ Compose (p', Compose Nothing)) x

data RawShallowStore m (f :: Type -> Type)
  = RawShallowStore
  { rssDeref :: RawHash -> m (Maybe (f RawHash))
  , rssUploadShallow :: f RawHash -> m RawHash
  }

liftRawStore :: forall m f. Monad m => Functor f => RawShallowStore m f -> ShallowStore m f
liftRawStore (RawShallowStore d u) = ShallowStore d' u'
  where
    u' f =
      u (fmap getConst f) >>= pure . Const
    d' (Const h) =
        d h >>= \case
          Nothing -> pure Nothing
          Just x -> pure . Just $ fmap Const x


-- technically store is now a semigroup
-- TODO: write fallback vals to original cache! (or don't, it makes laziness more observable..)
withFallback :: Monad m => Store m f -> Store m f -> Store m f
withFallback main fallback = Store deref' (sUploadShallow main)
  where
    deref' h = do
      sDeref main h >>= \case
        Just mainRes -> pure $ Just mainRes
        Nothing -> sDeref fallback h

uploadDeep
  :: forall m f
   . Traversable f
  => Monad m
  => Store m f
  -> (Fix f)
  -> m (Hash f)
uploadDeep store = cataM (sUploadShallow store)

liftStore :: (forall x. m x -> m' x) -> Store m f -> Store m' f
liftStore f (Store d u) = Store (f . d) (f . u)
