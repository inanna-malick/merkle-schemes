module Merkle.Store where

--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throw)
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Kind (Type)
--------------------------------------------
import           Errors
import           Merkle.Functors (HashAnnotated)
import           Merkle.Types (Hash)
import           Util.RecursionSchemes
--------------------------------------------

type DerefRes f = f (Fix (HashAnnotated f `Compose` Maybe `Compose` f))

data Store m (f :: Type -> Type)
  = Store
  { sDeref :: Hash f -> m (Maybe (DerefRes f))
  , sUploadShallow :: f (Hash f) -> m (Hash f)
  }

sDeref' :: MonadThrow m => Store m f -> Hash f -> m (DerefRes f)
sDeref' s h = sDeref s h >>= maybe (throw . LookupError $ getConst h) pure


-- technically store is now a semigroup
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

{-
-- IDEA: distinct shallow store type (for single-layer deref caps, can be lifted)
data Store m (f :: Type -> Type)
  = Store
  { sDeref :: Hash f -> m $ f $ Fix (HashAnnotated f `Compose` Maybe `Compose` f)
  , sUpload :: Fix f -> m (Hash f)
  }

data ShallowStore m (f :: Type -> Type)
  = Store
  { sDerefShallow :: Hash f -> m $ f $ Fix (HashAnnotated f `Compose` Maybe `Compose` f)
  , sUploadShallow :: AlgebraM m f (Hash f)
  }
-}
