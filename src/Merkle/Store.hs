module Merkle.Store where

--------------------------------------------
import           Data.Functor.Compose
import           Data.Kind (Type)
--------------------------------------------
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
