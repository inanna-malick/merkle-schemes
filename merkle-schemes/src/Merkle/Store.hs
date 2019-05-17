module Merkle.Store where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Merkle.Functors (HashAnnotated)
import           Merkle.Types (Hash)
import           Util.RecursionSchemes
--------------------------------------------


type Store m h f = (GetCapability m h f, PutCapability m h f)

type ShallowStore m h f = (GetCapabilityShallow m h f, PutCapability m h f)

data GetCapability m h f
  = GetCapability
  { gcGet :: Hash h f -> m (Maybe (DerefRes h f))
  }
type DerefRes h f = f (Fix (HashAnnotated h f `Compose` Maybe `Compose` f))

data GetCapabilityShallow m h f
  = GetCapabilityShallow
  { gcGetShallow :: Hash h f -> m (Maybe (f (Hash h f)))
  }


liftShallowStore
  :: forall m h f
   . (Monad m, Functor f)
  => ShallowStore m h f
  -> Store m h f
liftShallowStore (g,p) = (liftShallowGetCap g, p)

liftShallowGetCap
  :: forall m h f
   . (Monad m, Functor f)
  => GetCapabilityShallow m h f
  -> GetCapability m h f
liftShallowGetCap (GetCapabilityShallow g) = GetCapability g'
  where
    g' :: Hash h f -> m (Maybe (DerefRes h f))
    g' h =
        g h >>= \case
          Nothing -> pure Nothing
          Just x -> pure . Just $ fmap (\p' -> Fix $ Compose (p', Compose Nothing)) x


data PutCapability m h f
  = PutCapability
  { gcPut :: f (Hash h f) -> m (Hash h f)
  }


-- technically store is now a semigroup
-- TODO: write fallback vals to original cache! (or don't, it makes laziness more observable..)
withFallback :: Monad m => GetCapability m h f -> GetCapability m h f -> GetCapability m h f
withFallback main fallback = GetCapability get
  where
    get h = do
      gcGet main h >>= \case
        Just mainRes -> pure $ Just mainRes
        Nothing -> gcGet fallback h

uploadDeep
  :: forall m h f
   . (Traversable f, Monad m)
  => PutCapability m h f
  -> (Fix f)
  -> m (Hash h f)
uploadDeep = cataM . gcPut

liftStore :: (forall x. m x -> m' x) -> Store m h f -> Store m' h f
liftStore f (g,p) = (liftGetCap f g, liftPutCap f p)

liftGetCap :: (forall x. m x -> m' x) -> GetCapability m h f -> GetCapability m' h f
liftGetCap f (GetCapability g) = GetCapability (f . g)

liftPutCap :: (forall x. m x -> m' x) -> PutCapability m h f -> PutCapability m' h f
liftPutCap f (PutCapability g) = PutCapability (f . g)
