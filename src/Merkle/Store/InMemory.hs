module Merkle.Store.InMemory where

--------------------------------------------
import           Control.Monad.IO.Class
import           Data.Functor.Compose
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
--------------------------------------------
import           Util.RecursionSchemes
import           Merkle.Store
import           Merkle.Types
--------------------------------------------

inMemoryStore
  :: forall m f
   . ( Hashable f
     , Functor f
     , MonadIO m
     )
  => IORef (SSMap f) -> Store m f
inMemoryStore ior = Store
  { sDeref = \p -> liftIO $ lookup' p <$> readIORef ior
  , sUploadShallow = \x -> do
          let p = hash x
          liftIO $ modifyIORef ior (M.insert p x)
          pure p
  }
  where
    lookup' :: Hash f -> SSMap f -> Maybe (DerefRes f)
    lookup' p h =
      let lr = M.lookup p h
       in fmap (fmap (Fix . Compose . (, Compose $ Nothing))) lr


type SSMap f = Map (Hash f) (f (Hash f))
