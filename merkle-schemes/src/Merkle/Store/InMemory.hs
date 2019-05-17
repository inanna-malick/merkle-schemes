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
  :: forall m h f
   . ( Hashable h f
     , Functor f
     , Ord h
     , MonadIO m
     )
  => IORef (SSMap h f)
  -> Store m h f
inMemoryStore ior =
  ( GetCapability $ \p -> liftIO $ lookup' p <$> readIORef ior
  , PutCapability $ \x -> do
          let p = hash x
          liftIO $ modifyIORef ior (M.insert p x)
          pure p
  )
  where
    lookup' :: Hash h f -> SSMap h f -> Maybe (DerefRes h f)
    lookup' p h =
      let lr = M.lookup p h
       in fmap (fmap (Fix . Compose . (, Compose $ Nothing))) lr


type SSMap h f = Map (Hash h f) (f (Hash h f))
