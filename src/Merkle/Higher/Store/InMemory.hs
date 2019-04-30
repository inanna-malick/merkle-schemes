module Merkle.Higher.Store.InMemory where

--------------------------------------------
import           Control.Applicative (Const(..))
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Singletons
--------------------------------------------
import           Merkle.Higher.Store
import           Merkle.Higher.Types
--------------------------------------------

inMemoryStore
  :: forall m f
   . MonadIO m
  => (forall i. SingI i => IORef (SSMap f i))
  -> Store m f
  -> Store m f
inMemoryStore iorSelector fallback = Store
  { sGet = \p -> liftIO $ lookup' p <$> readIORef iorSelector
  , sPut = \x -> do
          -- upload to fallback
          h <- sPut fallback x
          -- cache locally
          liftIO $ modifyIORef iorSelector (M.insert h x)
          pure h
  }
  where
    lookup' :: forall i. Const (IPFSHash f) i -> SSMap f i -> Maybe (MerkleLayer f i)
    lookup' p h = M.lookup p h


type SSMap f i = Map (Const (IPFSHash f) i) (MerkleLayer f i)
