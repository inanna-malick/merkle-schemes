{-# LANGUAGE IncoherentInstances #-}

module Merkle.Store.Network where

--------------------------------------------
import           Control.Monad.IO.Class
import           Data.Proxy
import           Servant
import           Servant.Client
--------------------------------------------
import           Merkle.Store
import           Merkle.Types (Hash)
--------------------------------------------
import Data.Aeson.Orphans ()
import Data.Aeson

type DerefAPI f = "deref"   :> Capture "hash" (Hash f) :> Get '[JSON] (Maybe (DerefRes f))
type UploadAPI f = "upload" :> ReqBody '[JSON] (f (Hash f)) :> Post '[JSON] (Hash f)
type StoreAPI f = DerefAPI f :<|> UploadAPI f



-- TODO: use to implement hash-based sharding
-- should only be a few lines, just make a node like this
-- | Filesystem backed store using the provided dir
netStore
  :: forall f
   . ( Traversable f
     , Functor f
     , ToJSON1 f
     , FromJSON1 f
     )
  => Store ClientM f
netStore
  = Store
  { sDeref = derefGet
  , sUploadShallow = uploadPost
  }
  where
    derefGet :<|> uploadPost = client (Proxy :: Proxy (StoreAPI f))

server :: forall f. String -> Store IO f -> Server (StoreAPI f)
server typ s = derefGet :<|> uploadPost
  where
    derefGet x = do
      liftIO $ putStrLn $ "deref(" ++ typ ++ "): " ++ show x
      liftIO $ sDeref s x
    uploadPost x = do
      r <- liftIO $ sUploadShallow s x
      liftIO $ putStrLn $ "upload(" ++ typ ++ "), hash: " ++ show r
      pure r

app :: forall f . Functor f => FromJSON1 f => ToJSON1 f
    => String -> Store IO f -> Application
app typ s = serve (Proxy :: Proxy (StoreAPI f)) (server typ s)
