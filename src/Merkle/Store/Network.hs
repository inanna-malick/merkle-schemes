{-# LANGUAGE IncoherentInstances #-}

module Merkle.Store.Network where

--------------------------------------------
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Orphans ()
import           Data.Proxy
import           Servant
import           Servant.Client
--------------------------------------------
import           Merkle.Store
import           Merkle.Types (Hash)
--------------------------------------------

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

server :: forall f. Store IO f -> Server (StoreAPI f)
server s = derefGet :<|> uploadPost
  where
    derefGet = liftIO . sDeref s
    uploadPost = liftIO . sUploadShallow s

app :: forall f
     . (Functor f, FromJSON1 f, ToJSON1 f)
    => Store IO f
    -> Application
app = serve (Proxy :: Proxy (StoreAPI f)) . server
