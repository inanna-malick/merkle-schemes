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

type DerefAPI h f = "deref"   :> Capture "hash" (Hash h f) :> Get '[JSON] (Maybe (DerefRes h f))
type UploadAPI h f = "upload" :> ReqBody '[JSON] (f (Hash h f)) :> Post '[JSON] (Hash h f)
type StoreAPI h f = DerefAPI h f :<|> UploadAPI h f


-- TODO: use to implement hash-based sharding
-- should only be a few lines, just make a node like this
-- | Filesystem backed store using the provided dir
netStore
  :: forall h f
   . ( Traversable f
     , Functor f
     , ToJSON1 f
     , FromJSON1 f
     , ToHttpApiData (Hash h f)
     , FromJSON h
     , ToJSON h
     )
  => Store ClientM h f
netStore
  =
  ( GetCapability derefGet
  , PutCapability uploadPost
  )
  where
    derefGet :<|> uploadPost = client (Proxy :: Proxy (StoreAPI h f))

server :: forall h f. Store IO h f -> Server (StoreAPI h f)
server (GetCapability get, PutCapability put) = derefGet :<|> uploadPost
  where
    derefGet = liftIO . get
    uploadPost = liftIO . put

app :: forall h f
     . ( Functor f
       , FromJSON1 f
       , ToJSON1 f
       , FromHttpApiData (Hash h f)
       , FromJSON h
       , ToJSON h
       )
    => Store IO h f
    -> Application
app = serve (Proxy :: Proxy (StoreAPI h f)) . server
