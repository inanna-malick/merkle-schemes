{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Merkle.Higher.Store.IPFS where


-- --------------------------------------------
import           Control.Exception.Safe
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Singletons
import           Network.Wreq
-- --------------------------------------------
import           Merkle.Higher.Store
import           Merkle.Higher.Types
-- --------------------------------------------

-- NOTE: all this could pretty much just run from javascript, too, given a URL + PORT
--       basically just data structures, json encodings, and hitting API's with requests!
-- TODO: ghcjs

ipfsStore
  :: forall f
  . ( forall i. FromJSON (MerkleLayer f i)
    , forall i. ToJSON   (MerkleLayer f i)
    )
  => IPFSNode
  -> Store IO f
ipfsStore node = Store (fmap Just . getForHash node) (putForHash node)

data IPFSNode
  = IPFSNode
  { host :: String
  , port :: Int
  }

localHost :: IPFSNode
localHost = IPFSNode "localhost" 5001

getForHash
  :: (SingI i, FromJSON (MerkleLayer f i))
  => IPFSNode
  -> Hash f i
  -> IO (MerkleLayer f i)
getForHash (IPFSNode host' port') (Const (IPFSHash h)) = do
    resp <- getWith opts path
    -- response type will not be json, to ipfs this is just a blob
    case eitherDecode' (resp ^. responseBody) of
      Left err  -> throwM (JSONError err)
      Right val -> pure val

  where
    opts = defaults & param "arg" .~ [h]
    path = "http://" ++ host' ++ ":" ++ show port' ++ "/api/v0/block/get"


putForHash
  :: (SingI i, ToJSON (MerkleLayer f i))
  => IPFSNode
  -> MerkleLayer f i
  -> IO (Hash f i)
putForHash (IPFSNode host' port') fhi = do
    resp <- post path (partLBS "data" (encode fhi))
    pure . Const . IPFSHash $ resp ^. responseBody . key "Key" . _String
  where
    path = "http://" ++ host' ++ ":" ++ show port' ++ "/api/v0/block/put"
