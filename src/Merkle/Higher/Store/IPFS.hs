{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Merkle.Higher.Store.IPFS where

-- --------------------------------------------
import           Control.Exception.Safe
import           Control.Lens
import           Data.Aeson.Lens
import           Data.ByteString.Lazy.Internal (ByteString)
import           Data.Singletons
import           Network.Wreq
-- --------------------------------------------
import           Merkle.Higher.Store
import           Merkle.Higher.Types
-- --------------------------------------------


ipfsStore
  :: forall f
  -- HAX TO AVOID QuantifiedConstraint (no ghcjs support)
   . (forall i. SingI i => ByteString -> String `Either` MerkleLayer f i)
  -> (forall i. SingI i => MerkleLayer f i -> ByteString)
  -> IPFSNode
  -> Store IO f
ipfsStore decoder encoder node
  = Store (fmap Just . getForHash decoder node)
          (putForHash encoder node)

data IPFSNode
  = IPFSNode
  { host :: String
  , port :: Int
  }

localHost :: IPFSNode
localHost = IPFSNode "localhost" 5001

getForHash
  :: SingI i
  => (forall i'. SingI i' => ByteString -> String `Either` MerkleLayer f i')
  -> IPFSNode
  -> Hash f i
  -> IO (MerkleLayer f i)
getForHash decoder (IPFSNode host' port') (Const (IPFSHash h)) = do
    resp <- getWith opts path
    -- response type will not be json, to ipfs this is just a blob
    case decoder (resp ^. responseBody) of
      Left err  -> throwM (JSONError err)
      Right val -> pure val

  where
    opts = defaults & param "arg" .~ [h]
    path = "http://" ++ host' ++ ":" ++ show port' ++ "/api/v0/block/get"


putForHash
  :: SingI i
  => (forall i'. SingI i' => MerkleLayer f i' -> ByteString)
  -> IPFSNode
  -> MerkleLayer f i
  -> IO (Hash f i)
putForHash encoder (IPFSNode host' port') fhi = do
    resp <- post path (partLBS "data" (encoder fhi))
    pure . Const . IPFSHash $ resp ^. responseBody . key "Key" . _String
  where
    path = "http://" ++ host' ++ ":" ++ show port' ++ "/api/v0/block/put"
