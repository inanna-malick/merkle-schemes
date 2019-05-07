{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Merkle.Higher.Store.IPFS where

-- --------------------------------------------
import           Control.Exception.Safe
import           Control.Lens ((^.), (&), (.~))
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Singletons
import           Network.Wreq
-- --------------------------------------------
import           Merkle.Higher.Store
import           Merkle.Higher.Types
-- --------------------------------------------


ipfsStore
  :: forall f
   . ( forall i. SingI i => FromJSON (f Hash i)
     , forall i. SingI i => ToJSON   (f Hash i)
     , ExtractKeys f -- for linking obj graph together (weak foldable, kinda)
     )
  => IPFSNode
  -> Store IO f
ipfsStore node
  = Store (fmap Just . getForHash node)
          (putForHash node)

data IPFSNode
  = IPFSNode
  { host :: String
  , port :: Int
  }

localHost :: IPFSNode
localHost = IPFSNode "localhost" 5001

getForHash
  :: ( SingI i
     , forall i'. SingI i' => FromJSON (f Hash i')
     )
  => IPFSNode
  -> Hash i
  -> IO (f Hash i)
getForHash (IPFSNode host' port') (Hash h) = do
    resp <- getWith opts path
    -- response type will not be json, to ipfs this is just a blob
    case eitherDecode (resp ^. responseBody) of
      Left err  -> throwM (JSONError err)
      Right (DagNode val _ls) -> pure val

  where
    opts = defaults & param "arg" .~ [h]
    path = "http://" ++ host' ++ ":" ++ show port' ++ "/api/v0/object/get"


putForHash
  :: forall i f
   . ( SingI i
     , forall i'. SingI i' => ToJSON (f Hash i')
     , ExtractKeys f
     )
  => IPFSNode
  -> f Hash i
  -> IO (Hash i)
putForHash (IPFSNode host' port') fhi = do
    let obj = DagNode fhi $ extractHashKeys fhi
    resp <- post path (partLBS "data" $ encode obj)
    pure . Hash $ resp ^. responseBody . key "Hash" . _String
  where
    path = "http://" ++ host' ++ ":" ++ show port' ++ "/api/v0/object/put?datafieldenc=base64"

