{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Merkle.Higher.IPFS where


-- --------------------------------------------
import           Control.Exception.Safe
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Text (pack, unpack)
import           Data.Singletons
import           Network.Wreq
-- --------------------------------------------
import           Merkle.Higher.Types
-- --------------------------------------------

-- NOTE: all this could pretty much just run from javascript, too, given a URL + PORT
--       basically just data structures, json encodings, and hitting API's with requests!

data IPFSNode
  = IPFSNode
  { host :: String
  , port :: Int
  }

localHost :: IPFSNode
localHost = IPFSNode "localhost" 5001

getForHash :: SingI i => FromJSON (f Hash i) => IPFSNode -> Hash i -> IO (f Hash i)
getForHash (IPFSNode host' port') (Const (IPFSHash h)) = do
    resp <- getWith opts path
    -- response type will not be json, to ipfs this is just a blob
    case eitherDecode' (resp ^. responseBody) of
      Left err  -> throwM (JSONError err)
      Right val -> pure val

  where
    opts = defaults & param "arg" .~ [pack h]
    path = "http://" ++ host' ++ ":" ++ show port' ++ "/api/v0/block/get"


putForHash :: SingI i => ToJSON (f Hash i) => IPFSNode -> f Hash i -> IO (Hash i)
putForHash (IPFSNode host' port') fhi = do
    resp <- post path (partLBS "data" (encode fhi))
    pure . Const . IPFSHash . unpack $ resp ^. responseBody . key "Key" . _String
  where
    path = "http://" ++ host' ++ ":" ++ show port' ++ "/api/v0/block/put"
