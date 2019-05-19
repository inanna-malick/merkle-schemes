module Merkle.Store.IPFS where

-- --------------------------------------------
import           Control.Applicative (Const(..))
import           Control.Exception.Safe
import           Control.Lens ((^.), (&), (.~))
import           Data.Aeson
import           Data.Aeson.Lens
import           Network.Wreq
-- --------------------------------------------
import           Merkle.Store
import           Merkle.Types
import           Merkle.Types.IPFS
-----------------------------------------------


ipfsStore
  :: ( FromJSON (f (Hash RawIPFSHash f))
     , ToJSON   (f (Hash RawIPFSHash f))
     , ExtractKeys f -- for linking obj graph together (weak foldable, kinda)
     )
  => IPFSNode
  -> ShallowStore IO RawIPFSHash f
ipfsStore node
  = ( GetCapabilityShallow $ fmap Just . getForHash node
    , PutCapability $ putForHash node
    )

data IPFSNode
  = IPFSNode
  { host :: String
  , port :: Int
  }

localHost :: IPFSNode
localHost = IPFSNode "http://localhost" 5001

getForHash
  :: FromJSON (f (Hash RawIPFSHash f))
  => IPFSNode
  -> Hash RawIPFSHash f
  -> IO (f (Hash RawIPFSHash f))
getForHash (IPFSNode host' port') (Const h) = do
    putStrLn $ "get: " ++ show h
    print path
    resp <- getWith opts path
    print resp
    case eitherDecode (resp ^. responseBody) of
      Left err  -> throwM (JSONError err)
      Right (DagNode val _ls) -> pure val

  where
    opts = defaults & param "arg" .~ [unRawIPFSHash h]
    path = host' ++ ":" ++ show port' ++ "/api/v0/object/get?datafieldenc=base64"


putForHash
  :: ( ToJSON (f (Hash RawIPFSHash f))
     , ExtractKeys f
     )
  => IPFSNode
  -> f (Hash RawIPFSHash f)
  -> IO (Hash RawIPFSHash f)
putForHash (IPFSNode host' port') fhi = do
    let obj = DagNode fhi $ extractRawKeys fhi
    putStrLn "put"
    print $ encode obj
    resp <- post path (partLBS "data" $ encode obj)
    print resp
    pure . Const . RawIPFSHash $ resp ^. responseBody . key "Hash" . _String
  where
    path = host' ++ ":" ++ show port' ++ "/api/v0/object/put?datafieldenc=base64"

