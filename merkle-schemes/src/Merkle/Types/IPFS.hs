module Merkle.Types.IPFS where

--------------------------------------------
import           Data.Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import           Data.Text
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           GHC.Generics (Generic)
--------------------------------------------
import           Merkle.Types (Hash)
--------------------------------------------

-- IPFS: string, compatible, 58 bit encoding - using string instead of bytestring for simplicity
newtype RawIPFSHash
  = RawIPFSHash { unRawIPFSHash :: Text } deriving (Eq, Ord, Show, Generic)

instance ToJSON RawIPFSHash where
  toJSON = String . unRawIPFSHash

instance FromJSON RawIPFSHash where
  parseJSON =
    withText "RawRawIPFSHash" (pure . RawIPFSHash)

-- | For use with IPFS links (so only top-level element need be pinned)
-- should return all hashes of any type (eg bifunctor case where first
-- type parameter is also a hash)
class ExtractKeys f where
  extractRawKeys :: f (Hash RawIPFSHash f) -> [RawIPFSHash]

data DagNode a
  = DagNode
  { dnValue :: a
  , dnLinks :: [RawIPFSHash] -- raw hashes only
  }

instance FromJSON x => FromJSON (DagNode x) where
    parseJSON = withObject "dag node" $ \o -> do
              d <- o .: "Data"
              ls <- o .: "Links"
              ls' <- traverse (withObject "dag link" $ \o' -> o' .: "RawIPFSHash") ls
              case Base64.decode (encodeUtf8 d) >>= eitherDecode . LB.fromStrict of
                Left err -> fail err
                Right x  -> pure $ DagNode x ls'

instance ToJSON x => ToJSON (DagNode x) where
    toJSON (DagNode x ls)
      = object [ "Data" .= decodeLatin1 (Base64.encode $ LB.toStrict $ encode x)
               , "Links" .= fmap (\t -> object [ "Name" .= t
                                               , "RawIPFSHash" .= t
                                               -- TODO: figure out how to get actual size here
                                               ]) ls
               ]
