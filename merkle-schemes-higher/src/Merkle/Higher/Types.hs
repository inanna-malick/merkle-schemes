module Merkle.Higher.Types where

--------------------------------------------
import           Data.Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import           Data.Text
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           GHC.Generics (Generic)
--------------------------------------------

-- IPFS: string, compatible, 58 bit encoding - using string instead of bytestring for simplicity
newtype Hash k
  = Hash { unHash :: Text } deriving (Eq, Ord, Show, Generic)

instance ToJSON (Hash x) where
  toJSON = String . unHash

instance FromJSON (Hash x) where
  parseJSON =
    withText "RawHash" (pure . Hash)



data DagNode a
  = DagNode
  { dnValue :: a
  -- TODO: links? skipping for now
  }

instance FromJSON x => FromJSON (DagNode x) where
    parseJSON = withObject "dag node" $ \o -> do
              d <- o .: "Data"
              case Base64.decode (encodeUtf8 d) >>= eitherDecode . LB.fromStrict of
                Left err -> fail err
                Right x  -> pure $ DagNode x

instance ToJSON x => ToJSON (DagNode x) where
    toJSON (DagNode x)
      = object [ "Data" .= decodeLatin1 (Base64.encode $ LB.toStrict $ encode x)
               -- add this because it may be req'd for parsing (FIXME)
               , "Links" .= ([] :: [Int]) -- TODO actual links encoding (not sure how to get size here..)
               ]
