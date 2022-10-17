module Merkle.Higher.Types where

--------------------------------------------
import           Data.Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import           Data.Text
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           GHC.Generics (Generic)
--------------------------------------------


-- TODO: Blake2/3, functions for annotating, lifting to LMMT
newtype Hash k
  = Hash { unHash :: RawHash } deriving (Eq, Ord, Show, Generic)

type RawHash = Text

instance ToJSON (Hash x) where
  toJSON = String . unHash

instance FromJSON (Hash x) where
  parseJSON =
    withText "RawHash" (pure . Hash)

-- | enumerate raw untyped keys as owned by some type
class ExtractKeys f where
  extractHashKeys :: f Hash i -> [RawHash]


data DagNode a
  = DagNode
  { dnValue :: a
  , dnLinks :: [Text] -- raw hashes only
  }

instance FromJSON x => FromJSON (DagNode x) where
    parseJSON = withObject "dag node" $ \o -> do
              d <- o .: "Data"
              ls <- o .: "Links"
              ls' <- traverse (withObject "dag link" $ \o' -> o' .: "Hash") ls
              case Base64.decode (encodeUtf8 d) >>= eitherDecode . LB.fromStrict of
                Left err -> fail err
                Right x  -> pure $ DagNode x ls'

instance ToJSON x => ToJSON (DagNode x) where
    toJSON (DagNode x ls)
      = object [ "Data" .= decodeLatin1 (Base64.encode $ LB.toStrict $ encode x)
               , "Links" .= fmap (\t -> object [ "Name" .= t
                                               , "Hash" .= t
                                               -- TODO: figure out how to get actual size here
                                               ]) ls
               ]
