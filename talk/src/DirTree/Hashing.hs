module DirTree.Hashing where

import qualified Data.Aeson as AE

import qualified Crypto.Hash as CH
import qualified Crypto.Hash.Algorithms as CHA
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

-- | Hash pointer (points to value from which hash was derived)
newtype Hash = Hash { unHash :: CH.Digest CHA.SHA256 }
  deriving (Eq, Ord)

hashToText :: Hash -> Text
hashToText = decodeUtf8 . B16.encode . BA.pack . BA.unpack . unHash

textToHash :: Text -> Maybe Hash
textToHash = fmap Hash . f . B16.decode . encodeUtf8
  where
    f (x, remainder)
      -- no remaining unparseable bytes, sucess
      | remainder == B.empty = CH.digestFromByteString x
      | otherwise = Nothing


hash :: ByteString -> Hash
hash = Hash
     . CH.hashFinalize
     . CH.hashUpdates (CH.hashInit :: CH.Context CHA.SHA256)
     . pure

instance Show Hash where
  show = unpack . hashToText

instance AE.ToJSON Hash where
  toJSON = AE.String . hashToText

instance AE.FromJSON Hash where
  parseJSON =
    AE.withText "Hash"
      (maybe (fail "parsing failed") pure . textToHash)

