module Merkle.Types where

--------------------------------------------
import qualified Crypto.Hash as CH
import qualified Crypto.Hash.Algorithms as CHA
import qualified Data.Aeson as AE
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Functor.Const (Const(..))
import           Data.Kind (Type)
import           Data.Text (Text, unpack, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
--------------------------------------------
import           Util.HRecursionSchemes (Alg)
--------------------------------------------

class Hashable (f :: (k -> Type) -> k -> Type) where
  -- flatten a single layer of structure where all
  -- sub-layers are hash pointers down to a hash
  hash :: Alg f Hash

-- | Type-tagged hash pointer
type Hash = Const RawHash

-- | Hash pointer (points to value from which hash was derived)
-- , digests tagged with hash alg (blake2b_256) at type level
newtype RawHash = RawHash { unRawHash :: CH.Digest CHA.Blake2b_256 }
  deriving (Eq, Ord)

hashToText :: RawHash -> Text
hashToText = decodeUtf8 . B16.encode . BA.pack . BA.unpack . unRawHash

textToHash :: Text -> Maybe RawHash
textToHash = fmap RawHash . f . B16.decode . encodeUtf8
  where
    f (x, remainder)
      -- no remaining unparseable bytes, sucess (note, 2x failure paths, todo: better logging)
      | remainder == B.empty = CH.digestFromByteString x
      | otherwise = Nothing

instance Show RawHash where
  show x = "#[" ++ unpack (hashToText x) ++ "]"

instance AE.ToJSON RawHash where
  toJSON = AE.String . hashToText

instance AE.FromJSON RawHash where
  parseJSON =
    AE.withText "RawHash"
      (maybe (fail "parsing failed") pure . textToHash)


unpackString :: String -> ByteString
unpackString = encodeUtf8 . pack

unpackHash :: Hash i -> ByteString
unpackHash = BA.pack . BA.unpack . unRawHash . getConst

-- | do actual hash computation type stuff. blake2b!
doHash :: [ByteString] -> Hash i
doHash = Const . RawHash . CH.hashFinalize
       . CH.hashUpdates (CH.hashInit :: CH.Context CHA.Blake2b_256)
