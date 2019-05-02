module Merkle.Higher.Types where

--------------------------------------------
import qualified Data.Aeson as AE
import           GHC.Generics (Generic)
import           Data.Text
--------------------------------------------

-- IPFS: string, compatible, 58 bit encoding - using string instead of bytestring for simplicity
newtype Hash k
  = Hash { unHash :: Text } deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON (Hash x) where
  toJSON = AE.String . unHash

instance AE.FromJSON (Hash x) where
  parseJSON =
    AE.withText "RawHash" (pure . Hash)
