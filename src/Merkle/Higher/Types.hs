module Merkle.Higher.Types where
  -- ( module Merkle.Higher.Types
  -- ) where

--------------------------------------------
import           Control.Applicative (Const)
import qualified Data.Aeson as AE
import           GHC.Generics (Generic)
import           Data.Text
--------------------------------------------

-- IPFS: string, compatible, 58 bit encoding - using string instead of bytestring for simplicity
newtype IPFSHash (f :: (k -> *) -> k -> *)
  = IPFSHash { unIPFSHash :: Text } deriving (Eq, Ord, Generic)
type Hash f = Const (IPFSHash f)

instance AE.ToJSON (IPFSHash x) where
  toJSON = AE.String . unIPFSHash

instance AE.FromJSON (IPFSHash x) where
  parseJSON =
    AE.withText "RawHash" (pure . IPFSHash)

type MerkleLayer f = f (Hash f)
