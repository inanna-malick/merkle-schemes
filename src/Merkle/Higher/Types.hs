module Merkle.Higher.Types where
  -- ( module Merkle.Higher.Types
  -- ) where

--------------------------------------------
import qualified Data.Aeson as AE
import           Data.Functor.Const (Const)
import           GHC.Generics (Generic)
import           Data.Kind (Type)
import           Data.Text
--------------------------------------------

-- IPFS: string, compatible, 58 bit encoding - using string instead of bytestring for simplicity
newtype IPFSHash (f :: (k -> Type) -> k -> Type) = IPFSHash { unIPFSHash :: Text } deriving Generic
type Hash f = Const (IPFSHash f)

instance AE.ToJSON (IPFSHash x) where
  toJSON = AE.String . unIPFSHash

instance AE.FromJSON (IPFSHash x) where
  parseJSON =
    AE.withText "RawHash" (pure . IPFSHash)
