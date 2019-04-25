module Merkle.Higher.Types where
  -- ( module Merkle.Higher.Types
  -- ) where

--------------------------------------------
import qualified Data.Aeson as AE
import           Data.Functor.Const (Const)
import           GHC.Generics (Generic)
import           Data.Kind (Type)
--------------------------------------------
-- import           Merkle.Types hiding (Hashable(hash))
import           Util.HRecursionSchemes (Alg)
--------------------------------------------

-- string, compatible, 58 bit encoding - using string instead of bytestring for simplicity
newtype IPFSHash = IPFSHash { unIPFSHash :: String } deriving Generic
type Hash = Const IPFSHash


instance AE.ToJSON IPFSHash

instance AE.FromJSON IPFSHash
