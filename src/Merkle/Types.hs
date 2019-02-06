module Merkle.Types where

--------------------------------------------
import           Data.Aeson
import qualified Data.Hashable as Hash
import           GHC.Generics (Generic)
--------------------------------------------
-- import           Util.RecursionSchemes (Fix(..))
-- import           Util.MyCompose
--------------------------------------------

type Hash = Int

-- | Pointer to a hash-identified entity
newtype Pointer = Pointer { unPointer :: Hash }
  deriving (Eq, Ord, Show, Generic)
instance Hash.Hashable Pointer

instance FromJSON Pointer where
    -- this generates a Value
    parseJSON js = Pointer <$> parseJSON js

instance ToJSON Pointer where
    -- this generates a Value
    toJSON (Pointer p) =
        toJSON p
