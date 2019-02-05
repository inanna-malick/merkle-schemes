module Merkle.Types where

--------------------------------------------
import           Data.Aeson
import qualified Data.Hashable as Hash
import           GHC.Generics (Generic)
--------------------------------------------
import           Util.RecursionSchemes (Fix(..))
import           Util.MyCompose
--------------------------------------------

type Hash = Int

-- | Pointer to a hash-identified entity
newtype Pointer = Pointer { unPointer :: Hash }
  deriving (Eq, Ord, Show, Generic)
instance Hash.Hashable Pointer

-- | Some entity (f a) identified by a hash pointer. Can either be a direct or indirect reference
data HashIdentifiedEntity a
  = Direct   Pointer a -- node id is included in node metadata of direct ref (pointer)
  | Indirect Pointer       -- indirect ref is just a pointer in some hash-addressed store
  deriving (Eq, Show, Functor)

mtPointer :: Fix (HashIdentifiedEntity :+ f) -> Pointer
mtPointer (In (C (Direct p _))) = p
mtPointer (In (C (Indirect p))) = p

instance FromJSON Pointer where
    -- this generates a Value
    parseJSON js = Pointer <$> parseJSON js

instance ToJSON Pointer where
    -- this generates a Value
    toJSON (Pointer p) =
        toJSON p
