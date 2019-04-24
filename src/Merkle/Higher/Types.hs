module Merkle.Higher.Types
  ( module Merkle.Higher.Types
  , module Merkle.Types
  ) where

--------------------------------------------
import           Data.Kind (Type)
--------------------------------------------
import           Merkle.Types hiding (Hashable(hash))
import           Util.HRecursionSchemes (Alg)
--------------------------------------------

class Hashable (f :: (k -> Type) -> k -> Type) where
  -- flatten a single layer of structure where all
  -- sub-layers are hash pointers down to a hash
  hash :: Alg f Hash
