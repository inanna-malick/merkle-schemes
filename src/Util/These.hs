module Util.These where

--------------------------------------------
import qualified Data.Hashable as Hash
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
--------------------------------------------

-- | this, that or these
data These a b = This a | These a b | That b deriving (Eq, Ord, Show, Functor)

-- | Given two maps, return 'This' for elements only in the first map,
--   'That' for elements only in the second map, and 'These' for elements in both
mapCompare :: Eq k => Hash.Hashable k => HashMap k v -> HashMap k v -> [These (k,v) (k,v)]
mapCompare h1 h2 = h1Only ++ h2Only ++ both
  where h1Only = fmap This . Map.toList $ Map.difference h1 h2
        h2Only = fmap That . Map.toList $ Map.difference h2 h1
        both   = Map.elems $ Map.intersectionWithKey (\k v1 v2 -> These (k,v1) (k,v2)) h1 h2
