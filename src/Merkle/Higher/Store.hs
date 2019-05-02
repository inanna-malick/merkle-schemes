module Merkle.Higher.Store where

--------------------------------------------
import           Data.Singletons
--------------------------------------------
import           Merkle.Higher.Types
--------------------------------------------

data Store m (f :: (k -> *) -> k -> *)
  = Store
  { sGet :: forall i. SingI i => Hash i -> m (Maybe (f Hash i))
  , sPut :: forall i. SingI i => f Hash i -> m (Hash i)
  }
