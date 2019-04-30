module Merkle.Higher.Store where

--------------------------------------------
import           Data.Singletons
--------------------------------------------
import           Merkle.Higher.Types
--------------------------------------------

data Store m (f :: (k -> *) -> k -> *)
  = Store
  { sGet :: forall i. SingI i => Hash f i -> m (Maybe (f (Hash f) i))
  , sPut :: forall i. SingI i => f (Hash f) i -> m (Hash f i)
  }
