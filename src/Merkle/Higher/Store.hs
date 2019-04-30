module Merkle.Higher.Store where

--------------------------------------------
import           Control.Applicative (Const(..))
import           Data.Singletons
--------------------------------------------
import           Merkle.Higher.Types
--------------------------------------------

data Store m (f :: (k -> *) -> k -> *)
  = Store
  { sGet :: forall i. SingI i => Const (IPFSHash f) i -> m (Maybe (f (Const (IPFSHash f)) i))
  , sPut :: forall i. SingI i => f (Const (IPFSHash f)) i -> m (Const (IPFSHash f) i)
  }
