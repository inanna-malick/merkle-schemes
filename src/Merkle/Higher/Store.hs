module Merkle.Higher.Store where

--------------------------------------------
import           Data.Kind (Type)
import           Data.Singletons
import           Merkle.Higher.Functors (Indirect)
import           Merkle.Higher.Types (Hash)
import           Util.HRecursionSchemes
--------------------------------------------

type DerefRes f = f (Term (Tagged Hash `HCompose` Indirect `HCompose` f))


-- Not including optional deref failure, will only be used as IPFS/GUN interface
data Store m (f :: (k -> Type) -> k -> Type)
  = Store
  { sDeref :: forall i. SingI i => Hash i -> m (f Hash i)
  , sUpload :: AlgM m f Hash
  }

uploadDeep
  :: forall m f
   . HTraversable f
  => Monad m
  => Store m f
  -> NatM m (Term f) Hash
uploadDeep store = cataM (sUpload store)
