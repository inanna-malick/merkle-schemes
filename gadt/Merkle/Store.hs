module Merkle.Store where

--------------------------------------------
import           Data.Kind (Type)
import           Merkle.Functors (Indirect)
import           Merkle.Types (Hash)
import           Util.MyCompose
import           Util.HRecursionSchemes
--------------------------------------------

data Store m (f :: (k -> Type) -> k -> Type)
  = Store
  { sDeref :: NatM m Hash (f (Term (Tagged Hash :++ Indirect :++ f)))
  , sUploadShallow :: AlgM m f Hash
  }

uploadDeep
  :: forall m f
   . HTraversable f
  => Monad m
  => Store m f
  -> NatM m (Term f) Hash
uploadDeep store = cataM (sUploadShallow store)
