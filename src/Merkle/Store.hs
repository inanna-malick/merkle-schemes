module Merkle.Store where

--------------------------------------------
import           Data.Kind (Type)
import           Merkle.Types (HashPointer, HashIndirect)
import           Util.HRecursionSchemes
--------------------------------------------

data Store m (f :: (k -> Type) -> k -> Type)
  = Store
  { sDeref :: NatM m (Const HashPointer) (f (Term (HashIndirect f)))
  , sUploadShallow :: AlgM m f (Const HashPointer)
  }

uploadDeep
  :: forall m f
   . HTraversable f
  => Monad m
  => Store m f
  -> NatM m (Term f) (Const HashPointer)
uploadDeep store = cataM (sUploadShallow store)
