module Merkle.Store.Test where

--------------------------------------------
import           Control.Monad.Trans.State.Lazy (StateT, gets, modify)
import           Data.Functor.Compose
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
--------------------------------------------
import           Util.RecursionSchemes
import           Merkle.Store
import           Merkle.Types
--------------------------------------------

testStore
  :: forall m f
   . ( Hashable f
     , Functor f
     , Monad m
     )
  => Store (StateT (SSMap f) m) f
testStore = Store
  { sDeref = \p -> gets (lookup' p)
  , sUploadShallow = \x -> do
          let p = hash x
          modify (M.insert p x)
          pure p
  }
  where
    lookup' :: Hash f -> SSMap f -> Maybe (DerefRes f)
    lookup' p h =
      let lr = M.lookup p h
       in fmap (fmap (Fix . Compose . (, Compose $ Nothing))) lr


type SSMap f = Map (Hash f) (f (Hash f))
