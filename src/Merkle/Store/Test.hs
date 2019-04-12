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


type SSMap f = Map (Hash f) (f (Hash f))

-- TODO: move to Merkle.Store.Test
testStore
  :: forall m f s
   . ( Hashable f
     , Functor f
     , Monad m
     )
  => (s -> SSMap f)
  -> (s -> SSMap f -> s)
  -> Store (StateT s m) f
testStore getF updateF = Store
  { sDeref = \p -> gets (lookup' p . getF)
  , sUploadShallow = \x -> do
          let p = hash x
          modify (\s -> updateF s . M.insert p x $ getF s)
          pure p
  }
  where
    lookup' :: Hash f -> SSMap f -> Maybe (DerefRes f)
    lookup' p h =
      let lr = M.lookup p h
       in fmap (fmap (Fix . Compose . (, Compose $ Nothing))) lr
