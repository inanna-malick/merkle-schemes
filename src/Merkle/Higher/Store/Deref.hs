module Merkle.Higher.Store.Deref where

--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throwString)
import           Data.Functor.Compose
--------------------------------------------
import           Util.HRecursionSchemes
import           Merkle.Higher.Store
import           Merkle.Higher.Types
--------------------------------------------

-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store.
lazyDeref
  :: forall m f
   . (MonadThrow m, Monad m, HFunctor f)
  => Store m f
  -> Hash :-> Term (Tagged Hash `HCompose` Compose m `HCompose` f)
lazyDeref store = ana alg
  where
    alg :: Coalg (Tagged Hash `HCompose` Compose m `HCompose` f) Hash
    -- todo: better error reporting, this is hax from latenite
    alg h = HC . Tagged h . HC . Compose $ get h

    get h = sGet store h >>= maybe (throwString "elem not found") pure
