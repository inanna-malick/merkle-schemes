module Merkle.Higher.Store.Deref where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Util.HRecursionSchemes
import           Merkle.Higher.Types
--------------------------------------------

-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall m f
   . Monad m
  => HFunctor f
  => NatM m (Hash f) (f (Hash f))
  -> Hash f :-> Term (Tagged (Hash f) `HCompose` Compose m `HCompose` f)
lazyDeref derefLayer = ana alg
  where
    alg :: Coalg (Tagged (Hash f) `HCompose` Compose m `HCompose` f) (Hash f)
    alg h = HC . Tagged h . HC . Compose $ derefLayer h
