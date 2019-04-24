module Merkle.Higher.Store.Deref where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Util.HRecursionSchemes
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Types
--------------------------------------------

-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall m p
   . Monad m
  => HFunctor p
  => Store m p
  -> Hash :-> Term (Tagged Hash `HCompose` Lazy m `HCompose` p)
lazyDeref store = futu alg
  where
    alg :: CVCoalg (Tagged Hash `HCompose` Lazy m `HCompose` p) Hash
    alg p = HC . Tagged p . HC . Compose $ hfmap (cata helper) <$> sDeref store p

    helper :: Alg (Tagged Hash `HCompose` Indirect `HCompose` p)
                  (Context (Tagged Hash `HCompose` Lazy m `HCompose` p) Hash)
    helper (HC (Tagged p (HC (Compose Nothing)))) = Hole p
    helper (HC (Tagged p (HC (Compose (Just x)))))
      = Term . HC $ Tagged p $ (HC (Compose $ pure $ x))
