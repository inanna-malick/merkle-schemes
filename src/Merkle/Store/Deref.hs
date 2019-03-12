module Merkle.Store.Deref where

--------------------------------------------
import           Data.Functor.Compose
import           Data.Singletons
--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes
import           Merkle.Store
import           Merkle.Types
--------------------------------------------

-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall i m p
   . Monad m
  => HFunctor p
  => SingI i
  => Store m p
  -> Const HashPointer i
  -> Term (LazyHashTagged m p) i
lazyDeref store = futu alg
  where
    alg :: CVCoalg (LazyHashTagged m p) (Const HashPointer)
    alg p = Pair p . HC . Compose $ hfmap (cata helper) <$> sDeref store p

    helper :: Alg (HashIndirect p) (Context (LazyHashTagged m p) (Const HashPointer))
    helper (Pair p (HC (Compose Nothing))) = Hole p
    helper (Pair p (HC (Compose (Just x)))) = Term $ Pair p $ (HC (Compose $ pure $ x))











