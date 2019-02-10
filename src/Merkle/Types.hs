module Merkle.Types where

--------------------------------------------
import           Data.Functor.Foldable
--------------------------------------------
import           Util.MyCompose
--------------------------------------------

newtype Pointer = Pointer { unPointer :: Int } deriving (Eq, Ord, Show)

-- | Entity tagged with a hash that uniquely identifies it
type WithHash = (,) Pointer

pointer :: forall f . Fix $ WithHash :+ f -> Pointer
pointer = fst . getCompose . unfix

-- | Forget information at the term level - drop any direct references
makeShallow
  :: forall x
   . Functor x
  => x $ Fix (WithHash :+ x)
  -> x $ Pointer
makeShallow = fmap pointer

-- | Forget information at the type level
--   turn a value known to be shallow to a potentially-deep one
makeConcrete
  :: forall x
   . Functor x
  => x $ Pointer
  -> x $ Fix $ WithHash :+ Maybe :+ x
makeConcrete = fmap (\p -> Fix $ C (p,C Nothing))

indirectRef :: forall x . Pointer -> Fix $ WithHash :+ Maybe :+ x
indirectRef p = Fix $ C (p, x)
  where
    x :: Maybe :+ x $ Fix (WithHash :+ Maybe :+ x)
    x = C Nothing
