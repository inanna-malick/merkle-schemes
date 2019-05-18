module Merkle.Functors where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Merkle.Types
import           Util.RecursionSchemes
--------------------------------------------

type HashAnnotated h f = (,) (Hash h f)

htPointer
  :: Fix (HashAnnotated h x `Compose` f)
  -> Hash h x
htPointer (Fix (Compose (p, _))) = p

htElem
  :: Fix (HashAnnotated h x `Compose` f)
  -> f (Fix (HashAnnotated h x `Compose` f))
htElem (Fix (Compose (_, e))) = e

-- | Remove hash annotations from some HashAnnotated structure
stripTags :: Functor f => Fix (HashAnnotated h x `Compose` f) -> Fix f
stripTags = cata (Fix . snd . getCompose)

-- | Annotate each layer of some structure with its hash
hashTag
  :: Functor f
  => Hashable h f
  => Fix f
  -> Fix (HashAnnotated h f `Compose` f)
hashTag = annotate hash
