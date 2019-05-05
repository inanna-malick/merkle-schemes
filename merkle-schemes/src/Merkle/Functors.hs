module Merkle.Functors where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Merkle.Types
import           Util.RecursionSchemes
--------------------------------------------

type HashAnnotated f = (,) (Hash f)

htPointer
  :: Fix (HashAnnotated x `Compose` f)
  -> Hash x
htPointer (Fix (Compose (p, _))) = p

htElem
  :: Fix (HashAnnotated x `Compose` f)
  -> f (Fix (HashAnnotated x `Compose` f))
htElem (Fix (Compose (_, e))) = e

-- | Remove hash annotations from some HashAnnotated structure
stripTags :: Functor f => Fix (HashAnnotated x `Compose` f) -> Fix f
stripTags = cata (Fix . snd . getCompose)

-- | Annotate each layer of some structure with its hash
hashTag
  :: Functor f
  => Hashable f
  => Fix f
  -> Fix (HashAnnotated f `Compose` f)
hashTag = annotate hash
