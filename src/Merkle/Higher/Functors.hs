module Merkle.Higher.Functors where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Merkle.Higher.Types
import           Util.HRecursionSchemes
--------------------------------------------

pointer :: forall f . Term (Tagged Hash `HCompose` f) :-> Hash
pointer (Term (HC (Tagged p _))) = p

-- | Remove hash annotations from some Tagged Hash structure
stripTags :: HFunctor f => Term (Tagged Hash `HCompose` f) :-> Term f
stripTags = cata (Term . _elem . getHC)

-- | Flatten a Tagged Hash structure
flatten
  :: HFunctor f
  => f (Term (Tagged Hash `HCompose` f)) :-> f Hash
flatten = hfmap pointer

-- | Annotate each layer of some structure with its hash
hashTag
  :: HFunctor f
  => Hashable f
  => Term f :-> Term (Tagged Hash `HCompose` f)
hashTag = annotate hash

type Indirect = Compose Maybe

-- | Make some fully substantiated hash tagged structure 'indirect'
makeIndirect
  :: HFunctor f
  => Term (Tagged Hash `HCompose` f) :-> Term (Tagged Hash `HCompose` Indirect `HCompose` f)
makeIndirect = cata (\(HC (Tagged p e)) -> Term . HC . Tagged p . HC . Compose $ Just e)

type Lazy m = Compose m

-- | Make some fully substantiated hash tagged structure 'indirect'
makeLazy
  :: HFunctor f
  => Monad m
  => Term (Tagged Hash `HCompose` f) :-> Term (Tagged Hash `HCompose` Lazy m `HCompose` f)
makeLazy = cata (\(HC (Tagged p e)) -> Term . HC . Tagged p . HC . Compose $ pure e)


-- | Fully consumes potentially-infinite effectful stream and may not terminate
makeStrict
  :: forall m f
   . HTraversable f
  => Monad m
  => NatM m (Term (Tagged Hash `HCompose` Lazy m `HCompose` f)) (Term (Tagged Hash `HCompose` f))
makeStrict = anaM derefLayer'

derefLayer'
  :: forall f m
   . Functor m
  => CoalgM m (Tagged Hash `HCompose` f) (Term (Tagged Hash `HCompose` Lazy m `HCompose` f) )
derefLayer' (Term (HC (Tagged p (HC (Compose m))))) = HC . Tagged p <$> m


derefLayer
  :: forall f m
   . Functor m
  => CoalgM m f (Term (Tagged Hash `HCompose` Lazy m `HCompose` f) )
derefLayer = fmap (_elem . getHC) . derefLayer'
