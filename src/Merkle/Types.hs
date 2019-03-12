module Merkle.Types where

--------------------------------------------
import qualified Data.Aeson as AE
import           Data.Functor.Compose
import qualified Data.Hashable as H
--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes
--------------------------------------------

type HashTagged f = Pair (Const HashPointer) f

pointer :: forall f . Term (HashTagged f) :-> Const HashPointer
pointer (Term (Pair p _)) = p

-- | Remove hash annotations from some HashTagged structure
stripTags :: HFunctor f => Term (HashTagged f) :-> Term f
stripTags = cata (Term . pelem)

-- | Flatten a HashTagged structure
flatten
  :: HFunctor f
  => f (Term (HashTagged f)) :-> f (Const HashPointer)
flatten = hfmap pointer

-- | Annotate each layer of some structure with its hash
hashTag
  :: HFunctor f
  => HashFunction f
  -> Term f :-> Term (HashTagged f)
hashTag hf = cata (\x -> Term $ Pair (hf $ flatten x) x)

type HashIndirect f = HashTagged (Compose Maybe :++ f)

-- | Make some fully substantiated hash tagged structure 'indirect'
makeIndirect
  :: HFunctor f
  => Term (HashTagged f) :-> Term (HashIndirect f)
makeIndirect = cata (\(Pair p e) -> Term $ Pair p $ HC $ Compose $ Just e)

type LazyHashTagged m f = HashTagged (Compose m :++ f)

-- | Make some fully substantiated hash tagged structure 'indirect'
makeLazy
  :: HFunctor f
  => Monad m
  => Term (HashTagged f) :-> Term (LazyHashTagged m f)
makeLazy = cata (\(Pair p e) -> Term $ Pair p $ HC $ Compose $ pure e)


-- | Fully consumes potentially-infinite effectful stream and may not terminate
makeStrict
  :: forall m p
   . HTraversable p
  => Monad m
  => NatM m (Term (LazyHashTagged m p)) (Term (HashTagged p))
makeStrict = anaM alg
  where
    alg :: CoalgM m (HashTagged p) (Term (LazyHashTagged m p))
    alg (Term (Pair p (HC (Compose e)))) = Pair p <$> e

derefLayer
  :: forall f m
   . NatM m (Term (LazyHashTagged m f))
            (f (Term (LazyHashTagged m f)))
derefLayer (Term (Pair _ (HC (Compose m)))) = m


type HashFunction f = f (Const HashPointer) :-> Const HashPointer

-- | Hash pointer (points to value from which hash was derived),
newtype HashPointer = HashPointer { unHashPointer :: String }
  deriving (Eq, Ord)

instance Show HashPointer where
  show (HashPointer x) = "#[" ++ x ++ "]"

instance H.Hashable HashPointer where
  hashWithSalt i (HashPointer a) = i `H.hashWithSalt` (H.hash a)

instance AE.ToJSON HashPointer where
  toJSON (HashPointer x) = AE.toJSON x

instance AE.FromJSON HashPointer where
  parseJSON v = HashPointer <$> AE.parseJSON v

-- one-way function
-- (because I'm lazy and don't want or need to write a parser, no fundamental reason)
-- NICE COMPACT STRING REPR FOR CONVENIENCE
mkHashPointer :: Int -> HashPointer
mkHashPointer p = HashPointer $ prefix p ++ f (abs p)
  where
    prefix n | n > 0     = "x"
             | n < 0     = "y"
             | otherwise = "z"
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    base = length chars

    f n | n == 0 = ""
        | n < 0 = f $ (-1) * n -- no loss of info, handled via prefix
        | otherwise = chars !! (n `rem` base) : f (n `div` base)
