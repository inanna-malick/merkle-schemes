module HGit.Gen where

--------------------------------------------
import           Data.Singletons
import           Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
--------------------------------------------
import           Merkle.Types
import           HGit.Serialization
import           HGit.Types
import           Util.HRecursionSchemes
--------------------------------------------

-- | always substantiated, just for type purposes
genIndTagged :: forall m . MonadGen m => NatM m Sing (Term (HashIndirect HGit))
genIndTagged = fmap makeIndirect <$> genTagged

genTagged :: forall m . MonadGen m => NatM m Sing (Term (HashTagged HGit))
genTagged = fmap (hashTag structuralHash) <$> gen

gen :: forall m . MonadGen m => NatM m Sing (Term HGit)
gen = anaM alg
  where
    alg :: CoalgM m HGit Sing
    alg = \case
      SCommitTag    ->
        Gen.recursive Gen.choice
          [pure NullCommit]
          [(\msg -> Commit msg sing (pure sing)) <$> shortString]
      SDirTag ->
        Gen.recursive Gen.choice
          [pure $ Dir []]
          [Dir <$> shortList dirOrFile]
      SBlobTag -> fmap Blob shortString


-- constant size
shortString :: MonadGen m => m String
shortString = Gen.string (Range.singleton 5) Gen.alphaNum -- arbitrary length, 5

-- shrinks with gen size
shortList :: MonadGen m => m a -> m [a]
shortList m = Gen.list (Range.linear 0 5) m -- arbitrary length, [0,5] (base/max)

dirOrFile :: MonadGen m => m (NamedFileTreeEntity Sing)
dirOrFile =
        Gen.recursive Gen.choice
          [f]
          [f, d]
  where
    f = (,FileEntity $ sing) <$> shortString
    d = (,DirEntity  $ sing) <$> shortString
