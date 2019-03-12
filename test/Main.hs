module Main where


import Data.Singletons
import Data.Functor.Compose
import Control.Monad.IO.Class
import qualified Data.Aeson as AE
import Test.Hspec
import Control.Exception (evaluate)
import HGit.Serialization
import HGit.Diff
import HGit.Merge
import HGit.Diff.Types
import HGit.Gen
import HGit.Types.HGit
import Util.MyCompose
import Util.HRecursionSchemes
import Merkle.Types
import Merkle.Store
import Merkle.Store.Deref

import Data.Map (Map)
import qualified Data.Map as M


import           Control.Monad.Trans.State.Lazy

import           Control.Monad.Trans.Except

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


main :: IO ()
main = do
  let dir xs = Term $ Dir xs
      dir' n xs = (n,) . DirEntity $ dir xs
      -- TODO/IDEA: store commit messages as blobs!
      blob body = Term $ Blob body -- todo delete blobtree
      file n  = (n,) . FileEntity . blob
      commit msg r ps  = Term $ Commit msg r ps
      liftHD :: Term HGit :-> Term (HashIndirect HGit)
      liftHD = makeIndirect . hashTag structuralHash

  let roundtrip :: forall i . SingI i => HashIndirectTerm i -> Either String (HashIndirectTerm i)
      roundtrip = AE.eitherDecode . AE.encode


  -- let x' = liftHD $ dir [file "fname" "fblob"]
  -- let x = HashIndirectTerm $ liftHD' $ x'

  let propRoundtrip s =
        property $ do
          dt <- forAll $ (HashIndirectTerm <$> genIndTagged s)
          -- liftIO $ print $ AE.encode dt
          roundtrip dt === Right dt

  checkParallel $ Group "Encoding.RoundTrip" [
        ("dir tag round trip", propRoundtrip SDirTag),
        ("file tag round trip", propRoundtrip SBlobTag),
        ("commit tag round trip", propRoundtrip SCommitTag)
      ]

  pure ()

  hspec $ do
    describe "round trip (HashIndirectTerm)" $ do
      it "commit encoding" $ do
        let r1 = dir [file "fname" "fblob", dir' "subdir" [file "f1" "foo", file "f2" "bar"]]
            r2 = dir [("base", DirEntity r1), dir' "tmp" [("bkup", DirEntity r1)]]
            c = commit "commit 2" r1 (pure $ commit "c1" r2 $ pure $ Term NullCommit)
            hidt = HashIndirectTerm $ liftHD c

        -- print $ AE.encode hidt

        roundtrip hidt `shouldBe` Right hidt

    -- todo tests that confirm laziness via boobytrapped branches (via error on eval)
    describe "diff" $ do
      let lift = makeLazy . hashTag structuralHash
          diffTest r1 r2 expected = do
            diffRes <- diffMerkleDirs (lift r1) (lift r2)
            diffRes `shouldBe` expected

      it "modify file" $ do
        let r1 = dir [dir' "foo" [file "bar" "bar.body.v1"]]
            r2 = dir [dir' "foo" [file "bar" "bar.body.v999.final.freeze.01.draft.5"]]
        diffTest r1 r2 [(["foo", "bar"], FileModified)]

      it "add file" $ do -- todo different enums for file/dir created?
        let r1 = dir []
            r2 = dir [dir' "foo" [file "bar" "bar.body"]]
        diffTest r1 r2 [(["foo"], EntityCreated)]

      it "add dir" $ do -- todo make full recursive add/delete diff?
        let r1 = dir []
            r2 = dir [file "bar" "bar.body"]
        diffTest r1 r2 [(["bar"], EntityCreated)]

      it "replace dir with file" $ do
        let r1 = dir [file "foo" "foo.body", dir' "baz" [file "bar" "bar.body"]]
            r2 = dir [dir' "foo" [file "bar" "bar.body"], file "baz" "baz.body"]
        diffTest r1 r2 [(["baz"], DirReplacedWithFile), (["foo"], FileReplacedWithDir)]


    describe "merge" $ do
      let lift = makeLazy . hashTag structuralHash

      it "merge with safely overlapping changes" $ do
        let r1 = dir [ dir' "baz" [ file "bar" "bar.body"
                                  ]
                     , file "bar" "bar.body"
                     ]
            r2 = dir [ dir' "baz" [ file "foo" "foo.body"
                                  ]
                     , file "bar" "bar.body"
                     ]

            expected = dir [ dir' "baz" [ file "bar" "bar.body"
                                        , file "foo" "foo.body"
                                        ]
                           , file "bar" "bar.body"
                           ]

        (strictRes, _) <- flip runStateT emptyStore $ do
          Right res <- runExceptT $ mergeMerkleDirs' testStore (lift r1) (lift r2)
          makeStrict res

        (HashTaggedNT strictRes) `shouldBe` (HashTaggedNT $ hashTag structuralHash expected)

      it "merge with file-level conflict" $ do
        let r1 = dir [dir' "baz" [file "bar" "bar.body.b"]]
            r2 = dir [dir' "baz" [file "bar" "bar.body.a"]]

        (Left err, _storeState) <- flip runStateT emptyStore
                                 . runExceptT $ mergeMerkleDirs' testStore (lift r1) (lift r2)

        err `shouldBe` MergeViolation ["baz", "bar"]

type SSMap i = Map (Const HashPointer i) (HGit (Const HashPointer) i)

data StoreState
  = StoreState
  { ssCommits :: SSMap 'CommitTag
  , ssDirs    :: SSMap 'DirTag
  , ssBlobs   :: SSMap 'BlobTag
  }

emptyStore :: StoreState
emptyStore = StoreState M.empty M.empty M.empty

-- todo: bake 'Maybe' into lookup fn, stores should only have control over, eg, decode parse fail error type
testStore :: forall m . MonadIO m => Store (StateT StoreState m) HGit
testStore = Store
  { sDeref = handleDeref
  , sUploadShallow = \x -> do
      case x of
        NullCommit -> do
          let p = structuralHash x
          state <- get
          let state' = state { ssCommits = M.insert p x (ssCommits state) }
          put state'
          pure p
        c@(Commit _ _ _) -> do
          let p = structuralHash x
          state <- get
          let state' = state { ssCommits = M.insert p x (ssCommits state) }
          put state'
          pure p
        d@(Dir _) ->  do
          let p = structuralHash x
          state <- get
          let state' = state { ssDirs = M.insert p x (ssDirs state) }
          put state'
          pure p
        b@(Blob _) -> do
          let p = structuralHash x
          state <- get
          let state' = state { ssBlobs = M.insert p x (ssBlobs state) }
          put state'
          pure p
  }
  where
    handleDeref :: forall i
                 . SingI i
                => Const HashPointer i
                -> StateT StoreState m $ HGit (Term (HashIndirect HGit)) i
    handleDeref p = case sing @i of
        SCommitTag -> gets ssCommits >>= maybe (fail "key not found") (pure . hfmap (Term . flip Pair (HC $ Compose $ Nothing))) . M.lookup p
        SBlobTag   -> gets ssBlobs   >>= maybe (fail "key not found") (pure . hfmap (Term . flip Pair (HC $ Compose $ Nothing))) . M.lookup p
        SDirTag    -> gets ssDirs   >>= maybe (fail "key not found") (pure . hfmap (Term . flip Pair (HC $ Compose $ Nothing))) . M.lookup p
