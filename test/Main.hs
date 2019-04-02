module Main where

import           Control.Exception.Safe                (throw, bracket)

import Data.Functor.Compose
import Control.Monad.IO.Class
import Test.Hspec
import HGit.Diff
import HGit.Merge
import HGit.Diff.Types
import HGit.Types.HGit
import Util.MyCompose
import Util.RecursionSchemes
import Merkle.Functors
import Merkle.Store
import Merkle.Store.Network
import Merkle.Store.FileSystem
import Merkle.Store.Deref
import Merkle.Types
import Data.Map (Map)
import qualified Data.Map as M
import           Control.Monad.Trans.State.Lazy (StateT, gets, runStateT, modify)
import           Control.Monad.Trans.Except (runExceptT)

import           Hedgehog
import Control.Monad.Fail (MonadFail)

import System.IO.Temp
import System.IO

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Control.Concurrent               as C
import qualified Network.Wai.Handler.Warp         as Warp
import           Servant.Client (mkClientEnv, runClientM, Scheme(..), BaseUrl(..))

import Network.HTTP.Client (newManager, defaultManagerSettings)

import Data.Aeson.Orphans ()


-- TODO: split merkle and hgit tests?

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let ffail :: MonadFail m => Hash x -> Fix (HashAnnotated x `Compose` m `Compose` x)
      ffail h = Fix $ Compose (h, Compose $ fail "Boom!")
      dir :: Applicative m
          => [NamedFileTreeEntity (Hash Blob) (Fix $ HashAnnotated HashableDir `Compose` m `Compose` HashableDir)]
          -> Fix (HashAnnotated (Dir (Hash Blob)) `Compose` m `Compose` Dir (Hash Blob))
      dir xs =
        let d = Dir xs
            h = hash $ fmap htPointer d
         in Fix $ Compose (h, Compose $ pure d)
      dir' n xs = (n,) . DirEntity $ dir xs
      blobHash body = hash $ Chunk body emptyHash
      file n = (n,) . FileEntity . blobHash

  -- generate a flat structure
  let propRoundtripShallow s =
        property $ do
          x <- forAll genDir
          h <- sUploadShallow s x
          r <- sDeref' s h
          (fmap htPointer r) === x

      genHash = do
        seed <- Gen.string  (Range.singleton 100) Gen.alphaNum
        pure $ doHash [unpackString seed]

      genNamedFileEntity = do
        n <- Gen.string (Range.singleton 10) Gen.alphaNum
        (n,) <$> Gen.choice [fmap DirEntity genHash, fmap FileEntity genHash]

      genDir :: Gen (HashableDir (Hash HashableDir))
      genDir = Dir <$> Gen.list (Range.constant 1 10) genNamedFileEntity

  -- generate a nested structure -> deep upload -> strict download -> check (==)
  let propRoundtripDeep s =
        property $ do
          x <- forAll gen
          h <- uploadDeep s x
          r <- strictDeref $ lazyDeref' s h
          stripTags r === x

      gen :: MonadGen m => m (Fix HashableDir)
      gen = anaM genDirR 5

      genDirR :: MonadGen m => Int -> m (HashableDir Int)
      genDirR n = Dir <$> Gen.list (Range.constant 1 3) (Gen.choice [genFile, genDir' n])

      genDir' :: MonadGen m => Int -> m (NamedFileTreeEntity (Hash Blob) Int)
      genDir' n = if n <= 0
        then genFile
        else do
          name <- Gen.string  (Range.singleton 10) Gen.alphaNum
          pure (name, DirEntity $ n - 1)

      genFile :: MonadGen m => m (NamedFileTreeEntity (Hash Blob) Int)
      genFile = do
        name <- Gen.string (Range.singleton 10) Gen.alphaNum
        body <- Gen.string (Range.singleton 100) Gen.alphaNum
        pure $ file name body -- file w/ hash of body

  -- use shared hash-addressed store for all tests - if hash == content ==, so safe. Merkle!
  propres <- withSystemTempDirectory "proptest" $ \ fspath ->
    withSystemTempDirectory "proptest" $ \ netpath -> do
      let port = 8081 -- todo random?
      manager <- newManager defaultManagerSettings
      let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
      let netStore' :: Store (PropertyT IO) HashableDir
          netStore' = liftStore (\mx -> liftIO (runClientM mx env) >>= either throw pure) netStore
      bracket (liftIO . C.forkIO . Warp.run port . app "dir" $ (fsStore netpath :: Store IO HashableDir))
        C.killThread $ \_ -> do
          checkSequential $ Group "Store.RoundTrip"
            [ ("fs: flat Dir merkle tree round trip via store", propRoundtripShallow $ fsStore fspath)
            , ("fs: deep Dir merkle tree round trip via store", propRoundtripDeep $ fsStore fspath)
            , ("network: flat Dir merkle tree round trip via store", propRoundtripShallow netStore')
            , ("network: deep Dir merkle tree round trip via store", propRoundtripDeep netStore')
            ]

  print propres

  hspec $ do
    describe "diff" $ do
      let diffTest r1 r2 expected = do
            diffRes <- diffMerkleDirs r1 r2
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

      it "diff lazily without descending into non-conflicting dir branches changes" $ do
        let shared :: forall m. Monad m => Fix (HashAnnotated HashableDir `Compose` m `Compose` HashableDir)
            shared = dir [ dir' "baz" [ file "bar" "bar.body"
                         ]
                         , file "bar" "bar.body"
                         ]

            sharedPointer = htPointer (shared @ Maybe) -- just needs some type param... FIXME
            -- ffail is used to booby-trap branches of the lazy merkle tree which shouldn't be derefed
            r1 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "foo" "foo.body"]
            r2 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "baz" "baz.body"]

        diffTest r1 r2 [(["foo"], EntityDeleted), (["baz"], EntityCreated)]

    describe "merge" $ do
      it "merge lazily without descending into non-conflicting dir branches changes" $ do
        let shared :: forall m. Monad m => Fix (HashAnnotated HashableDir `Compose` m `Compose` HashableDir)
            shared = dir [ dir' "baz" [ file "bar" "bar.body"
                         ]
                         , file "bar" "bar.body"
                         ]

            sharedPointer = htPointer (shared @ Maybe) -- just needs some type param... FIXME
            -- ffail is used to booby-trap branches of the lazy merkle tree which shouldn't be derefed
            r1 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "foo" "foo.body"]
            r2 = dir [ ("shared", DirEntity . ffail $ sharedPointer)
                     , file "baz" "baz.body"]

            expected :: forall m. Monad m => Fix (HashAnnotated HashableDir `Compose` m `Compose` HashableDir)
            expected = dir [ file "baz" "baz.body"
                           , file "foo" "foo.body"
                           , ("shared", DirEntity $ shared)
                           ]

        (strictRes, _) <- flip runStateT (M.empty :: SSMap HashableDir) $ do
          -- needs to be derefed from store, so upload
          _ <- strictDeref shared >>= uploadDeep testStore . stripTags
          Right res <- runExceptT $ mergeMerkleDirs' testStore r1 r2

          -- res still has poisoned branches, so deref from pointer (b/c store has shared structure)
          strictDeref $ lazyDeref' testStore $ htPointer res

        strictExpected <- strictDeref expected -- janky.. should have lazy & strict branches

        strictRes `shouldBe` strictExpected

      it "merge with safely overlapping changes" $ do
        let r1 = dir [ dir' "baz" [ file "bar" "bar.body"

                                  ]
                     , file "bar" "bar.body"
                     ]
            r2 = dir [ dir' "baz" [ file "foo" "foo.body"
                                  ]
                     , file "bar" "bar.body"
                     ]

            expected = dir [ file "bar" "bar.body"
                           , dir' "baz" [ file "bar" "bar.body"
                                        , file "foo" "foo.body"
                                        ]
                           ]

        (strictRes, _) <- flip runStateT M.empty $ do
          Right res <- runExceptT $ mergeMerkleDirs' testStore r1 r2
          strictDeref res

        strictExpected <- strictDeref expected -- janky.. should have lazy & strict branches
        strictRes `shouldBe` strictExpected

      it "merge with file-level conflict" $ do
        let r1 = dir [dir' "baz" [file "bar" "bar.body.b"]]
            r2 = dir [dir' "baz" [file "bar" "bar.body.a"]]

        (Left err, _storeState) <- flip runStateT M.empty
                                 . runExceptT $ mergeMerkleDirs' testStore r1 r2

        err `shouldBe` MergeViolation ["baz", "bar"]

type SSMap f = Map (Hash f) (f (Hash f))

-- TODO: move to Merkle.Store.Test
testStore
  :: forall m f
   . ( Hashable f
     , Functor f
     , MonadIO m
     )
  => Store (StateT (SSMap f) m) f
testStore = Store
  { sDeref = \p -> gets (lookup' p)
  , sUploadShallow = \x -> do
          let p = hash x
          modify (M.insert p x)
          pure p
  }
  where
    lookup' :: Hash f -> SSMap f -> Maybe (DerefRes f)
    lookup' p h =
      let lr = M.lookup p h
       in fmap (fmap (Fix . Compose . (, Compose $ Nothing))) lr
