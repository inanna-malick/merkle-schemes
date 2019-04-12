{-# LANGUAGE TemplateHaskell #-}

module Main where

--------------------------------------------
import qualified Control.Concurrent               as C
import           Control.Exception.Safe (throw, bracket)
import           Control.Monad.IO.Class
import qualified Data.Aeson as AE
import           Data.Eq.Deriving
import           GHC.Generics
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Network.Wai.Handler.Warp         as Warp
import           Servant.Client (mkClientEnv, runClientM, Scheme(..), BaseUrl(..))
import           System.IO.Temp
import           Text.Show.Deriving
--------------------------------------------
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
--------------------------------------------
import           Data.Aeson.Orphans ()
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Store.Deref
import           Merkle.Store.FileSystem
import           Merkle.Store.Network
import           Merkle.Types
import           Util.RecursionSchemes
--------------------------------------------


genHash :: forall f. Gen (Hash f)
genHash = do
  seed <- Gen.string  (Range.singleton 100) Gen.alphaNum
  pure $ doHash [unpackString seed]

-- | generate a nested structure -> deep upload -> strict download -> check (==)
storeTestDeep :: (Functor f, Traversable f, Eq1 f, Show1 f) => Gen (Fix f) -> Store (PropertyT IO) f -> Property
storeTestDeep g s = property $ do
    x <- forAll g
    h <- uploadDeep s x
    r <- strictDeref $ lazyDeref' s h
    stripTags r === x



type FileBody = String
type PartialFilePath = String

data MockDirectoryTree x
  = Dir PartialFilePath [x]
  | File PartialFilePath FileBody
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

instance AE.ToJSON1 MockDirectoryTree
instance AE.FromJSON1 MockDirectoryTree

$(deriveShow1 ''MockDirectoryTree)
$(deriveEq1   ''MockDirectoryTree)

instance Hashable MockDirectoryTree where
  -- just show the thing, then hash that string
  hash = doHash . pure . unpackString . show

genMockDirTree :: MonadGen m => Int -> m (Fix MockDirectoryTree)
genMockDirTree = anaM genDirAlg

genDirAlg :: MonadGen m => Int -> m (MockDirectoryTree Int)
genDirAlg n = if n <= 0
  then genFile
  -- slight bias towards more dir structure until we run out of depth
  else Gen.choice [genFile, genDir, genDir]

  where
    genDir = do
      name <- Gen.string  (Range.singleton 10) Gen.alphaNum
      subdirs <- Gen.list (Range.constant 1 3) (pure $ n - 1)
      pure $ Dir name subdirs

    genFile = do
      name <- Gen.string (Range.singleton 10) Gen.alphaNum
      body <- Gen.string (Range.singleton 100) Gen.alphaNum
      pure $ File name body



main :: IO Bool
main = do
  -- hSetBuffering stdout NoBuffering
  -- use shared hash-addressed store for all tests - if hash == content ==, so safe. Merkle!
  withSystemTempDirectory "proptest" $ \ fspath ->
    withSystemTempDirectory "proptest" $ \ netpath -> do
      let port = 8081 -- todo random?
      manager <- newManager defaultManagerSettings
      let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
      let netStore' :: Store (PropertyT IO) MockDirectoryTree
          netStore' = liftStore (\mx -> liftIO (runClientM mx env) >>= either throw pure) netStore
      bracket (liftIO . C.forkIO . Warp.run port $ app (fsStore netpath :: Store IO MockDirectoryTree))
        C.killThread $ \_ -> do
          -- all this should work fine in parallel - store is hash addressed, so no collisions should matter
          checkParallel $ Group "Store.RoundTrip"
            [ ("file system store round trip", storeTestDeep (genMockDirTree 5) $ fsStore fspath)
            , ("network store round trip", storeTestDeep (genMockDirTree 5) netStore')
            ]
