{-# LANGUAGE TemplateHaskell #-}

module Merkle.Store.TestUtils where

--------------------------------------------
import           Control.Exception.Safe (MonadThrow)
import           Control.Monad.IO.Class
import qualified Data.Aeson as AE
import           Data.Eq.Deriving
import           GHC.Generics
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
import           Merkle.Types
import           Util.RecursionSchemes
--------------------------------------------

-- | generate a nested structure -> deep upload -> strict download -> check (==)
storeTestDeep
  :: (Hashable f, Functor f, Traversable f, Eq1 f, Show1 f, MonadIO m, MonadThrow m)
  => Gen (Fix f)
  -> Store (PropertyT m) f
  -> PropertyT m ()
storeTestDeep g s = do
    x <- forAll g
    h <- uploadDeep s x
    r <- strictDeref $ lazyDeref' s h
    stripTags r === x


data Txn = Txn { from :: String, to :: String, amt :: Int }
  deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON Txn
instance AE.FromJSON Txn

data MockBlockchain a = Block [Txn] a | GenesisBlock
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

instance AE.ToJSON1 MockBlockchain
instance AE.FromJSON1 MockBlockchain
$(deriveShow1 ''MockBlockchain)
$(deriveEq1   ''MockBlockchain)

instance Hashable MockBlockchain where
  -- just show the thing, then hash that string. sufficient for testing.
  hash = doHash . pure . unpackString . show


genMockBlockchain :: MonadGen m => Int -> m (Fix MockBlockchain)
genMockBlockchain = anaM genBlockAlg

genBlockAlg :: MonadGen m => Int -> m (MockBlockchain Int)
genBlockAlg n = if n <= 0
  then pure GenesisBlock
  -- slight bias towards more dir structure until we run out of depth
  else genBlock

  where
    genBlock = flip Block (n -1) <$> Gen.list (Range.constant 1 5) genTxn
    genTxn = do
      from' <- Gen.string (Range.singleton 10) Gen.alphaNum
      to' <- Gen.string (Range.singleton 10) Gen.alphaNum
      amt' <- Gen.integral (Range.constant 0 100000)
      pure $ Txn from' to' amt'


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
  -- just show the thing, then hash that string. sufficient for testing.
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
