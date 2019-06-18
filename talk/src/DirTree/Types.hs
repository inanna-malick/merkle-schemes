module DirTree.Types where

import Control.RecursionSchemes
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Compose
import GHC.Generics (Generic)
import DirTree.Hashing

data DirLayer a = DirLayer { entities :: [(FilePath, FileTreeEntity a)]}
  deriving (Generic, Traversable, Foldable, Functor, Eq, Show)

instance AE.ToJSON   (DirLayer Hash)
instance AE.FromJSON (DirLayer Hash)

data FileTreeEntity a
  = FileEntity String
  | DirEntity  a
  deriving (Generic, Traversable, Foldable, Functor, Eq, Show)

type DirTree = Fix DirLayer


instance AE.ToJSON   (FileTreeEntity Hash)
instance AE.FromJSON (FileTreeEntity Hash)


type LazyDirTree m = Fix ((,) Hash `Compose` m `Compose` DirLayer)

buildLazyTree
  :: Monad m
  => (Hash -> m (DirLayer Hash))
  -> Hash
  -> LazyDirTree m
buildLazyTree fn = ana alg
  where
    alg h = Compose (h, Compose $ fn h)

-- | Hash a single directory layer
hashLayer :: DirLayer Hash -> Hash
hashLayer = hash . BL.toStrict . AE.encode

-- | Hash a directory tree by hashing each layer
hashDirTree :: DirTree -> Hash
hashDirTree = cata hashLayer
