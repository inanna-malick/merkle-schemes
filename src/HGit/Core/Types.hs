{-# LANGUAGE TemplateHaskell #-}

module HGit.Core.Types where

--------------------------------------------
import qualified Data.Aeson as AE
import           Data.Bifunctor.TH
import           Data.ByteString (ByteString)
import           Data.Eq.Deriving
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Functor.Compose
import           Control.Monad (join)
import           GHC.Generics
import           Text.Show.Deriving
--------------------------------------------
import           Merkle.Types
import           Merkle.Functors
import           Util.RecursionSchemes
--------------------------------------------

type PartialFilePath = String
type BranchName      = String
type CommitMessage   = String

data Blob a
  -- NOTE: using String instead of Bytestring to allow for easy examination of serialized files
  = Chunk String a
  | Empty
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

instance AE.ToJSON1 Blob
instance AE.FromJSON1 Blob

data FileTreeEntity a b
  = FileEntity a -- file type
  | DirEntity  b -- continued directory structure type
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

$(deriveBifoldable    ''FileTreeEntity)
$(deriveBifunctor     ''FileTreeEntity)
$(deriveBitraversable ''FileTreeEntity)


$(deriveShow1 ''FileTreeEntity)
$(deriveShow2 ''FileTreeEntity)
$(deriveEq2   ''FileTreeEntity)
$(deriveEq1   ''FileTreeEntity)

instance AE.ToJSON1 (FileTreeEntity (Hash (Blob)))
instance AE.FromJSON1 (FileTreeEntity (Hash (Blob)))

type NamedFileTreeEntity a b
  = ( PartialFilePath -- name of this directory entry (files and dirs have same name rules)
    , FileTreeEntity a b
    )

data Dir a b = Dir { dirEntries :: [NamedFileTreeEntity a b] }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

type HashableDir = Dir (Hash Blob)

type LazyMerkleDir m x = Fix (HashAnnotated (Dir x) `Compose` m `Compose`  Dir x)

$(deriveBifoldable    ''Dir)
$(deriveBifunctor     ''Dir)
$(deriveBitraversable ''Dir)

$(deriveShow1 ''Dir)
$(deriveShow2 ''Dir)
$(deriveEq2   ''Dir)
$(deriveEq1   ''Dir)

instance AE.ToJSON1 (Dir (Hash (Blob)))
instance AE.FromJSON1 (Dir (Hash (Blob)))


data Commit a b = NullCommit | Commit String a (NonEmpty b)
  deriving  (Eq, Ord, Functor, Foldable, Traversable, Generic1)

$(deriveBifoldable    ''Commit)
$(deriveBifunctor     ''Commit)
$(deriveBitraversable ''Commit)

$(deriveShow1 ''Commit)
$(deriveShow2 ''Commit)
$(deriveEq2   ''Commit)
$(deriveEq1   ''Commit)

type HashableCommit = Commit (Hash HashableDir)

instance AE.ToJSON1   HashableCommit
instance AE.FromJSON1 HashableCommit

-- | sort dir here by file name, specific order is irrelevant
canonicalOrdering :: [NamedFileTreeEntity a b] -> [NamedFileTreeEntity a b]
canonicalOrdering = sortOn fst

instance Hashable Blob where
  -- file-type entities
  hash (Chunk chunk next) = doHash $ ["blob", unpackString chunk, unpackHash next]
  hash (Empty) = emptyHash

instance Hashable HashableDir where
  hash (Dir []) = emptyHash
  -- non-empty dir-type entities
  hash (Dir xs) = doHash $ ["dir" :: ByteString] ++ join (fmap hash' $ canonicalOrdering xs)
    where
      hash' (n, FileEntity h) = ["subfile", unpackString n, unpackHash h]
      hash' (n, DirEntity  h) = ["subdir",  unpackString n, unpackHash h]

instance Hashable HashableCommit where
  -- commit-type entities
  hash NullCommit = emptyHash
  hash (Commit msg root parents)
    = doHash $
        [ unpackString msg
        , unpackHash root
        ] ++ toList (fmap unpackHash parents)
