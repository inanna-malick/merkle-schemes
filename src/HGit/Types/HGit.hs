module HGit.Types.HGit where

--------------------------------------------
import           Data.ByteString (ByteString)
import           Data.List (sortOn)
import           Data.List.NonEmpty
import           Data.Singletons.TH
import           Control.Monad (join)
--------------------------------------------
import           HGit.Types.Common
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
import           Merkle.Types
--------------------------------------------

$(singletons [d|
  data HGitTag = BlobTag | DirTag | CommitTag
 |])

data HGit a i where
  -- file chunk bits
  -- NOTE: using String instead of Bytestring to allow for easy examination of serialized files
  --       (strings are representable in JSON, ByteStrings would need to be base64 encoded)
  Blob :: String -> HGit a 'BlobTag

  -- dir and file bits
  Dir :: [NamedFileTreeEntity a]
      -> HGit a 'DirTag

  -- commits
  Commit :: CommitMessage           -- todo: commit message as blob!
         -> a 'DirTag               -- root directory (itself unnamed)
         -> NonEmpty (a 'CommitTag) -- parent commits (at least one)
         -> HGit a 'CommitTag
  NullCommit :: HGit a 'CommitTag


-- | sort dir here by file name, order is irrelevant
canonicalOrdering :: [NamedFileTreeEntity a] -> [NamedFileTreeEntity a]
canonicalOrdering = sortOn fst

emptyDir :: forall x. HGit x 'DirTag
emptyDir = Dir []

dirEntries
  :: HGit x 'DirTag
  -> [NamedFileTreeEntity x]
dirEntries (Dir ns) = ns

data FileTreeEntity f
  = FileEntity (f 'BlobTag) -- a file
  | DirEntity  (f 'DirTag)       -- more directory structure

fte :: (f 'BlobTag -> a)
    -> (f 'DirTag       -> a)
    -> FileTreeEntity f
    -> a
fte f _ (FileEntity x) = f x
fte _ g (DirEntity  x) = g x

type NamedFileTreeEntity f
  = ( PartialFilePath -- name of this directory entry (files and dirs have same name rules)
    , FileTreeEntity f
    )

instance HFunctor HGit where
  hfmap _ (Blob fc)        = Blob fc
  hfmap f (Dir dcs)        = Dir $ fmap (fmap (fte (FileEntity . f) (DirEntity . f))) dcs
  hfmap f (Commit n rc nc) = Commit n (f rc) (fmap f nc)
  hfmap _  NullCommit      = NullCommit

instance HTraversable HGit where
  hmapM _ (Blob fc) = pure $ Blob fc
  hmapM nat (Dir dcs) = do
    let f (n, DirEntity dir)   = fmap ((n,) . DirEntity)  $ nat dir
        f (n, FileEntity file) = fmap ((n,) . FileEntity) $ nat file
    dcs' <- traverse f dcs
    pure $ Dir dcs'
  hmapM nat (Commit msg rc ncs) = do
    rc' <- nat rc
    ncs' <- traverse nat ncs
    pure $ Commit msg rc' ncs'
  hmapM _  NullCommit = pure NullCommit

instance Hashable HGit where
  hash :: HGit Hash :-> Hash
  -- special cases
  hash (Dir []) = emptyDirHash
  hash (NullCommit) = nullCommitHash

  -- file-type entities
  hash (Blob x) = doHash ["blob", unpackString x]

  -- non-empty dir-type entities
  hash (Dir xs) = doHash $ ["dir" :: ByteString] ++ join (fmap hashNFTE $ canonicalOrdering xs)
    where hashNFTE (name, f) = [unpackString name] ++ hashFTE f
          hashFTE =
            fte (\(chp :: Hash 'BlobTag) -> ["subfile", unpackHash chp])
                (\(chp :: Hash 'DirTag)  -> ["subdir", unpackHash chp])

  -- commit-type entities
  hash (Commit msg root parents)
    = doHash $
        [ unpackString msg
        , unpackHash root
        ] ++ toList (fmap unpackHash parents)

nullCommitHash :: Hash 'CommitTag
nullCommitHash = doHash [mempty]

emptyDirHash :: Hash 'DirTag
emptyDirHash = doHash [mempty]
