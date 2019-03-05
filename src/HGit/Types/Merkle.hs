module HGit.Types.Merkle where

--------------------------------------------
import           Data.Functor.Const
import qualified Data.Functor.Compose as FC
import           Data.Singletons.TH
--------------------------------------------
import           HGit.Types.Common
import           Util.MyCompose
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------

$(singletons [d|
  data HGitTag = FileChunkTag | DirTag | CommitTag
 |])

data FileTreeEntity f
  = FileEntity (f 'FileChunkTag) -- a file (named blobtree)
  | DirEntity  (f 'DirTag)       -- more directory structure

fte :: (f 'FileChunkTag -> a)
    -> (f 'DirTag       -> a)
    -> FileTreeEntity f
    -> a
fte f _ (FileEntity x) = f x
fte _ g (DirEntity  x) = g x

type NamedFileTreeEntity f
  = ( PartialFilePath -- name of this directory entry (files and dirs have same name rules)
    , FileTreeEntity f
    )

data HGit a i where
  -- file chunk bits
  Blob :: FileChunk -> HGit a 'FileChunkTag
  BlobTree :: [a 'FileChunkTag] -> HGit a 'FileChunkTag

  -- dir and file bits
  -- TODO: dedicated sum type for file type branches - could use to represent (eg) symlinks or w/e
  Dir :: [NamedFileTreeEntity a]
      -> HGit a 'DirTag

  -- commits
  Commit :: CommitMessage
         -> a 'DirTag         -- root directory (itself unnamed)
         -> a 'CommitTag      -- previous commit
         -> HGit a 'CommitTag
  NullCommit :: HGit a 'CommitTag

emptyDir :: forall x. HGit x 'DirTag
emptyDir = Dir []

dirEntries
  :: HGit (Term (FC.Compose (LazyHashTagged m) :++ HGit)) 'DirTag
  -> [NamedFileTreeEntity (Term (FC.Compose (LazyHashTagged m) :++ HGit))]
dirEntries (Dir ns) = ns

instance HFunctor HGit where
  hfmap _ (Blob fc)        = Blob fc
  hfmap f (BlobTree fcs)   = BlobTree $ fmap f fcs
  hfmap f (Dir dcs)        = Dir $ fmap (fmap (fte (FileEntity . f) (DirEntity . f))) dcs
  hfmap f (Commit n rc nc) = Commit n (f rc) (f nc)
  hfmap _  NullCommit      = NullCommit


instance HTraversable HGit where
  hmapM _ (Blob fc) = pure $ Blob fc
  hmapM nat (BlobTree fcs) = do
    fcs' <- traverse nat fcs
    pure $ BlobTree fcs'
  hmapM nat (Dir dcs) = do
    let f (n, DirEntity dir)   = fmap ((n,) . DirEntity)  $ nat dir
        f (n, FileEntity file) = fmap ((n,) . FileEntity) $ nat file
    dcs' <- traverse f dcs
    pure $ Dir dcs'
  hmapM nat (Commit msg rc nc) = do
    rc' <- nat rc
    nc' <- nat nc
    pure $ Commit msg rc' nc'
  hmapM _  NullCommit = pure NullCommit


instance SHFunctor HGit where
  shfmap _ (Blob fc)        = Blob fc
  shfmap f (BlobTree fcs)   = BlobTree $ fmap f fcs
  shfmap f (Dir dcs)        = Dir $ fmap (fmap (fte (FileEntity . f) (DirEntity . f))) dcs
  shfmap f (Commit n rc nc) = Commit n (f rc) (f nc)
  shfmap _  NullCommit      = NullCommit

type HashIndirect = (,) HashPointer :+ Maybe
type LazyHashTagged m = (,) HashPointer :+ m

pointer :: forall f i x. Term (FC.Compose ((,) HashPointer :+ x) :++ f) i -> HashPointer
pointer (Term (HC (FC.Compose (C (p, _))))) = p

pointer' :: forall f x . Term (FC.Compose ((,) HashPointer :+ x) :++ f) :-> Const HashPointer
pointer' = Const . pointer
