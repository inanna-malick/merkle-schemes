module DirTree.FileStore where

--------------------------------------------
import qualified Data.Aeson as AE
import           Data.Text (unpack)
--------------------------------------------
import           Control.RecursionSchemes
import           DirTree.Types
import           DirTree.Hashing
--------------------------------------------

-- | write a single directory layer to a file system store
writeLayer :: FilePath -> DirLayer Hash -> IO Hash
writeLayer baseDir layer = do
  let h = hashLayer layer
      path = hashToPath baseDir h
  AE.encodeFile path layer -- write dir layer to path
  pure h

-- | write a directory tree to a filesystem store
writeDirTree :: FilePath -> DirTree -> IO Hash
writeDirTree baseDir = cataM (writeLayer baseDir)


-- | read a single directory layer from a file system store
readLayer :: FilePath -> Hash -> IO (DirLayer Hash)
readLayer baseDir h = do
  res <- AE.decodeFileStrict (hashToPath baseDir h)
  case res of
    Nothing -> fail "hash deref failed"
    Just  x -> pure x

-- | read a directory tree to a filesystem store
readDirTree :: FilePath -> Hash -> IO DirTree
readDirTree baseDir = anaM (readLayer baseDir)

-- | generate a file path given some base directory and a hash
hashToPath :: FilePath -> Hash -> FilePath
hashToPath baseDir = (baseDir ++) . ("/" ++) . unpack . hashToText
