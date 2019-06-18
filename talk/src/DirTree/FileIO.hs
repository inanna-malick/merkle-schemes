module DirTree.FileIO where

--------------------------------------------
import qualified System.Directory as Dir
--------------------------------------------
import           Control.RecursionSchemes
import           DirTree.Types
--------------------------------------------

readTree
  :: FilePath
  -> IO DirTree
readTree = anaM alg
  where
    alg :: FilePath -> IO (DirLayer FilePath)
    alg path = do
          dirContents <- Dir.getDirectoryContents path
          let dirContents'
                = fmap (\x -> path ++ "/" ++ x)
                -- ignore all starting with '.' (eg ..,.,.hgit/)
                . filter (\fn -> take 1 fn /= ".")
                $ dirContents
          dirContents'' <- traverse categorize dirContents'
          pure $ DirLayer $ fmap (\(p,e) -> (justTheName p, e)) dirContents''

    categorize p = do
      isFile <- Dir.doesFileExist p
      if isFile
        then (justTheName p,) . FileEntity <$> readFile p
        else do
          isDir <- Dir.doesDirectoryExist p
          if isDir
            then pure $ (justTheName p, DirEntity p)
            else fail ("file read error: unexpected type at " ++ p)

-- | take just the name given a file path
justTheName :: FilePath -> String
justTheName = reverse . takeWhile (/= '/') . reverse
