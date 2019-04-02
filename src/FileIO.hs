-- | Functions for interacting with the filesystem to
-- create dir trees from merkle trees or vice versa
module FileIO where

--------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy
import qualified Data.List as List
import           Data.Foldable (traverse_)
import qualified System.Directory as Dir
--------------------------------------------
import           Util.RecursionSchemes
import           HGit.Types.HGit
--------------------------------------------
import           System.IO
import Data.Bitraversable (bitraverse)


-- | Write strict hgit dirtree to file path
writeTree
  :: forall m
   . MonadIO m
  => FilePath
  -> Fix (Dir (Fix Blob))
  -> m ()
writeTree outdir tree = evalStateT (cataM writeDir tree) [outdir]
  where
    writeFileChunk :: AlgebraM (StateT [FilePath] m) Blob ()
    writeFileChunk (Empty) = pure ()
    writeFileChunk (Chunk contents _) =
      gets (List.intercalate "/" . reverse) >>=  liftIO . flip appendFile contents

    writeDir :: AlgebraM (StateT [FilePath] m) (Dir (Fix Blob)) ()
    writeDir (Dir children) = flip traverse_ children $ \(pathChunk, e) -> do
      modify (push pathChunk)
      _ <- bitraverse (\fb -> touch >> cataM writeFileChunk fb) -- todo: error if file already exists
                      (\() -> mkDir) e
      modify pop

    mkDir = gets (List.intercalate "/" . reverse) >>= liftIO . Dir.createDirectory
    touch = gets (List.intercalate "/" . reverse) >>= liftIO . flip writeFile ""

    push x xs = x:xs
    pop (_:xs)  = xs
    pop []    = []



readTree
  :: FilePath
  -> IO (Fix (Dir (Fix Blob)))
readTree = anaM alg
  where
    alg :: CoAlgebraM IO (Dir (Fix Blob)) FilePath
    alg path = do
          dirContents <- Dir.getDirectoryContents path
          let dirContents'
                = fmap (\x -> path ++ "/" ++ x)
                -- ignore all starting with '.' (eg ..,.,.hgit/)
                . filter (\fn -> take 1 fn /= ".")
                $ dirContents
          dirContents'' <- traverse categorize dirContents'
          pure $ Dir $ fmap (\(p,e) -> (justTheName p, e)) dirContents''

    categorize p = do
      isFile <- Dir.doesFileExist p
      if isFile
        then (justTheName p,) . FileEntity <$> withFile p ReadMode readBlob
        else do
          isDir <- Dir.doesDirectoryExist p
          if isDir
            then pure $ (justTheName p, DirEntity p)
            else fail ("file read error: unexpected type at " ++ p)


justTheName :: FilePath -> String -- hacky hax but it works - take just the name given a file path
justTheName = reverse . takeWhile (/= '/') . reverse


readBlob
  :: Handle
  -> IO (Fix Blob)
readBlob = anaM alg
  where
    alg :: CoAlgebraM IO Blob Handle
    alg h = do
      isEof <- hIsEOF h
      if isEof then pure Empty
               else (flip Chunk h <$> hGetLine h) -- linear types would be nice here
