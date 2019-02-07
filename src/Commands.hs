
module Commands (MerkleDiffOpts(..), Command(..), parse)where

import Options.Applicative
import Data.Semigroup ((<>))

import Merkle.Tree.Types


parse :: IO MerkleDiffOpts
parse = execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

data MerkleDiffOpts
  =  MerkleDiffOpts
  { storePath  :: FilePath
  , commandOpt :: Command
  }

-- todo records for each?
data Command
  = Put  FilePath -- Pointer
  | Get  Pointer (Maybe FilePath) -- write to temp file if Nothing
  | Diff Pointer Pointer
  | Demo -- run old main
  | Find Pointer String -- todo: find first and terminate? or keep going?


fileOpt :: String -> Char -> String -> String -> Parser FilePath
fileOpt l s h m = strOption
  (  long l
  <> short s
  <> metavar m
  <> help h)


fileInput, fileOutput, storeDir :: Parser FilePath
fileInput  = fileOpt "file" 'f' "Input File" "FILENAME"
fileOutput = fileOpt "file" 'f' "Output File" "FILENAME"
storeDir   = fileOpt "store" 's' "Store Path" "STORE_PATH"


pointerInput :: String -> Char -> String -> Parser Pointer
pointerInput m s l = Pointer <$> option auto
            ( long l
           <> short s
           <> metavar m
           <> help "Output the last K lines" )


parser :: Parser MerkleDiffOpts
parser
  = MerkleDiffOpts
      <$>
       storeDir
      <*>
       subparser
       ( command "put"  (info putOptions  ( progDesc "read a dir tree from the fs and upload it to the repo" ))
      <> command "get"  (info getOptions  ( progDesc "read a tree from the repo and write it to the fs" ))
      <> command "diff" (info diffOptions  ( progDesc "diff two merkle trees" ))
      <> command "demo" (info (pure Demo)  ( progDesc "run random demo thingy" ))
      <> command "find" (info findOptions  ( progDesc "lazily search a merkle tree" ))
       )
  where
    putOptions  = Put  <$> fileInput
    getOptions  = Get  <$> pointerInput "P" 'p' "pointer" <*> optional fileOutput
    findOptions = Find <$> pointerInput "P" 'p' "pointer" <*> fileInput
    diffOptions = Diff
              <$> pointerInput "BEFORE" 'b' "before"
              <*> pointerInput "AFTER"  'a'  "after"
