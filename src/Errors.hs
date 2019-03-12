module Errors where

--------------------------------------------
import           Control.Exception (Exception)
--------------------------------------------
import           Merkle.Types (HashPointer)
import           HGit.Types   (BranchName)
--------------------------------------------

data MerkleTreeLookupError
  = EntityNotFoundInStore HashPointer
  deriving Show

instance Exception MerkleTreeLookupError


data RepoStateError
  = DecodeError String
  | BranchNotFound BranchName
  deriving Show

instance Exception RepoStateError

data FileReadError
  = FileReadError FilePath -- tried to read this path but failed (todo better errors? idk lol)
  deriving Show
