module Errors where

--------------------------------------------
import           Control.Exception (Exception)
--------------------------------------------
import           Merkle.Types (RawHash)
import           HGit.Types.HGit (BranchName)
--------------------------------------------

data LookupError
  = LookupError RawHash
  deriving Show

instance Exception LookupError

data RepoStateError
  = DecodeError String
  | BranchNotFound BranchName
  | RemoteNotFound
  deriving Show

instance Exception RepoStateError

data FileReadError
  = FileReadError FilePath -- tried to read this path but failed (todo better errors? idk lol)
  deriving Show
