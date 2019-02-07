module Errors (MerkleTreeLookupError(..), FileReadError(..))where

--------------------------------------------
import           Merkle.Tree.Types (Pointer(..))
--------------------------------------------

data MerkleTreeLookupError
  = EntityNotFoundInStore Pointer
  deriving Show

data FileReadError
  = FileReadError FilePath -- tried to read this path but failed (todo better errors? idk lol)
  deriving Show
