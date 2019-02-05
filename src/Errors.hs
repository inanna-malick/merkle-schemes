module Errors where

--------------------------------------------
import           Merkle.Types (Pointer(..))
--------------------------------------------

-- todo different name? hierarchy of errors?
-- | Errors that can occur while comparing two merkle trees
data MerkleTreeCompareError
  = HashValidationError -- domain precondition failed? idk weird should never happen
  | LookupError MerkleTreeLookupError
  deriving Show

data DomainError
  = CompareError MerkleTreeCompareError
  | InputError MerkleTreeLookupError
  | FileWriteError MerkleTreeLookupError -- from substantiating tree before writing to filesystem
  deriving Show

data MerkleTreeLookupError
  = EntityNotFoundInStore Pointer
  deriving Show

data FileReadError
  = FileReadError FilePath -- tried to read this path but failed (todo better errors? idk lol)
  deriving Show
