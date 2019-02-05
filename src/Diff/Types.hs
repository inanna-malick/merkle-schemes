module Diff.Types where

--------------------------------------------
import           Merkle.Tree.Types (Name)
--------------------------------------------

type FileBody = String

data Diff = LeafModified  (Name, FileBody, FileBody)
          | FileReplacedWithDir Name
          | DirReplacedWithFile Name
          | EntityAddedToDir
          | EntityRemovedFromDir
          | EntityRenamed Name Name
          | EntityDeleted Name
          | EntityCreated Name
  deriving (Show)
