module DirTree.Diff.Types where


data Diff = FileModified
          | FileReplacedWithDir
          | DirReplacedWithFile
          | EntityDeleted
          | EntityCreated
  deriving (Eq, Show)
