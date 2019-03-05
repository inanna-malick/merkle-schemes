module HGit.Types.Common where

type PartialFilePath = String
type BranchName      = String
type CommitMessage   = String
type FileChunk       = String



-- one-way function
-- (because I'm lazy and don't want or need to write a parser, no fundamental reason)
mkHashPointer :: Int -> HashPointer
mkHashPointer p = HashPointer $ prefix p ++ f (abs p)
  where
    prefix n | n > 0     = "x"
             | n < 0     = "y"
             | otherwise = "z"
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    base = length chars

    f n | n == 0 = ""
        | n < 0 = f $ (-1) * n -- increase risk of hash collisions here by 2x, but #YOLO
        | otherwise = chars !! (n `rem` base) : f (n `div` base)

newtype HashPointer = HashPointer { unHashPointer :: String }
  deriving (Eq, Ord)
instance Show HashPointer where
  show (HashPointer x) = "#[" ++ x ++ "]"
