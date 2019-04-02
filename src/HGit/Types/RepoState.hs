
module HGit.Types.RepoState (RepoState(..), initialRepoState) where

--------------------------------------------
import           Data.Aeson
import qualified Data.Map as M
import           GHC.Generics
--------------------------------------------
import           HGit.Types.HGit
import           Merkle.Types (Hash, emptyHash)
--------------------------------------------


data RepoState
  = RepoState
  { branches      :: M.Map BranchName (Hash (Commit (Hash (Dir (Hash Blob)))))
  , currentBranch :: BranchName
  , remote        :: Maybe (String, Int) -- optional host and port for remote (single, #YOLO)
  } deriving (Generic)

initialRepoState :: RepoState
initialRepoState
  = RepoState
  { branches      = M.fromList [(initial, emptyHash)]
  , currentBranch = initial
  , remote        = Nothing
  }
  where
    initial = "default"
instance ToJSON RepoState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RepoState
