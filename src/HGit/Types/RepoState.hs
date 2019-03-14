
module HGit.Types.RepoState (RepoState(..), initialRepoState) where

--------------------------------------------
import           Data.Aeson
import qualified Data.Map as M
import           GHC.Generics
--------------------------------------------
import           HGit.Types.Common
import           HGit.Types.HGit
import           Merkle.Types (Hash)
--------------------------------------------


data RepoState
  = RepoState
  { branches      :: M.Map BranchName (Hash 'CommitTag)
  , currentBranch :: BranchName
  } deriving (Generic)

initialRepoState :: RepoState
initialRepoState
  = RepoState
  { branches      = M.fromList [(initial, nullCommitHash)]
  , currentBranch = initial
  }
  where
    initial = "default"
instance ToJSON RepoState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RepoState
