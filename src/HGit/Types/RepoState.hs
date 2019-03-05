
module HGit.Types.RepoState (RepoState(..), initialRepoState) where

--------------------------------------------
import           Data.Aeson
import qualified Data.Functor.Compose as FC
import qualified Data.Map as M
import           Data.Vector (fromList, toList)
import           Data.Text (Text)
import           GHC.Generics
--------------------------------------------
import           HGit.Serialization
import           HGit.Types.Common
import           HGit.Types.Merkle
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
import           Util.MyCompose
--------------------------------------------


data RepoState
  = RepoState
  { branches      :: M.Map BranchName HashPointer
  , currentBranch :: BranchName
  -- NOTE: not yet used, will be req'd for laziness via tracking partially-fetched state
  , substantiated :: SubstantiationState
  } deriving (Generic)

initialRepoState :: RepoState
initialRepoState
  = RepoState
  { branches      = M.fromList [(initial, hash NullCommit)]
  , currentBranch = initial
  , substantiated = SubstantiationState emptyRoot
  }
  where
    emptyRoot = Dir []
    initial = "default"


instance ToJSON RepoState where
    toJSON (RepoState bs cb st) =
      object
      [ "branches"      .= (M.map unHashPointer bs)
      , "currentBranch" .= cb
      , "substantiated" .= st
      ]
instance FromJSON RepoState where
    parseJSON = withObject "RepoState" $ \o -> do
      bs <- o .: "branches"
      cb <- o .: "currentBranch"
      s  <- o .: "substantiated"
      pure $ RepoState (M.map HashPointer bs) cb s


-- for diffing: just traverse this and, for each substantiated path, ingest and compare file
-- note: this will end up being a big ass object if each file is substantiated - can't force
-- via type level but MUST ENFORCE that no file blobs are represented inline here, just dir structure
-- can grab file blobs from store if required for diffing
newtype SubstantiationState
  = SubstantiationState
  { unSubstantiationState :: HGit (Term (FC.Compose HashIndirect :++ HGit)) 'DirTag
  }

instance FromJSON SubstantiationState where
    parseJSON = withArray "array" (\a -> SubstantiationState . Dir . toList <$> traverse mkElem a)
      where
        mkElem v = decodeNamedDir handleDir handleFile v
        handleFile o = do
          p <- o .: "pointer"
          pure . Term . HC . FC.Compose $ C (HashPointer p, Nothing)
        handleDir o = do
          p <- o .:  "pointer"
          e <- o .:? "entity"
          pure . Term . HC . FC.Compose $ C (HashPointer p, unSubstantiationState <$> e)

instance ToJSON SubstantiationState where
    toJSON (SubstantiationState (Dir xs)) =
        Array . fromList $ encodeNamedDir handleDir handleFile <$> xs
      where
        -- throw away file contents - files don't get persisted here!
        -- NOTE: type signal required to suppress warnings (Term vs. Hole)
        handleFile
          :: forall i x y
           . Term (FC.Compose ((,) HashPointer :+ x) :++ y) i
          -> [(Text, Value)]
        handleFile (Term (HC (FC.Compose (C (p, _))))) = ["pointer" .= unHashPointer p]
        handleDir  (Term (HC (FC.Compose (C (p, Nothing))))) = ["pointer" .= unHashPointer p]
        handleDir  (Term (HC (FC.Compose (C (p, Just dir)))))
          = ["pointer" .= unHashPointer p
            ,"children"  .= SubstantiationState dir
            ]
