module Store.FileSystem where

--------------------------------------------
import qualified Data.Aeson as AE
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Hashable as Hash
import           System.Directory (getTemporaryDirectory, createDirectory)
import           System.Random (randomIO)
--------------------------------------------
import           Errors
import           Merkle.Tree.Types
import           Merkle.Tree.Encoding
import           Util.MyCompose
import           Store.Capability
--------------------------------------------

-- | Filesystem backed store using a temp dir
tmpFsStore :: IO $ Store $ ExceptT MerkleTreeLookupError IO
tmpFsStore = do
  dir <- createTmpDir "merklestore"
  pure $ fsStore dir

-- | Filesystem backed store using the provided dir
fsStore :: FilePath -> Store $ ExceptT MerkleTreeLookupError IO
fsStore root
  = Store
  { sDeref = \p -> do
      liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via fs state store"
      contents <- liftIO $ B.readFile (root ++ "/" ++ f p)
      -- liftIO . putStrLn $ "returning deref res via fs state store: " ++ show contents
      case AE.decode contents of
        Nothing -> throwError $ EntityNotFoundInStore p
        Just x  -> do
          pure $ makeConcrete $ unSMTL x
  , sUploadShallow = \smtl -> do
      let p = Pointer $ Hash.hash $ SMTL smtl
      liftIO $ B.writeFile (root ++ "/" ++ f p) (AE.encode $ SMTL smtl)
      pure p
  }
  where
    -- todo something cooler and more human-readable eg [a..z]
    f p = "pointer_" ++ show (unPointer p)

-- todo this should really be bracket pattern for cleanup
createTmpDir :: String -> IO FilePath
createTmpDir prefix = do
  sysTmp <- getTemporaryDirectory
  x <- randomIO -- collision detection? lmao no,lol  #YOLO
  let dir = sysTmp ++ "/" ++ prefix ++ show (x :: Int)
  createDirectory dir
  putStrLn $ "created temp dir: " ++ dir
  pure dir
