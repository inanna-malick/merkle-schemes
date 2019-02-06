module Store where

--------------------------------------------
import qualified Data.Aeson as AE
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Hashable as Hash
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.IORef
import           System.Directory (getTemporaryDirectory, createDirectory)
import           System.Random (randomIO)
--------------------------------------------
import           Errors
import           Merkle.Types
import           Merkle.Tree.Types
import           Util.MyCompose
import           Util.RecursionSchemes
--------------------------------------------


-- | Some capability to interact with some hash addressed merkle tree store
data Store m
  = Store
  { -- | given a pointer, fetch the corresponding entity. Provides type-level guarantee that
    --   at least one level of structure is fetched '(Named :+ Tree)' while allowing for multiple
    --   levels of structure to be returned in one call via 'MerkleTree' subnode type
    sDeref :: Pointer -> m $ Named :+ Tree $ LazyMerkleTree
    -- | given a shallow layer of structure with subnodes identified by a pointer, store it.
    -- this allows for each store to use its own hash algorithm - not sure if I like that
  , sUploadShallow :: Named :+ Tree $ Pointer -> m Pointer
  }

-- todo this should really be bracket pattern for cleanup
createTmpDir :: String -> IO FilePath
createTmpDir prefix = do
  sysTmp <- getTemporaryDirectory
  x <- randomIO -- collision detection? lmao no,lol  #YOLO
  let dir = sysTmp ++ "/" ++ prefix ++ show (x :: Int)
  createDirectory dir
  putStrLn $ "created temp dir: " ++ dir
  pure dir


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



-- todo own file
type GlobalStore = IORef (HashMap Pointer (Named :+ Tree $ LazyMerkleTree))

-- | Store backed by in-memory IORef HashMap
iorefStore :: IORef $ HashMap Pointer $ Named :+ Tree $ Pointer
           -> Store $ ExceptT MerkleTreeLookupError IO
iorefStore ioref
  = Store
  { sDeref = \p -> do
      store <- liftIO $ readIORef ioref
      liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via global state store"
      case Map.lookup p store of
        Nothing -> throwError $ EntityNotFoundInStore p
        Just x  -> do
          -- putStrLn $ "returning deref res: " ++ showT x
          pure $ makeConcrete x
  , sUploadShallow = \smtl -> do
      let p = Pointer $ Hash.hash $ SMTL smtl
      liftIO $ modifyIORef' ioref (Map.insert p smtl)
      pure p
  }

-- | consume effectful tree, annotate nodes with hash,
--   adds them to some global store during this traversal
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
addTreeToStore
  :: forall m
   . Monad m
  => Store m
  -> Fix (Named :+ Tree)
  -> m $ Fix $ WithHash :+ (Named :+ Tree)
addTreeToStore store = cata alg
  where
    alg :: Algebra (Named :+ Tree) (m $ Fix $ WithHash :+ Named :+ Tree)
    alg (C (name, entity')) = do
      entity <- (name,) <$> case entity' of
        Leaf body -> pure $ Leaf body
        Node children -> do
          children' <- traverse id children
          pure $ Node children'
      p <- sUploadShallow store . fmap pointer $ C entity
      pure . Fix . C . (p,) $ C entity
