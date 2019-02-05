{-# LANGUAGE RankNTypes #-}


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
--------------------------------------------


-- | Some capability to interact with a global store. Used to abstract
--   over multiple impls, eg redis vs. local ioref store for tests
data Store m
  = Store
  { -- return type being 'Concrete' instead of 'Shallow' allows possible optimization:
    -- returning multiple layers at once based on (eg) past usage patterns
    deref :: Pointer -> m ConcreteMerkleTreeLayer
    -- this allows for each store to use its own hash algorithm - not sure if I like that
  , uploadShallow :: ShallowMerkleTreeLayer -> m Pointer
  }


hoistStore :: (forall a. m1 a -> m2 a) -> Store m1 -> Store m2
hoistStore f gs
  = Store
  { deref = \p -> f $ deref gs p
  , uploadShallow = \smtl -> f $ uploadShallow gs smtl
  }


tmpFsStore :: IO (Store (ExceptT MerkleTreeLookupError IO))
tmpFsStore = do
  sysTmp <- getTemporaryDirectory
  x <- randomIO
  let dir = sysTmp ++ "/merklestore" ++ show (x :: Int)
  createDirectory dir
  putStrLn $ "using store: " ++ dir
  pure $ fsStore dir

fsStore :: FilePath -> Store (ExceptT MerkleTreeLookupError IO)
fsStore root
  = Store
  { deref = \p -> do
      liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via fs state store"
      contents <- liftIO $ B.readFile (root ++ "/" ++ f p)
      case AE.decode contents of
        Nothing -> throwError $ EntityNotFoundInStore p
        Just x  -> do
          -- putStrLn $ "returning deref res via fs state store: " ++ ??? x
          pure $ makeConcrete x
  , uploadShallow = \smtl -> do
      let p = Pointer $ Hash.hash smtl
          -- todo something cooler
      liftIO $ B.writeFile (root ++ "/" ++ f p) (AE.encode smtl)
      pure p
  }
  where
    f p = "pointer_" ++ show (unPointer p)



iorefStore :: IORef (HashMap Pointer ShallowMerkleTreeLayer)
           -> Store (ExceptT MerkleTreeLookupError IO)
iorefStore ioref
  = Store
  { deref = \p -> do
      store <- liftIO $ readIORef ioref
      liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via global state store"
      case Map.lookup p store of
        Nothing -> throwError $ EntityNotFoundInStore p
        Just x  -> do
          -- putStrLn $ "returning deref res: " ++ showT x
          pure $ makeConcrete x
  , uploadShallow = \smtl -> do
      let pointer = Pointer $ Hash.hash smtl
      liftIO $ modifyIORef' ioref (Map.insert pointer smtl)
      pure pointer
  }
