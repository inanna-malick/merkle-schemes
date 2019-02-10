module Store.InMemory where

--------------------------------------------
import           Control.Monad.Except
import qualified Data.Hashable as Hash
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IORef
--------------------------------------------
import           Errors
import           Merkle.Tree.Types
import           Merkle.Tree.Encoding
import           Util.MyCompose
import           Store.Capability
--------------------------------------------


-- | Store backed by in-memory IORef HashMap
iorefStore :: IORef $ Map Pointer $ Named :+ Tree $ Pointer
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
