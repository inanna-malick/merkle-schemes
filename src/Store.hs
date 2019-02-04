{-# LANGUAGE RankNTypes #-}


module Store where

import Merkle.Types
import Merkle.Tree.Types


--------------------------------------------
import qualified Data.Hashable as Hash
import           Data.HashMap.Strict (HashMap)
import           Data.IORef
--------------------------------------------


--------------------------------------------
import           Control.Monad.Except
import qualified Data.HashMap.Strict as Map
--------------------------------------------
import           Errors
--------------------------------------------

-- import qualified Database.Redis.IO as Redis


-- | Some capability to interact with a global store. Used to abstract
--   over multiple impls, eg redis vs. local ioref store for tests
data Store m
  = Store
  { -- return type being 'Concrete' instead of 'Shallow' allows possible optimization:
    -- returning multiple layers at once based on (eg) past usage patterns
    deref :: Pointer -> m ConcreteMerkleTreeLayer
    -- kinda gross - this _has_ to do hashing but w/e lol fuck it
  , uploadShallow :: ShallowMerkleTreeLayer -> m Pointer
  }


hoistStore :: (forall a. m1 a -> m2 a) -> Store m1 -> Store m2
hoistStore f gs
  = Store
  {
    deref = \p -> f $ deref gs p
  , uploadShallow = \smtl -> f $ uploadShallow gs smtl
  }

-- | Blah blah blah redis is bad - true, but also I can have it
--   running in docker in 2m and this is a toy project
-- redisStore :: IO (Store (ExceptT MerkleTreeLookupError IO))
-- redisStore = do
--   pool <- Redis.mkPool Redis.defSettings -- todo use bracket here
--   pure $
--   Store
--     { deref = \p -> do
--         liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via redis"
--         let key = Redis.Key $ undefined $ p
--         liftIO . Redis.runRedis $ Redis.get key
--         case x of
--           Nothing -> throwError $ EntityNotFoundInStore p
--           Just x  -> do
--             -- putStrLn $ "returning deref res: " ++ showT x
--             -- todo parse json
--             pure $ makeConcrete x
--     , uploadShallow = \smtl -> do
--         let pointer = Pointer $ Hash.hash smtl
--             key = Redis.Key $ undefined $ p
--             value = toJSON smtl
--         liftIO . Redis.runRedis $ Redis.set key value
--         pure pointer
--     }

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
