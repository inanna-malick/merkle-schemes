module Merkle.Store.FileSystem where

--------------------------------------------
import           Control.Applicative (Const(..))
import           Control.Exception.Safe (MonadThrow, throwString)
import           Control.Monad.Except
import qualified Data.Aeson as AE
import           Data.Text (unpack)
import           System.Directory (doesFileExist)
--------------------------------------------
import           Merkle.Store
import           Merkle.Types
import           Merkle.Types.BlakeHash
--------------------------------------------

-- | Filesystem backed store using the provided dir
fsStore
  :: forall m f
   . ( MonadIO m
     , MonadThrow m
     , Functor f
     , Hashable RawBlakeHash f
     , AE.ToJSON1   f
     , AE.FromJSON1 f
     )
  => FilePath
  -> ShallowStore m RawBlakeHash f
fsStore root
  =
  ( GetCapabilityShallow $ \p -> do
      let fp = root ++ "/" ++ fn p
      exists <- liftIO $ doesFileExist fp
      if not exists
        then pure Nothing
        else liftIO $ AE.eitherDecodeFileStrict fp >>= \case
            -- throw if deserialization fails
            Left  e -> throwString e
            Right (HashTerm x) -> pure $ Just x

  , PutCapability $ \x -> do
      let p = hash x
      liftIO . AE.encodeFile (root ++ "/" ++ fn p)
             $ HashTerm x
      pure p
  )
  where
    fn :: Hash RawBlakeHash f -> String
    fn = unpack . hashToText . getConst
