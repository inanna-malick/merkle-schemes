module Merkle.Store.FileSystem where

--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.Except
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE (encodingToLazyByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Compose
import           Data.Functor.Const
import           System.Directory (doesFileExist)
--------------------------------------------
import           Errors
import           Merkle.Store
import           Merkle.Types
import           Util.RecursionSchemes
--------------------------------------------

-- | Filesystem backed store using the provided dir
fsStore
  :: forall m f
   . ( MonadIO m
     , MonadThrow m
     , Functor f
     , Hashable f
     , AE.ToJSON1   f
     , AE.FromJSON1 f
     )
  => FilePath
  -> Store m f
fsStore root
  = Store
  { sDeref = \p -> do
      let fp = root ++ "/" ++ fn p
      exists <- liftIO $ doesFileExist fp
      if not exists
        then pure Nothing
        else do
          contents <- liftIO $ B.readFile fp
          case AE.eitherDecodeStrict contents of
            -- throw if deserialization fails
            Left  e -> throw . DecodeError $ show e
            Right (HashTerm x) -> pure . Just $ fmap (\p' -> Fix $ Compose (p', Compose Nothing)) x

  , sUploadShallow = \x -> do
      let p = hash x
      liftIO . BL.writeFile (root ++ "/" ++ fn p)
             . AE.encodingToLazyByteString
             . AE.toEncoding
             $ HashTerm x
      pure p
  }
  where
    fn :: Hash i -> String
    fn = show . hashToText . getConst
