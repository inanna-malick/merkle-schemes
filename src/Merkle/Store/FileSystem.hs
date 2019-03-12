module Merkle.Store.FileSystem where

--------------------------------------------
import qualified Data.Aeson as AE
import qualified Data.Aeson.Internal as AE
import qualified Data.Aeson.Parser as AE
import qualified Data.Aeson.Types as AE
import qualified Data.Aeson.Encoding as AE (encodingToLazyByteString)
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.Except
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Compose
import           Data.Singletons
--------------------------------------------
import           Errors
import           Merkle.Store
import           Merkle.Types
import           Util.HRecursionSchemes
import           Util.MyCompose
--------------------------------------------

-- | Filesystem backed store using the provided dir
fsStore
  :: forall m p
   . ( MonadIO m
     , MonadThrow m
     , HFunctor p
     )
  => p (Const HashPointer) :-> Const HashPointer             -- hash structure
  -> p (Const HashPointer) :-> Const AE.Value                -- encode json
  -> NatM AE.Parser (Const AE.Value) (p (Const HashPointer)) -- decode json
  -- the 'forall x' here enforces the invariant that exceptions can only contain empty structure!
  -- EG: the null commit is often represented using the hash '0' or some other special value
  -> (forall i x . SingI i => Const HashPointer i -> Maybe (p x i))
  -> FilePath
  -> Store m p
fsStore hash encode decode exceptions root
  = Store
  { sDeref = \p -> case exceptions p of
      Just exception -> pure exception
      Nothing -> handleDeref p
  , sUploadShallow = \x -> do
      let p = hash x
          fn = unHashPointer $ getConst p
      -- liftIO . putStrLn $ "upload thing that hashes to pointer " ++ show p ++ "to state store @ " ++ fn
      liftIO . BL.writeFile (root ++ "/" ++ fn)
             . AE.encodingToLazyByteString . AE.toEncoding
             $ encode x

      pure p
  }
  where
    handleDeref :: forall i
                 . SingI i
                => Const HashPointer i
                -> m $ p (Term (HashIndirect p)) i
    handleDeref (Const p) = do
      -- liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via fs state store @ " ++ fn
      contents <- liftIO $ B.readFile (root ++ "/" ++ unHashPointer p)
      case (AE.eitherDecodeStrictWith AE.json' (AE.iparse decode . Const) contents) of
        Left  e -> throw . DecodeError $ show e
        Right x -> do
          -- liftIO . putStrLn $ "got: " ++ (filter ('\\' /=) $ show contents)
          pure $ hfmap (Term . flip Pair (HC $ Compose $ Nothing)) x
