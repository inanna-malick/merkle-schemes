module HGit.Store.FileSystem where

--------------------------------------------
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE (encodingToLazyByteString)

import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Functor.Compose as FC
import           Data.Functor.Const (Const(..))
import           Data.Singletons
--------------------------------------------
import           Errors
import           HGit.Serialization
import           HGit.Store
import           HGit.Types
import           Util.HRecursionSchemes
import           Util.MyCompose
--------------------------------------------

-- | Filesystem backed store using the provided dir
fsStore
  :: forall m
   . MonadIO m
  => MonadThrow m
  => FilePath
  -> Store m HGit
fsStore root
  = Store
  { sDeref = handleDeref'
  , sUploadShallow = \x -> do
      let bytes = AE.encodingToLazyByteString . AE.toEncoding $ sencode x
          p = hash' x
          fn = f $ getConst p
      -- liftIO . putStrLn $ "upload thing that hashes to pointer " ++ show p ++ "to state store @ " ++ fn
      liftIO $ B.writeFile (root ++ "/" ++ fn) bytes
      pure p
  }
  where
    -- TODO: layered store architecture,
    -- TODO: 'withFallback' structure supporting in-memory cache, filesystem, remote, etc
    -- null commit and empty dir are always in store
    -- TODO: make write path ignore these too
    handleDeref' :: forall i
                 . SingI i
                => Const HashPointer i
                -> m $ HGit (Term (FC.Compose HashIndirect :++ HGit)) i
    handleDeref' p = case (sing @i) of
          SCommitTag ->
            if (p == hash' NullCommit)
              then pure NullCommit
              else handleDeref p
          SDirTag    ->
            if (p == hash' (Dir []))
              then pure (Dir [])
              else handleDeref p
          _ -> handleDeref p

    handleDeref :: forall i
                 . SingI i
                => Const HashPointer i
                -> m $ HGit (Term (FC.Compose HashIndirect :++ HGit)) i
    handleDeref (Const p) = do
      let fn = f p
      -- liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via fs state store @ " ++ fn
      contents <- liftIO $ B.readFile (root ++ "/" ++ fn)
      case (AE.eitherDecode contents) of
        Left  e -> throw $ DecodeError e
        Right (HGitConst x) -> do
          -- liftIO . putStrLn $ "got: " ++ (filter ('\\' /=) $ show contents)
          pure $ hfmap (\(Const p') -> Term $ HC $ FC.Compose $ C (p', Nothing)) x

    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    base = length chars

    f n | n == 0 = ""
        | n < 0 = f $ (-1) * n -- increase risk of hash collisions here by 2x, but #YOLO
        | otherwise = chars !! (n `rem` base) : f (n `div` base)
