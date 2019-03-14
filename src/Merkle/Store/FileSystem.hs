{-# LANGUAGE QuantifiedConstraints #-}

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
import           Data.Singletons
--------------------------------------------
import           Errors
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Types
import           Util.HRecursionSchemes
import           Util.MyCompose
--------------------------------------------

-- | Filesystem backed store using the provided dir
fsStore
  :: forall m f w
   . ( MonadIO m
     , MonadThrow m
     , HFunctor f
     , Hashable f
     , forall i. SingI i => AE.ToJSON (w i)
     , forall i. SingI i => AE.FromJSON (w i)
     )
  => Term (Tagged Hash :++ Indirect :++ f) :-> w
  -> w :-> Term (Tagged Hash :++ Indirect :++ f)
  -> (forall i x . SingI i => Hash i -> Maybe (f x i))
  -> FilePath
  -> Store m f
fsStore encode decode exceptions root
  = Store
  { sDeref = \p -> case exceptions p of
      Just exception -> pure exception
      Nothing -> handleDeref p
  , sUploadShallow = \x -> do
      let p = hash x
          fakeDeep :: f Hash :-> f (Term (Tagged Hash :++ Indirect :++ f))
          fakeDeep = hfmap f

      liftIO . BL.writeFile (root ++ "/" ++ fn p)
             . AE.encodingToLazyByteString . AE.toEncoding
             . encode . Term . HC . Tagged p
             . HC . Compose . Just $ fakeDeep x

      pure p
  }
  where
    f :: Hash :-> Term (Tagged Hash :++ Indirect :++ f)
    f p = Term . HC . Tagged p . HC $ Compose Nothing

    fn :: Hash i -> String
    fn = show . hashToText . getConst

    handleDeref :: forall i
                 . SingI i
                => Hash i
                -> m $ f (Term (Tagged Hash :++ Indirect :++ f)) i
    handleDeref p = do
      -- liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via fs state store @ " ++ fn
      contents <- liftIO $ B.readFile (root ++ "/" ++ fn p)
      case AE.eitherDecodeStrict contents of
        Left  e -> throw . DecodeError $ show e
        Right x -> case decode x of
          Term (HC (Tagged p' (HC (Compose (Just x')))))
            | p == p' -> pure x'
            | otherwise -> throw . DecodeError $ "hash mismatch (data corruption? lmao idk, here be dragons)"
          Term (HC (Tagged _ (HC (Compose Nothing)))) ->
            throw . DecodeError $ "should always have first layer of structure substantiated"
