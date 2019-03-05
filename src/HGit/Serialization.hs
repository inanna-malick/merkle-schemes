{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module HGit.Serialization where

--------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Functor.Const
import qualified Data.Hashable as H
import           Data.Singletons
import           Data.Text
--------------------------------------------
import           HGit.Types.Common
import           HGit.Types.Merkle
import           Util.HRecursionSchemes ((:->)) -- YOLO 420 SHINY AND CHROME
import           Util.MyCompose
--------------------------------------------

instance SingI x => FromJSON (HGitConst x) where
  parseJSON = fmap HGitConst <$> sdecode sing

newtype HGitConst i = HGitConst { unHGitConst :: HGit (Const HashPointer) i}

sdecode :: Sing x -> Value -> Parser $ HGit (Const HashPointer) x
sdecode = \case
  SFileChunkTag -> withObject "HGit (Const HashPointer) FileChunkTag" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "blobtree" -> do
              children <- v .: "children"
              pure $ BlobTree $ fmap (Const . HashPointer) children
          "blob" -> do
              contents <- v .: "contents"
              pure $ Blob contents
          x -> fail $ "require [blob, blobtree] type" ++ x

  SDirTag       -> withObject "HGit (Const HashPointer) DirTag" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "dir" -> do
              children  <- v .: "children"
              children' <- traverse parseThingy children
              pure $ Dir children'
          x -> fail $ "require [file, dir] type" ++ x

  SCommitTag    -> withObject "HGit (Const HashPointer) CommitTag" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "nullcommit" -> pure NullCommit
          "commit" -> do
              name <- v .: "name"
              root <- v .: "root"
              prev <- v .: "prev"
              pure $ Commit name (Const $ HashPointer root) (fmap (Const . HashPointer) prev)
          x -> fail $ "require [commit, nullcommit] type" ++ x

  where
    parseThingy
      :: Value
      -> Parser $ NamedFileTreeEntity (Const HashPointer)
    parseThingy = decodeNamedDir -- both branches just Const pointers
      (fmap (Const . HashPointer) . (.: "pointer")) (fmap (Const . HashPointer) . (.: "pointer"))

encodeNamedDir
  :: (f 'DirTag       -> [(Text, Value)])
  -> (f 'FileChunkTag -> [(Text, Value)])
  -> NamedFileTreeEntity f
  -> Value
encodeNamedDir ed ef (path, e)
  = object $
  [ "path" .= path
  ] ++ case e of
        DirEntity  dir  -> ["type" .= ("dir" :: Text)] ++ ed dir
        FileEntity file -> ["type" .= ("file" :: Text)] ++ ef file

decodeNamedDir
  :: (Object -> Parser (f 'DirTag))
  -> (Object -> Parser (f 'FileChunkTag))
  -> Value
  -> Parser (NamedFileTreeEntity f) -- lmao, need new type
decodeNamedDir pd pf
  =  withObject "named dir entity pointer thingy" $ \v -> do
        name    <- v .: "path"
        typ     <- v .: "type"
        case typ of
          "dir"  -> do
            e <- pd v
            pure (name, DirEntity e)
          "file" -> do
            e <- pf v
            pure (name, FileEntity e)
          x      -> fail $ "require [file, dir] type" ++ x

sencode :: HGit (Const HashPointer) x -> Value
sencode = \case
    Blob contents ->
        object [ "type"     .= ("blob" :: Text)
               , "contents" .= pack contents
               ]
    BlobTree children ->
        object [ "type"     .= ("blobtree" :: Text)
               , "children" .= fmap (unHashPointer . getConst) children
               ]

    Dir children ->
        object [ "type" .= ("dir" :: Text)
               , "children" .= fmap mkThingy children
               ]

    Commit name root prev ->
        object [ "type" .= ("commit" :: Text)
               , "name" .= pack name
               , "root" .= (unHashPointer $ getConst root)
               , "prev" .= fmap (unHashPointer . getConst) prev
               ]
    NullCommit ->
        object [ "type" .= ("nullcommit" :: Text)
               ]

  where
    mkThingy :: NamedFileTreeEntity (Const HashPointer) -> Value
    mkThingy = encodeNamedDir (pure . ("pointer" .=) . unHashPointer . getConst)
                              (pure . ("pointer" .=) . unHashPointer . getConst)


hash :: HGit (Const HashPointer) x -> HashPointer
hash = mkHashPointer . H.hash . sencode


hash' :: HGit (Const HashPointer) :-> Const HashPointer
hash' = Const . hash


emptyDirHash :: Const HashPointer 'DirTag
emptyDirHash = hash' emptyDir
