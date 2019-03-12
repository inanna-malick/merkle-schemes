{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module HGit.Serialization where

--------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Functor.Compose
import qualified Data.List.NonEmpty as NEL
import qualified Data.Hashable as H
import           Data.Singletons
import           Data.Text
import           Data.Vector (fromList, toList)
--------------------------------------------
import           HGit.Types.HGit
import           Merkle.Types
import           Util.HRecursionSchemes
import           Util.MyCompose
--------------------------------------------


sdecode :: NatM Parser (Const Value) (HGit (Const HashPointer))
sdecode = sdecode' sing . getConst

sdecode' :: Sing x -> Value -> Parser $ HGit (Const HashPointer) x
sdecode' = \case
  SBlobTag -> withObject "HGit (Const HashPointer) BlobTag" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "blob" -> do
              contents <- v .: "contents"
              pure $ Blob contents
          x -> fail $ "require [blob] type" ++ x

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
              parents <- v .: "parents"
              pure $ Commit name (Const $ HashPointer root) (fmap (Const . HashPointer) parents)
          x -> fail $ "require [commit, nullcommit] type" ++ x

  where
    parseThingy
      :: Value
      -> Parser $ NamedFileTreeEntity (Const HashPointer)
    parseThingy = decodeNamedDir -- both branches just Const pointers
      (fmap (Const . HashPointer) . (.: "pointer")) (fmap (Const . HashPointer) . (.: "pointer"))

encodeNamedDir
  :: (f 'BlobTag -> [(Text, Value)])
  -> (f 'DirTag       -> [(Text, Value)])
  -> NamedFileTreeEntity f
  -> Value
encodeNamedDir ef ed (path, e) = object $
  [ "path" .= path
  ] ++ case e of
        DirEntity  dir  -> ["type" .= ("dir" :: Text)] ++ ed dir
        FileEntity file -> ["type" .= ("file" :: Text)] ++ ef file

decodeNamedDir
  :: (Object -> Parser (f 'DirTag))
  -> (Object -> Parser (f 'BlobTag))
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

sencode :: HGit (Const HashPointer) :-> Const Value
sencode x =  Const $ case x of
    Blob contents ->
        object [ "type"     .= ("blob" :: Text)
               , "contents" .= pack contents
               ]

    Dir children ->
        object [ "type" .= ("dir" :: Text)
               , "children" .= fmap mkThingy children
               ]

    Commit name root parents ->
        object [ "type" .= ("commit" :: Text)
               , "name" .= pack name
               , "root" .= (unHashPointer $ getConst root)
               , "parents" .= fmap (unHashPointer . getConst) parents
               ]
    NullCommit ->
        object [ "type" .= ("nullcommit" :: Text)
               ]
  where
    mkThingy :: NamedFileTreeEntity (Const HashPointer) -> Value
    mkThingy = encodeNamedDir (pure . ("pointer" .=) . unHashPointer . getConst)
                              (pure . ("pointer" .=) . unHashPointer . getConst)




-- TODO: move this? yes.
structuralHash :: HGit (Const HashPointer) :-> Const HashPointer
-- special cases
structuralHash (Dir []) = emptyDirHash
structuralHash (NullCommit) = nullCommitHash

-- file-type entities
structuralHash (Blob x) = Const $ mkHashPointer $ H.hash x

-- non-empty dir-type entities
structuralHash (Dir xs) = Const $ mkHashPointer $ H.hash $ fmap hashNFTE xs
  where hashNFTE (name, f) = H.hash name `H.hashWithSalt` hashFTE f
        hashFTE =
          fte (\chp -> H.hash ("file" :: Text) `H.hashWithSalt` H.hash chp)
              (\chp -> H.hash ("dir"  :: Text) `H.hashWithSalt` H.hash chp)

-- commit-type entities
structuralHash (Commit msg root parents)
  = Const $ mkHashPointer $ H.hash
  [ H.hash msg
  , H.hash root
  , H.hash parents
  ]

-- | Compare two hash indirect structures, returning True only if both are exactly equal
--   (eg not just equal via pointer comparison, all substantiations and list orderings must be identical)
eqStructuralUgh :: forall i . Term (HGit) i -> Term (HGit) i -> Bool
eqStructuralUgh (Term me1) (Term me2) = case (me1, me2) of
    ((Blob b1), (Blob b2)) -> b1 == b2
    (NullCommit, NullCommit) -> True
    ((Commit msg1 root1 parents1), (Commit msg2 root2 parents2)) ->
      msg1 == msg2
        && eqStructuralUgh root1 root2
        && ( Prelude.foldl (&&) True $ NEL.zipWith eqStructuralUgh parents1 parents2
           )
    ((Dir xs1), (Dir xs2)) -> Prelude.foldl (&&) True $ Prelude.zipWith eqFTE xs1 xs2
    (_, _) -> False

  where
    eqFTE (n1, DirEntity d1) (n2, DirEntity d2) = n1 == n2 && eqStructuralUgh d1 d2
    eqFTE (n1, FileEntity f1) (n2, FileEntity f2) = n1 == n2 && eqStructuralUgh f1 f2
    eqFTE _ _ = False


-- shit name, investigate Eq1 and such
newtype HashTaggedNT i = HashTaggedNT (Term (HashTagged HGit) i)
instance Show (HashTaggedNT i) where
  show _ = "todo"
instance SingI i => Eq (HashTaggedNT i) where
  (HashTaggedNT a) == (HashTaggedNT b) = eqStructural a b

-- | Compare two hash indirect structures, returning True only if both are exactly equal
--   (eg not just equal via pointer comparison, all substantiations and list orderings must be identical)
eqStructural :: forall i . Term (HashTagged HGit) i -> Term (HashTagged HGit) i -> Bool
eqStructural (Term (Pair p1 (me1))) (Term (Pair p2 (me2))) =
  p1 == p2 && case (me1, me2) of
    ((Blob b1), (Blob b2)) -> b1 == b2
    (NullCommit, NullCommit) -> True
    ((Commit msg1 root1 parents1), (Commit msg2 root2 parents2)) ->
      msg1 == msg2
        && eqStructural root1 root2
        && ( Prelude.foldl (&&) True $ NEL.zipWith eqStructural parents1 parents2
           )
    ((Dir xs1), (Dir xs2)) -> Prelude.foldl (&&) True $ Prelude.zipWith eqFTE xs1 xs2
    (_, _) -> False

  where
    eqFTE (n1, DirEntity d1) (n2, DirEntity d2) = n1 == n2 && eqStructural d1 d2
    eqFTE (n1, FileEntity f1) (n2, FileEntity f2) = n1 == n2 && eqStructural f1 f2
    eqFTE _ _ = False




-- | Compare two hash indirect structures, returning True only if both are exactly equal
--   (eg not just equal via pointer comparison, all substantiations and list orderings must be identical)
eqStructural' :: forall i . Term (HashIndirect HGit) i -> Term (HashIndirect HGit) i -> Bool
eqStructural' (Term (Pair p1 (HC (Compose me1)))) (Term (Pair p2 (HC (Compose me2)))) =
  p1 == p2 && case (me1, me2) of
    (Just (Blob b1), Just (Blob b2)) -> b1 == b2
    (Just NullCommit, Just NullCommit) -> True
    (Just (Commit msg1 root1 parents1), Just (Commit msg2 root2 parents2)) ->
      msg1 == msg2
        && eqStructural' root1 root2
        && ( Prelude.foldl (&&) True $ NEL.zipWith eqStructural' parents1 parents2
           )
    (Just (Dir xs1), Just (Dir xs2)) -> Prelude.foldl (&&) True $ Prelude.zipWith eqFTE xs1 xs2
    (Nothing, Nothing) -> True
    (_, _) -> False

  where
    eqFTE (n1, DirEntity d1) (n2, DirEntity d2) = n1 == n2 && eqStructural' d1 d2
    eqFTE (n1, FileEntity f1) (n2, FileEntity f2) = n1 == n2 && eqStructural' f1 f2
    eqFTE _ _ = False



nullCommitHash :: Const HashPointer 'CommitTag
nullCommitHash = Const $ mkHashPointer 0

emptyDirHash :: Const HashPointer 'DirTag
emptyDirHash = Const $ mkHashPointer 0

newtype HashIndirectTerm i
  = HashIndirectTerm
  { unHashIndirectTerm :: Term (HashIndirect HGit) i
  }

instance Eq (HashIndirectTerm i) where
  (HashIndirectTerm a) == (HashIndirectTerm b) = eqStructural' a b

instance SingI i => Show (HashIndirectTerm i) where
  show = show . encode -- lazy (not in the good way), mostly just for use in tests

-- NOTE: this really needs round trip properties for surety.. which means generators..
instance SingI i => FromJSON (HashIndirectTerm i) where
    parseJSON = fmap HashIndirectTerm . anaM alg . Const
      where
        -- parser (Result) is a monad, so we can just run in that
        alg :: CoalgM Parser (HashIndirect HGit) (Const Value)
        alg x = flip (withObject "pointer tagged entity") (getConst x) $ \o -> do
          p <- o .:  "pointer"
          (mentity :: Maybe Value) <- o .:! "entity" -- entity present but null != entity not present
          case mentity of
            Nothing -> pure $ Pair p $ HC $ Compose Nothing
            Just entity -> handle p entity

        handle :: forall i'
                . SingI i'
               => Const HashPointer i'
               -> Value
               -> Parser $ (HashIndirect HGit) (Const Value) i'
        handle p v = case sing @i' of
          SDirTag -> handleDirTag p v -- todo inline
          SCommitTag -> case v of
            Null -> pure $ Pair p $ HC $ Compose $ Just $ NullCommit
            x    -> flip (withObject "commit") x $ \o -> do
              name <- o .: "msg"
              root <- o .: "root"
              parents <- o .: "parents"
              pure $ Pair p $ HC $ Compose $ Just $ Commit name (Const root) (fmap Const parents)


          SBlobTag -> case v of
            (String t) -> pure $ Pair p $ HC $ Compose $ Just $ Blob $ unpack t
            _          -> fail "expected string"


        handleDirTag
          :: Const HashPointer 'DirTag
          -> Value -> Parser $ (HashIndirect HGit) (Const Value) 'DirTag
        handleDirTag p = withArray "dir entries" $ \a -> do
          (elems :: [NamedFileTreeEntity (Const Value)]) <- traverse mkElem $ toList a
          let res :: (HashIndirect HGit) (Const Value) 'DirTag
              res = Pair p $ HC $ Compose $ Just $ Dir elems
          pure $ res

        mkElem :: Value -> Parser $ NamedFileTreeEntity (Const Value)
        mkElem v = decodeNamedDir handleDir handleFile v
        handleFile o = do
          file <- o .: "file"
          pure $ Const file
        handleDir o = do
          dir <- o .: "dir"
          pure $ Const dir

instance SingI i => ToJSON (HashIndirectTerm i) where
    toJSON = getConst . cata alg . unHashIndirectTerm
      where
        alg :: Alg (HashIndirect HGit) (Const Value)
        alg (Pair (Const p) (HC (Compose Nothing))) = Const $ object ["pointer" .= unHashPointer p]
        alg (Pair (Const p) (HC (Compose (Just x)))) = Const $
          object [ "pointer" .= unHashPointer p
                 , "entity" .= encodeEntity  x
                 ]

        encodeEntity :: HGit (Const Value) :=> Value
        encodeEntity (Dir xs) =
          Array $ fromList $ fmap (encodeNamedDir (pure . ("file" .= )) (pure . ("dir" .= ))) xs

        encodeEntity (NullCommit) = Null
        encodeEntity (Commit msg root parents) =
          object [ "msg" .= msg
                 , "root" .= getConst root
                 , "parents" .= fmap getConst parents
                 ]

        encodeEntity (Blob fc) = String $ pack fc
