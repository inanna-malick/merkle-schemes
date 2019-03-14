{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module HGit.Serialization where

--------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Functor.Compose
import           Data.Functor.Const
import qualified Data.List.NonEmpty as NEL
import           Data.Singletons
import           Data.Text
import           Data.Vector (fromList, toList)
--------------------------------------------
import           HGit.Types.HGit
import           Merkle.Functors
import           Merkle.Types
import           Util.HRecursionSchemes
import           Util.MyCompose
--------------------------------------------

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

-- shit name, investigate Eq1 and such
newtype HashTaggedNT i = HashTaggedNT (Term (Tagged Hash :++ HGit) i)
instance SingI i => Show (HashTaggedNT i) where
  show (HashTaggedNT x) = show $ HashTaggedIndirectTerm $ makeIndirect x
instance SingI i => Eq (HashTaggedNT i) where
  (HashTaggedNT a) == (HashTaggedNT b) = eqStructural a b

-- | Compare two hash indirect structures, returning True only if both are exactly equal
--   (eg not just equal via pointer comparison, all substantiations and list orderings must be identical)
eqStructural :: forall i . Term (Tagged Hash :++ HGit) i -> Term (Tagged Hash :++ HGit) i -> Bool
eqStructural (Term (HC (Tagged p1 (me1)))) (Term (HC (Tagged p2 (me2)))) =
  p1 == p2 && case (me1, me2) of
    ((Blob b1), (Blob b2)) -> b1 == b2
    (NullCommit, NullCommit) -> True
    ((Commit msg1 root1 parents1), (Commit msg2 root2 parents2)) ->
      msg1 == msg2
        && eqStructural root1 root2
        && ( Prelude.foldl (&&) True $ NEL.zipWith eqStructural parents1 parents2
           )
    ((Dir xs1), (Dir xs2)) -> Prelude.foldl (&&) True
                            $ Prelude.zipWith eqFTE (canonicalOrdering xs1)
                                                    (canonicalOrdering xs2)
    (_, _) -> False

  where
    eqFTE (n1, DirEntity d1) (n2, DirEntity d2) = n1 == n2 && eqStructural d1 d2
    eqFTE (n1, FileEntity f1) (n2, FileEntity f2) = n1 == n2 && eqStructural f1 f2
    eqFTE _ _ = False




-- | Compare two hash indirect structures, returning True only if both are exactly equal
--   (eg not just equal via pointer comparison, all substantiations and list orderings must be identical)
eqStructural' :: forall i . Term (Tagged Hash :++ Indirect :++ HGit) i -> Term (Tagged Hash :++ Indirect :++ HGit) i -> Bool
eqStructural' (Term (HC (Tagged p1 (HC (Compose me1))))) (Term (HC (Tagged p2 (HC (Compose me2))))) =
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


newtype HashTaggedIndirectTerm i
  = HashTaggedIndirectTerm
  { unHashTaggedIndirectTerm :: Term (Tagged Hash :++ Indirect :++ HGit) i
  }

instance Eq (HashTaggedIndirectTerm i) where
  (HashTaggedIndirectTerm a) == (HashTaggedIndirectTerm b) = eqStructural' a b

instance SingI i => Show (HashTaggedIndirectTerm i) where
  show = show . encode -- lazy (not in the good way), mostly just for use in tests

-- NOTE: this really needs round trip properties for surety.. which means generators..
instance SingI i => FromJSON (HashTaggedIndirectTerm i) where
    parseJSON = fmap HashTaggedIndirectTerm . anaM alg . Const
      where
        -- parser (Result) is a monad, so we can just run in that
        alg :: CoalgM Parser (Tagged Hash :++ Indirect :++ HGit) (Const Value)
        alg x = flip (withObject "pointer tagged entity") (getConst x) $ \o -> do
          p <- o .:  "pointer"
          (mentity :: Maybe Value) <- o .:! "entity" -- entity present but null != entity not present
          case mentity of
            Nothing -> pure . HC . Tagged p . HC $ Compose Nothing
            Just entity -> handle p entity

        handle :: forall i'
                . SingI i'
               => Hash i'
               -> Value
               -> Parser $ (Tagged Hash :++ Indirect :++ HGit) (Const Value) i'
        handle p v = case sing @i' of
          SDirTag -> handleDirTag p v -- todo inline
          SCommitTag -> case v of
            Null -> pure . HC . Tagged p . HC . Compose $ Just NullCommit
            x    -> flip (withObject "commit") x $ \o -> do
              name <- o .: "msg"
              root <- o .: "root"
              parents <- o .: "parents"
              pure . HC . Tagged p . HC . Compose . Just $ Commit name (Const root) (fmap Const parents)


          SBlobTag -> case v of
            (String t) -> pure . HC . Tagged p $ HC $ Compose $ Just $ Blob $ unpack t
            _          -> fail "expected string"


        handleDirTag
          :: Hash 'DirTag
          -> Value -> Parser $ (Tagged Hash :++ Indirect :++ HGit) (Const Value) 'DirTag
        handleDirTag p = withArray "dir entries" $ \a -> do
          (elems :: [NamedFileTreeEntity (Const Value)]) <- traverse mkElem $ toList a
          let res :: (Tagged Hash :++ Indirect :++ HGit) (Const Value) 'DirTag
              res = HC . Tagged p . HC $ Compose $ Just $ Dir elems
          pure $ res

        mkElem :: Value -> Parser $ NamedFileTreeEntity (Const Value)
        mkElem v = decodeNamedDir handleDir handleFile v
        handleFile o = do
          file <- o .: "file"
          pure $ Const file
        handleDir o = do
          dir <- o .: "dir"
          pure $ Const dir

instance SingI i => ToJSON (HashTaggedIndirectTerm i) where
    toJSON = getConst . cata alg . unHashTaggedIndirectTerm
      where
        alg :: Alg (Tagged Hash :++ Indirect :++ HGit) (Const Value)
        alg (HC (Tagged p (HC (Compose Nothing))))
          = Const $ object ["pointer" .= getConst p]
        alg (HC (Tagged p (HC (Compose (Just x))))) = Const $
          object [ "pointer" .= getConst p
                 , "entity"  .= encodeEntity  x
                 ]

        encodeEntity :: HGit (Const Value) :=> Value
        encodeEntity (Dir xs) =
          Array $ fromList $ fmap (encodeNamedDir (pure . ("file" .= )) (pure . ("dir" .= ))) xs

        encodeEntity (NullCommit) = Null
        encodeEntity (Commit msg root parents) =
          object [ "msg"     .= msg
                 , "root"    .= getConst root
                 , "parents" .= fmap getConst parents
                 ]

        -- todo: base64?
        encodeEntity (Blob fc) = String $ pack fc
