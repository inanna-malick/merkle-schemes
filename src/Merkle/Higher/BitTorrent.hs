{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Merkle.Higher.BitTorrent where

--------------------------------------------
import           Data.Aeson as AE
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Base64 as Base64
import           Data.List.NonEmpty as NEL
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           Data.Singletons.TH
import           GHC.Generics
--------------------------------------------
import           Merkle.Higher.Types
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------

$(singletons [d|
  data TorrentTag = ReleaseTag | TorrentTag | ChunkTag
 |])

data Position = Position { pOffset :: Int, pIdx :: Int }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Position where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Position

data ChunkRange = ChunkRange { crFrom :: Position, crTo :: Position }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ChunkRange where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ChunkRange

data BitTorrent a i where
  -- Release, eg some set of torrents representing different versions of some quote linux distro unquote
  Release :: String -- release-level metadata
          -> [a 'TorrentTag] -- torrents
          -> BitTorrent a 'ReleaseTag

  -- torrent, files are pointers into lists of chunks (consistient size)
  Torrent :: String -- description of torrent contents, ascii art, etc
          -> [(FilePath, ChunkRange)] -- pointers into chunk list
          -> NonEmpty (a 'ChunkTag)   -- list of pointers to chunks
          -> BitTorrent a 'TorrentTag

  -- chunk of bytes
  Chunk :: ByteString -> BitTorrent a 'ChunkTag


-- todo: test a bunch, I guess?
mkTorrent
  :: String
  -> [(FilePath, ByteString)]
  -> Term BitTorrent 'TorrentTag
mkTorrent meta files = Term $ Torrent meta pointers' chunks'' 
  where
    maxChunkSize = 1024

    chunks'' = fmap (Term . Chunk) chunks'
    (chunks', pointers') = foldl f (("" :| []), []) files

    f (chunks, pointers) (fp, bs)
        -- no space in last chunk
        | B.length (NEL.last chunks) >= maxChunkSize = _makeNewChunks
        -- can fit entirely in last chunk
        | B.length (NEL.last chunks) + B.length bs <= maxChunkSize =
          let newChunkIdx = NEL.length - 1
              newChunk = B.append (NEL.last chunks) bs
              newChunks = newChunk :| (NEL.tail $ NEL.reverse chunks)
              pointer = ChunkRange
                      { crFrom =
                      , crTo 
                      }
           in (newChunks, pointers ++ 

        -- must be added to some combination of last chunk and this one
        | otherwise = _appendAndMakeNewChunks


exampleRelease :: Term BitTorrent 'ReleaseTag
exampleRelease
  = Term $ Release "test release"
                   [exampleTorrent1, exampleTorrent2]

exampleTorrent1 :: Term BitTorrent 'TorrentTag
exampleTorrent1 = mkTorrent "test torrent 1"
            [ ("foo/bar.md", "file contents 1")
            , ("foo.md",     "file contents 2")
            , ("baz.md",     "file contents 1")
            ]

exampleTorrent2 :: Term BitTorrent 'TorrentTag
exampleTorrent2 = mkTorrent "test torrent 2"
            [ ("warez.jk", "deadbeef") ]

instance (Eq (a 'ChunkTag), Eq (a 'TorrentTag), Eq (a 'ReleaseTag)) => Eq (BitTorrent a i) where
  Chunk b == Chunk b' = b == b'
  Release s as == Release s' as' = s == s' && as == as'
  Torrent s ps as == Torrent s' ps' as' = s == s' && as == as' && ps == ps'


instance HFunctor BitTorrent where
  hfmap _ (Chunk fc)        = Chunk fc
  hfmap f (Torrent md ps chunks) = Torrent md ps (fmap f chunks)
  hfmap f (Release md torrents) = Release md (fmap f torrents)

-- half-impl'd defn
instance HTraversable BitTorrent where
  hmapM _ (Chunk fc) = pure $ Chunk fc
  hmapM nat (Torrent md ps chunks) = do
    chunks' <- traverse nat chunks
    pure $ Torrent md ps chunks'
  hmapM nat (Release md torrents) = do
    torrents' <- traverse nat torrents
    pure $ Release md torrents'

instance SingI i => FromJSON (BitTorrent Hash i) where
    parseJSON x = case (sing :: Sing i) of
          SChunkTag -> flip (withObject "chunk") x $ \o -> do
              c <- o .: "chunk"
              case Base64.decode (encodeUtf8 c) of
                Left err -> fail err
                Right bs -> pure $ Chunk bs

          STorrentTag -> flip (withObject "torrent") x $ \o -> do
              m <- o .: "metadata"
              pointers <- o .: "pointers"
              chunks <- o .: "chunks"
              pure $ Torrent m pointers chunks

          SReleaseTag -> flip (withObject "release") x $ \o -> do
              m <- o .: "metadata"
              torrents <- o .: "torrents"
              pure $ Release m torrents



instance SingI i => ToJSON (BitTorrent Hash i) where
    toJSON (Chunk c)
      = object ["chunk" .= decodeLatin1 (Base64.encode c)]
    toJSON (Torrent md pointers chunks)
      = object [ "metadata" .= md
               , "pointers" .= pointers
               , "chunks"   .= chunks
               ]
    toJSON (Release md torrents)
      = object [ "metadata" .= md
               , "torrents" .= torrents
               ]
