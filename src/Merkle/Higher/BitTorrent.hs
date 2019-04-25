{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Merkle.Higher.BitTorrent where

--------------------------------------------
import           Data.Aeson
import           Data.Functor.Const
import           Data.ByteString (ByteString)
import           Data.Singletons.TH
import           GHC.Generics (Generic)
--------------------------------------------
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------
import Data.ByteString.Base64.Type


$(singletons [d|
  data TorrentTag = ReleaseTag | MetaDataTag | TorrentTag | ChunkTag
 |])

type FileName = FilePath
type ChunkOffset = Int
type ChunkIdx    = Int

data Position = Position ChunkOffset ChunkIdx
  deriving (Generic)

instance ToJSON Position where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Position


-- constant-size chunk of bytes (last chunk can be between 1 and that many bytes)
type Chunk = ByteString

data ChunkRange = ChunkRange { from :: Position, to :: Position }
  deriving (Generic)

instance ToJSON ChunkRange where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ChunkRange

data BitTorrent a i where
  -- Release, eg some set of torrents representing different versions of some quote linux distro unquote
  Release :: a 'MetaDataTag -- release-level metadata
          -> [a 'TorrentTag] -- torrents
          -> BitTorrent a 'ReleaseTag

  -- Unstructured metadata. Can be used to provide anything from
  -- ASCII art commemorating the people who uploaded some file to comments on
  -- the video or audio quality provided by a quote linux distro unquote to
  -- a top-level description of a release
  MetaData :: String -- String because these are unicode only
           -> BitTorrent a 'MetaDataTag

  -- list of chunks + filename-chunk mappings (allows for invalid states re same, can be validated)
  -- Torrent' :: a 'MetaDataTag -- description of torrent contents, ascii art, etc
  --          -> [(FileName, ChunkRange)] -- pointers into chunk list
  --          -> [a 'ChunkTag] -- list of pointers to chunks
  --          -> BitTorrent a 'TorrentTag

  -- simple representation for dev work
  Torrent :: a 'MetaDataTag -- description of torrent contents, ascii art, etc
          -> [(FileName, a 'ChunkTag)] -- files
          -> BitTorrent a 'TorrentTag

  -- constant-size chunk (except for last chunk, which is [1, max] bytes)
  Chunk :: Chunk -> BitTorrent a 'ChunkTag


instance HFunctor BitTorrent where
  hfmap _ (Chunk fc)        = Chunk fc
  hfmap _ (MetaData fc)        = MetaData fc
  hfmap f (Torrent md chunks) = Torrent (f md) (fmap (fmap f) chunks)
  hfmap f (Release md torrents) = Release (f md) (fmap f torrents)

-- half-impl'd defn
instance HTraversable BitTorrent where
  hmapM _ (Chunk fc) = pure $ Chunk fc
  hmapM _ (MetaData fc) = pure $ MetaData fc
  hmapM nat (Torrent md chunks) = do
    md' <- nat md
    chunks' <- traverse (traverse nat) chunks
    pure $ Torrent md' chunks'
  hmapM nat (Release md torrents) = do
    md' <- nat md
    torrents' <- traverse nat torrents
    pure $ Release md' torrents'

instance (SingI i, FromJSON x) => FromJSON (BitTorrent (Const x) i) where
    -- parseJSON :: forall i. Sing i => Value -> Parser (BitTorrent Hash i)
    parseJSON x = case sing @i of
          SChunkTag -> flip (withObject "chunk") x $ \o -> do
              c <- o .: "chunk"
              pure $ Chunk $ getByteString64 c

          SMetaDataTag -> flip (withObject "metadata") x $ \o -> do
              m <- o .: "metadata"
              pure $ MetaData m

          STorrentTag -> flip (withObject "torrent") x $ \o -> do
              m <- o .: "metadata"
              chunks <- o .: "chunks"
              pure $ Torrent m chunks

          SReleaseTag -> flip (withObject "release") x $ \o -> do
              m <- o .: "metadata"
              torrents <- o .: "torrents"
              pure $ Release m torrents



instance (SingI i, ToJSON x) => ToJSON (BitTorrent (Const x) i) where
    toJSON (Chunk c)
      = object ["chunk" .= makeByteString64 c] -- todo base64?
    toJSON (MetaData m)
      = object ["metadata" .= m]
    toJSON (Torrent md chunks)
      = object ["metadata" .= md
               , "chunks"  .= chunks
               ]
    toJSON (Release md torrents)
      = object [ "metadata" .= md
               , "torrents" .= torrents
               ]
