{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Merkle.Higher.BitTorrent where

--------------------------------------------
import           Control.Applicative (Const(..))
import           Data.Aeson as AE
import           Data.ByteString (ByteString)
import           Data.ByteString.Base64 as Base64
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           Data.Singletons.TH
--------------------------------------------
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------

$(singletons [d|
  data TorrentTag = ReleaseTag | TorrentTag | ChunkTag
 |])


data BitTorrent a i where
  -- Release, eg some set of torrents representing different versions of some quote linux distro unquote
  Release :: String -- release-level metadata
          -> [a 'TorrentTag] -- torrents
          -> BitTorrent a 'ReleaseTag

  -- simple representation for dev work. Real BT uses pointers into a list of constant-size chunks
  Torrent :: String -- description of torrent contents, ascii art, etc
          -> [(FilePath, [a 'ChunkTag])] -- files, each being some number of chunks
          -> BitTorrent a 'TorrentTag

  -- chunk of bytes
  Chunk :: ByteString -> BitTorrent a 'ChunkTag


exampleRelease :: Term BitTorrent 'ReleaseTag
exampleRelease
  = Term $ Release "test release"
                   [exampleTorrent1, exampleTorrent2]


exampleTorrent1 :: Term BitTorrent 'TorrentTag
exampleTorrent1
  = Term $ Torrent "test torrent 1"
            [ ("foo/bar.md", [ Term $ Chunk "file contents 1a"
                             , Term $ Chunk "file contents 1b"
                             ]
              )
            , ("foo.md", [Term $ Chunk "file contents 2"])
            , ("baz.md", [Term $ Chunk "file contents 1"])
            ]


exampleTorrent2 :: Term BitTorrent 'TorrentTag
exampleTorrent2
  = Term $ Torrent "test torrent 2"
            [ ("warez.jk", [Term $ Chunk "deadbeef"])
            ]


instance HFunctor BitTorrent where
  hfmap _ (Chunk fc)        = Chunk fc
  hfmap f (Torrent md chunks) = Torrent md (fmap (fmap (fmap f)) chunks)
  hfmap f (Release md torrents) = Release md (fmap f torrents)

-- half-impl'd defn
instance HTraversable BitTorrent where
  hmapM _ (Chunk fc) = pure $ Chunk fc
  hmapM nat (Torrent md chunks) = do
    chunks' <- traverse (traverse (traverse nat)) chunks
    pure $ Torrent md chunks'
  hmapM nat (Release md torrents) = do
    torrents' <- traverse nat torrents
    pure $ Release md torrents'



instance (SingI i, FromJSON x) => FromJSON (BitTorrent (Const x) i) where
    parseJSON x = case (sing :: Sing i) of
          SChunkTag -> flip (withObject "chunk") x $ \o -> do
              c <- o .: "chunk"
              case Base64.decode (encodeUtf8 c) of
                Left err -> fail err
                Right bs -> pure $ Chunk bs

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
      = object ["chunk" .= decodeLatin1 (Base64.encode c)]
    toJSON (Torrent md chunks)
      = object ["metadata" .= md
               , "chunks"  .= chunks
               ]
    toJSON (Release md torrents)
      = object [ "metadata" .= md
               , "torrents" .= torrents
               ]
