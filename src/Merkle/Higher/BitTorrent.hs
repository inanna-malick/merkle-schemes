{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Merkle.Higher.BitTorrent where

--------------------------------------------
import           Data.Aeson as AE
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Base64 as Base64
import           Data.Functor.Compose
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           Data.Singletons.TH
import           GHC.Generics
--------------------------------------------
import           Merkle.Higher.Types
import           Merkle.Higher.Store.Deref
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------

data ChunkRange = ChunkRange { crStart :: Int, crEnd :: Int }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ChunkRange where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ChunkRange


$(singletons [d|
  data TorrentTag = ReleaseTag | TorrentTag | ChunkTag
 |])

data BitTorrent a i where
  -- Release, eg some set of torrents representing different versions of some quote linux distro unquote
  Release :: String -- release-level metadata
          -> [a 'TorrentTag] -- torrents
          -> BitTorrent a 'ReleaseTag

  -- torrent, files are pointers into lists of chunks (consistient size)
  Torrent :: String -- description of torrent contents, ascii art, etc
          -> [(FilePath, ChunkRange)] -- pointers into chunk list
          -> [(a 'ChunkTag)]          -- list of pointers to chunks
          -> BitTorrent a 'TorrentTag

  -- chunk of bytes
  Chunk :: ByteString -> BitTorrent a 'ChunkTag


maxChunkSize :: Int
maxChunkSize = 1024


-- NOTE: needs actual tests, but I tested it in the repl and it works
-- NOTE: probably because all the of by one errors balance eachother out
getChunks
  :: ChunkRange
  -> BitTorrent (Term (Tagged Hash `HCompose` Compose IO `HCompose` BitTorrent)) 'TorrentTag
  -> IO ByteString
getChunks (ChunkRange start end) (Torrent _meta _pointers lazyChunks) = do
    let firstChunk = start `div` maxChunkSize
        lastChunk  = end `div` maxChunkSize
    let firstChunkOffset = start `mod` maxChunkSize

    chunks <- traverse deref (slice firstChunk lastChunk lazyChunks)

    let megaChunk' = B.concat $ fmap (\(Chunk bs) -> bs) chunks
        -- end - start should maybe be +1? like slice? but end is exclusive? lmao idk
        megaChunk = B.take (end - start) $ B.drop firstChunkOffset megaChunk'

    pure megaChunk

  where
    -- inefficient, etc etc #yolo (note: may have arithmetic errors all over the place here)
    slice from' to' xs = take (to' - from' + 1) (drop from' xs)


mkTorrent
  :: String
  -> [(FilePath, ByteString)] -- TODO: upload as it goes, needed for v. big data examples
  -> Term BitTorrent 'TorrentTag
mkTorrent meta files = Term $ Torrent meta pointers' chunks''
  where
    splitMegaChunk bs
      | B.length bs > 0 = [B.take maxChunkSize bs] ++ splitMegaChunk (B.drop maxChunkSize bs)
      | otherwise = []

    chunks'' = fmap (Term . Chunk) chunks'
    chunks' = splitMegaChunk megachunk'
    (megachunk', pointers', _lastPointer') = foldl f (B.empty, [], 0) files

    f (megachunk, pointers, lastPointer) (fp, bs) =
          let lastPointer' = lastPointer + B.length bs -- for next chunk
              pointer = ChunkRange
                      { crStart = lastPointer               -- inclusive
                      , crEnd   = lastPointer + B.length bs -- exclusive
                      }
           in (B.append megachunk bs, pointers ++ [(fp, pointer)], lastPointer')


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
