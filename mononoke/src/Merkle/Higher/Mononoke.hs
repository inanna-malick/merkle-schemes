{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Merkle.Higher.Mononoke where

--------------------------------------------
import           Data.Aeson as AE
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Base64 as Base64
import           Data.Functor.Compose
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           Data.Singletons.TH
import           GHC.Generics
--------------------------------------------
import           Merkle.Higher.Types
import           Merkle.Higher.Store
import           Merkle.Higher.Store.Deref
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------

$(singletons [d|
  data MTag = SnapshotT | FileTree | CommitT | Change | BlobT
 |])

-- TODO: hash type which allows for inline? maybe.
-- TODO: use case is commit comments, could point to a blob or have inline

type Path = Text

data Change a
  = Add [Path] (a (M a BlobT))
  | Del [Path]

instance ToJSON Change where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Change


data M a i where
  -- snapshots:
  Snapshot
    :: a 'FileTree -- snapshot of file tree in commit
    -> a 'CommitT -- originating commit
    -> M a 'SnapshotT


  -- file tree entries:
  File
    :: a 'Blob     -- file blob
    -> a 'CommitT  -- last modified in this commit
    -> a 'FileTree -- previous incarnation
    -> M a 'FileTree

  Dir
    :: [(Path, a 'FileTree)] -- children, todo nonempty
    -> a 'CommitT -- last modified in this commit
    -> a 'FileTree -- previous incarnation
    -> M a 'FileTree

  -- commits:
  NullCommit
    :: M a 'CommitT

  Commit
    :: Text        -- commit message
    -> [a 'Change] -- list of inline changes
    -> [a 'CommitT] -- parent commits, todo nonempty
    -> M a 'CommitT

  -- changes:
  Add
    :: NonEmpty Path -- full path of entity being added
    -> a 'BlobT
    -> M a 'Change

  Del
    :: NonEmpty Path -- full path of entity being added
    -> M a 'Change

  -- blobs:
  Blob
    :: ByteString
    -> M a 'BlobT


instance ExtractKeys M where
  extractHashKeys (Blob) = []


-- maxChunkSize :: Int
-- maxChunkSize = 1024

-- -- NOTE: needs actual tests, but I tested it in the repl and it works
-- -- NOTE: probably because all the of by one errors balance eachother out
-- getChunks
--   :: ChunkRange
--   -> BitTorrent (Term (Tagged Hash `HCompose` Compose IO `HCompose` BitTorrent)) 'TorrentTag
--   -> IO ByteString
-- getChunks (ChunkRange start end) (Torrent _meta _pointers lazyChunks) = do
--     let firstChunk = start `div` maxChunkSize
--         lastChunk  = end `div` maxChunkSize
--     let firstChunkOffset = start `mod` maxChunkSize

--     chunks <- traverse deref (slice firstChunk lastChunk lazyChunks)

--     let megaChunk' = B.concat $ fmap (\(Chunk bs) -> bs) chunks
--         -- end - start should maybe be +1? like slice? but end is exclusive? lmao idk
--         megaChunk = B.take (end - start) $ B.drop firstChunkOffset megaChunk'

--     pure megaChunk

--   where
--     -- inefficient, etc etc #yolo (note: may have arithmetic errors all over the place here)
--     slice from' to' xs = take (to' - from' + 1) (drop from' xs)

-- mkTorrent
--   :: Text
--   -> [(FilePath, ByteString)] -- TODO: upload as it goes, needed for v. big data examples
--   -> Term BitTorrent 'TorrentTag
-- mkTorrent meta files = Term $ Torrent meta pointers' chunks''
--   where
--     splitMegaChunk bs
--       | B.length bs > 0 = [B.take maxChunkSize bs] ++ splitMegaChunk (B.drop maxChunkSize bs)
--       | otherwise = []

--     chunks'' = fmap (Term . Chunk) chunks'
--     chunks' = splitMegaChunk megachunk'
--     (megachunk', pointers', _lastPointer') = foldl f (B.empty, [], 0) files

--     f (megachunk, pointers, lastPointer) (fp, bs) =
--           let lastPointer' = lastPointer + B.length bs -- for next chunk
--               pointer = ChunkRange
--                       { crStart = lastPointer               -- inclusive
--                       , crEnd   = lastPointer + B.length bs -- exclusive
--                       }
--            in (B.append megachunk bs, pointers ++ [(fp, pointer)], lastPointer')

-- -- haha holy crap it works (in repl tests with multiple chunks, todo is to write properties!)
-- mkTorrentLazy
--   :: Monad m
--   => Store m BitTorrent
--   -> Text
--   -> [m (FilePath, ByteString)] -- allows for lazy file read
--   -> m (BitTorrent Hash 'TorrentTag)
-- mkTorrentLazy store meta files = do
--     -- TODO: idk, record?
--     (lastChunk, chunkPointers, pointers', _lastPointer')
--       <- foldl f (pure (B.empty, [], [], 0)) files

--     -- TODO: assert, last chunk should be <= max size
--     lastChunkPointer <- sPut store $ Chunk lastChunk
--     pure $ Torrent meta pointers' $ chunkPointers ++ [lastChunkPointer]
--   where
--     -- need to upload bit by bit, one file may be multiple chunks
--     splitMegaChunk bs
--       | B.length bs >= maxChunkSize = do
--           let chunk = B.take maxChunkSize bs
--           pointer <- sPut store $ Chunk chunk
--           (remainder, pointers) <- splitMegaChunk (B.drop maxChunkSize bs)
--           pure (remainder, pointer : pointers)
--       | otherwise = pure (bs, [])

--     f acc eff = do
--           (megachunk, chunkPointers, pointers, lastPointer) <- acc
--           (fp, bs) <- eff
--           let lastPointer' = lastPointer + B.length bs -- for next chunk
--               pointer = ChunkRange
--                       { crStart = lastPointer               -- inclusive
--                       , crEnd   = lastPointer + B.length bs -- exclusive
--                       }
--           (megachunk', chunkPointers') <- splitMegaChunk $ B.append megachunk bs
--           pure ( megachunk'
--                , chunkPointers ++ chunkPointers'
--                , pointers ++ [(fp, pointer)]
--                , lastPointer'
--                )

-- exampleRelease :: Term BitTorrent 'ReleaseTag
-- exampleRelease
--   = Term $ Release "test release"
--                    [("torrent 1", Left exampleTorrent1), ("torrent 2", Left exampleTorrent2)]

-- exampleTorrent1 :: Term BitTorrent 'TorrentTag
-- exampleTorrent1 = mkTorrent "test torrent 1"
--             [ ("foo/bar.md", "file contents 1")
--             , ("foo.md",     "file contents 2")
--             , ("baz.md",     "file contents 1")
--             ]

-- exampleTorrent2 :: Term BitTorrent 'TorrentTag
-- exampleTorrent2 = mkTorrent "test torrent 2"
--             [ ("warez.jk", "deadbeef") ]

-- instance (Eq (a 'ChunkTag), Eq (a 'TorrentTag), Eq (a 'ReleaseTag)) => Eq (BitTorrent a i) where
--   Chunk b == Chunk b' = b == b'
--   Torrent s ps as == Torrent s' ps' as' = s == s' && as == as' && ps == ps'
--   Release s as == Release s' as' = s == s' && as == as'


-- instance HFunctor BitTorrent where
--   hfmap _ (Chunk fc)        = Chunk fc
--   hfmap f (Torrent md ps chunks) = Torrent md ps (fmap f chunks)
--   hfmap f (Release md torrents) = Release md (fmap (fmap (either (Left . f) (Right . f))) torrents)

-- -- half-impl'd defn
-- instance HTraversable BitTorrent where
--   hmapM _ (Chunk fc) = pure $ Chunk fc
--   hmapM nat (Torrent md ps chunks) = do
--     chunks' <- traverse nat chunks
--     pure $ Torrent md ps chunks'
--   hmapM nat (Release md torrents) = do
--     torrents' <- traverse (traverse (either (fmap Left . nat) (fmap Right . nat))) torrents
--     pure $ Release md torrents'

-- instance SingI i => FromJSON (BitTorrent Hash i) where
--     parseJSON x = case (sing :: Sing i) of
--           SChunkTag -> flip (withObject "chunk") x $ \o -> do
--               c <- o .: "chunk"
--               case Base64.decode (encodeUtf8 c) of
--                 Left err -> fail err
--                 Right bs -> pure $ Chunk bs

--           STorrentTag -> flip (withObject "torrent") x $ \o -> do
--               m <- o .: "metadata"
--               pointers <- o .: "pointers"
--               chunks <- o .: "chunks"
--               pure $ Torrent m pointers chunks

--           SReleaseTag -> flip (withObject "release") x $ \o -> do
--               m <- o .: "metadata"
--               torrents <- o .: "torrents" -- new name - torrents + releases.. contents?
--               pure $ Release m torrents


-- instance SingI i => ToJSON (BitTorrent Hash i) where
--     toJSON (Chunk c)
--       = object [ "chunk" .= decodeLatin1 (Base64.encode c)
--                ]
--     toJSON (Torrent md pointers chunks)
--       = object [ "metadata" .= md
--                , "pointers" .= pointers
--                , "chunks"   .= chunks
--                ]
--     toJSON (Release md torrents)
--       = object [ "metadata" .= md
--                , "torrents" .= torrents
--                ]
