-- | For use with demos, writes all files to some local dir instead of to IPFS daemon
module Merkle.Store.MockIPFS where

-- --------------------------------------------
import           Control.Applicative (Const(..))
import           Control.Exception.Safe
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T
import           System.Directory (doesFileExist)
-- --------------------------------------------
import           Merkle.Store
import           Merkle.Types
import           Merkle.Types.IPFS
import           Merkle.Types.BlakeHash
-----------------------------------------------


mockIpfsStore
  :: ( FromJSON (f (Hash RawIPFSHash f))
     , ToJSON   (f (Hash RawIPFSHash f))
     , ExtractKeys f -- for linking obj graph together (weak foldable, kinda)
     )
  => FilePath
  -> ShallowStore IO RawIPFSHash f
mockIpfsStore base
  = ( GetCapabilityShallow $ getForHash base
    , PutCapability        $ putForHash base
    )

toPath :: FilePath -> RawIPFSHash -> FilePath
toPath base (RawIPFSHash h) = base ++ "/" ++ T.unpack h ++ ".json"

-- | Use Multihash, should be close to IPFS hash format if not exactly ==
--   NOTE: no promise of exact equality exists here, may differ from IPFS hash
--   NOTE: embeds partial bytestring decode, should work
mkMockIPFSHash :: ToJSON x => x -> RawIPFSHash
mkMockIPFSHash x = RawIPFSHash $ hashToText $ doHash' [toStrict $ encode x]

getForHash
  :: FromJSON (f (Hash RawIPFSHash f))
  => FilePath
  -> Hash RawIPFSHash f
  -> IO (Maybe (f (Hash RawIPFSHash f)))
getForHash base (Const h) = do
    let fp = toPath base h
    putStrLn $ "get: " ++ show h  ++ " at " ++ fp

    exists <- liftIO $ doesFileExist fp
    if not exists
      then pure Nothing
      else liftIO $ eitherDecodeFileStrict fp >>= \case
          -- throw if deserialization fails
          Left  e -> throwString e
          Right (DagNode x _ls) -> pure $ Just x


putForHash
  :: ( ToJSON (f (Hash RawIPFSHash f))
     , ExtractKeys f
     )
  => FilePath
  -> f (Hash RawIPFSHash f)
  -> IO (Hash RawIPFSHash f)
putForHash base fhi = do
    let obj = DagNode fhi $ extractRawKeys fhi
        h = mkMockIPFSHash obj
        fp = toPath base h
    putStrLn $ "put at path" ++ fp
    print $ encode obj

    liftIO $ encodeFile fp obj

    pure $ Const h

