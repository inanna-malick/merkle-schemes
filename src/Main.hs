module Main where

--------------------------------------------
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
--------------------------------------------
import           Commands
import           Compare (compareMerkleTrees)
import           Deref (strictDeref)
import           FileIO (writeTree, readTree)
import           Merkle.Types
import           Merkle.Tree.Types
import           Util.MyCompose
import           Util.Util (mapErrUtil)
import           Util.RecursionSchemes
import           Store
--------------------------------------------

main :: IO ()
main = run =<< parse

run :: MerkleDiffOpts -> IO ()
run (MerkleDiffOpts storeDir (Diff before after)) = do
  let store = fsStore storeDir
  res <- runExceptT $ compareMerkleTrees store before after
  print $ fmap fst res
run (MerkleDiffOpts storeDir (Get p mfp)) = do
  let store = fsStore storeDir
  fp <- maybe (createTmpDir "merkle_get") pure mfp
  _res <- runExceptT $ strictlyDerefAndWrite store fp p
  putStrLn "done getting!"
  --print res
run (MerkleDiffOpts storeDir (Put fp)) = do
  let store = fsStore storeDir
  _res <- runExceptT $ strictlyReadAndUpload store fp
  putStrLn "done putting!"
  --print res
run (MerkleDiffOpts storeDir Demo) = do -- run the old main method used for testing
  res' <- runExceptT $ do
    let store = fsStore storeDir

    -- forget structure of merkle trees and retain only a pointer to the top level
    let forgetStructure = pointer

    -- read some merkle trees into memory (and into the store) and then forget all but the top pointer
    before <- mapErrUtil show $ forgetStructure <$> strictlyReadAndUpload store "examples/before/node1"
    after1 <- mapErrUtil show $ forgetStructure <$> strictlyReadAndUpload store "examples/after1/node1"
    after2 <- mapErrUtil show $ forgetStructure <$> strictlyReadAndUpload store "examples/after2/node1"
    after3 <- mapErrUtil show $ forgetStructure <$> strictlyReadAndUpload store "examples/after3/node2"

    mapErrUtil show $ do
      let s (a,b) = (cata s' a, cata s' b)
          -- todo pretty printer here
          s' (C (p, C Nothing)) = show (unPointer p) ++ ":unexpanded"
          s' (C (p, C (Just (C (n, t))))) = show (unPointer p) ++ ":(" ++ n ++"):" ++ show t
      liftIO $ putStrLn "comparing before to after1"
      compareMerkleTrees store before after1 >>= liftIO . print . fmap s

      liftIO $ putStrLn "comparing before to after2"
      compareMerkleTrees store before after2 >>= liftIO . print . fmap s

      liftIO $ putStrLn "comparing before to after3"
      compareMerkleTrees store before after3 >>= liftIO . print . fmap s

    mapErrUtil show $ liftIO (createTmpDir "output-example") >>= flip (strictlyDerefAndWrite store) after3

  print res'

-- | Write tree to file path (using pointer)
strictlyDerefAndWrite
  :: MonadIO m
  => Store m
  -> FilePath
  -> Pointer
  -> m ()
strictlyDerefAndWrite store outdir p = do
  derefed <- cata (Fix . snd . getCompose) -- strip hash annotations
         <$> strictDeref store p
  writeTree outdir derefed

-- | Read tree from the file system and upload to a store
strictlyReadAndUpload
  :: MonadIO m
  => Store m
  -> FilePath
  -> m StrictMerkleTree
strictlyReadAndUpload store dir = do
  let lazyTree = readTree dir
  strictTree <- cata (\(C effect) -> effect >>= traverse id >>= pure . Fix) lazyTree
  addTreeToStore store strictTree
